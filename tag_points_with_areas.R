# tag_points_with_areas.R — v3.4 (robust + normalized combo join + mapped-first combine + safe logging)
# - Tags saved places with City of Portland Neighborhood + Section (sextant)
# - Normalized, case-insensitive join to your combo CSV (no NA combo_label)
# - Combine step prefers rows that already have a mapped combo_label and prefers GeoJSON over KML
# - Writes per-list *_tagged.geojson + a combined ALL_combined_dedup_tagged.geojson
# - Any unmapped after combine are written to UNMAPPED_rows.csv (no console print error)

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(readr); library(stringr); library(stringi)
  library(purrr); library(tibble); library(tools)
})

# Pick the first column matching any regex pattern
pick_col <- function(nms, patterns) {
  for (p in patterns) {
    hit <- nms[grepl(p, nms, ignore.case = TRUE)]
    if (length(hit)) return(hit[1])
  }
  stop("Could not find a column matching: ", paste(patterns, collapse=" | "))
}

# Normalize names for joining (strip accents, trim, squash spaces, lowercase)
norm_name <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  tolower(trimws(gsub("\\s+", " ", x)))
}

# Ensure a point layer has core columns; create if missing and coerce types
ensure_core_cols <- function(g) {
  core <- c("title","desc","address","url","lon","lat")
  for (nm in core) if (!nm %in% names(g)) g[[nm]] <- NA_character_
  g$title   <- as.character(g$title)
  g$desc    <- as.character(g$desc)
  g$address <- as.character(g$address)
  g$url     <- as.character(g$url)
  g$lon     <- suppressWarnings(as.numeric(g$lon))
  g$lat     <- suppressWarnings(as.numeric(g$lat))
  g
}

tag_points_with_areas <- function(
    in_dir  = "out_lists_portland",
    out_dir = "out_lists_portland_tagged",
    neighborhoods_path = "data/Neighborhood_Boundaries.geojson",
    sextants_path      = "data/Portland_Administrative_Sextants.geojson",
    combo_map_csv      = "data/neighborhood_combo_map.csv"  # CSV with columns: neighborhood, combo_label
){
  stopifnot(dir.exists(in_dir))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- load polygons ----
  nb <- sf::st_read(neighborhoods_path, quiet = TRUE)
  sx <- sf::st_read(sextants_path,      quiet = TRUE)
  
  # normalize column names
  names(nb) <- tolower(names(nb))
  names(sx) <- tolower(names(sx))
  
  # guess attribute columns
  nb_name_col <- pick_col(names(nb), c("^name$", "neigh", "nbrhd", "neighbor"))
  sx_name_col <- pick_col(names(sx), c("sextant", "section", "^name$"))
  
  # CRS sanity (work in projected, then write out in WGS84)
  nb <- sf::st_make_valid(nb)
  sx <- sf::st_make_valid(sx)
  target_crs <- 3857
  nb <- sf::st_transform(nb, target_crs)
  sx <- sf::st_transform(sx, target_crs)
  
  files <- list.files(in_dir, pattern = "\\.(geojson|kml)$", full.names = TRUE, ignore.case = TRUE)
  stopifnot(length(files) > 0)
  
  # optional combo mapping
  have_combo <- !is.null(combo_map_csv) && file.exists(combo_map_csv)
  if (have_combo) {
    cm <- readr::read_csv(combo_map_csv, show_col_types = FALSE) %>%
      mutate(
        neighborhood = norm_name(.data$neighborhood),
        combo_label  = as.character(.data$combo_label)
      ) %>%
      select(nkey = neighborhood, combo_label)
  }
  
  for (f in files) {
    pts <- sf::st_read(f, quiet = TRUE) %>%
      ensure_core_cols() %>%
      sf::st_transform(target_crs)
    
    # join neighborhood (largest=TRUE disambiguates rare overlaps)
    pts_nb <- sf::st_join(pts, nb[, c(nb_name_col)], join = sf::st_within, left = TRUE, largest = TRUE)
    if (nb_name_col %in% names(pts_nb)) {
      names(pts_nb)[names(pts_nb) == nb_name_col] <- "neighborhood"
    } else if (!"neighborhood" %in% names(pts_nb)) {
      pts_nb$neighborhood <- NA_character_
    }
    
    # join sextant/section
    pts_sx <- sf::st_join(pts_nb, sx[, c(sx_name_col)], join = sf::st_within, left = TRUE, largest = TRUE)
    if (sx_name_col %in% names(pts_sx)) {
      names(pts_sx)[names(pts_sx) == sx_name_col] <- "section"
    } else if (!"section" %in% names(pts_sx)) {
      pts_sx$section <- NA_character_
    }
    
    # combo groups (normalized join)
    if (have_combo) {
      pts_sx <- pts_sx %>%
        mutate(nkey = norm_name(.data$neighborhood)) %>%
        left_join(cm, by = "nkey") %>%
        mutate(combo_label = dplyr::coalesce(.data$combo_label, .data$neighborhood)) %>%
        select(-nkey)
      
      matched <- sum(norm_name(pts_sx$neighborhood) %in% cm$nkey, na.rm = TRUE)
      message(sprintf("   • combo map matched %d of %d rows in %s",
                      matched, nrow(pts_sx), basename(f)))
    }
    
    # reorder/select core columns up front (keep all original too)
    geom_col   <- attr(pts_sx, "sf_column")
    keep_first <- intersect(c("title","desc","address","url","lon","lat","neighborhood","section","combo_label"), names(pts_sx))
    other_cols <- setdiff(names(pts_sx), c(keep_first, geom_col))
    pts_out    <- pts_sx[, c(keep_first, other_cols, geom_col)]
    
    # write to out_dir as GeoJSON (WGS84)
    pts_out <- sf::st_transform(pts_out, 4326)
    base    <- tools::file_path_sans_ext(basename(f))
    out_path <- file.path(out_dir, paste0(base, "_tagged.geojson"))
    sf::st_write(pts_out, out_path, delete_dsn = TRUE, quiet = TRUE)
    message("✓ tagged → ", basename(out_path),
            " (", sum(!is.na(pts_out$neighborhood)), " with neighborhood, ",
            sum(!is.na(pts_out$section)), " with section)")
  }
  
  # ---- combined + dedup master (prefer mapped rows, prefer GeoJSON over KML) ----
  tagged <- list.files(out_dir, pattern = "_tagged\\.geojson$", full.names = TRUE)
  
  read_one_tagged <- function(p) {
    g <- sf::st_read(p, quiet = TRUE)
    g <- ensure_core_cols(g)
    g$src <- basename(p)
    g
  }
  
  all_pts <- purrr::map_dfr(tagged, read_one_tagged)
  
  all_pts2 <- all_pts %>%
    dplyr::mutate(
      url_norm   = tolower(ifelse(is.na(.data$url), "", as.character(.data$url))),
      title_norm = tolower(ifelse(is.na(.data$title), "", as.character(.data$title))),
      lon_num    = suppressWarnings(as.numeric(.data$lon)),
      lat_num    = suppressWarnings(as.numeric(.data$lat)),
      key = dplyr::if_else(
        url_norm != "",
        url_norm,
        paste0(title_norm, "@", round(lon_num, 6), ",", round(lat_num, 6))
      ),
      is_mapped = !is.na(.data$combo_label) & .data$combo_label != .data$neighborhood,
      src_rank  = ifelse(grepl("\\.geojson$", .data$src, ignore.case = TRUE), 1L, 2L)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$is_mapped), .data$src_rank) %>%
    dplyr::distinct(.data$key, .keep_all = TRUE) %>%
    dplyr::select(-url_norm, -title_norm, -lon_num, -lat_num, -is_mapped, -src_rank)
  
  out_combined <- file.path(out_dir, "ALL_combined_dedup_tagged.geojson")
  sf::st_write(all_pts2, out_combined, delete_dsn = TRUE, quiet = TRUE)
  
  leftover <- sum(all_pts2$combo_label == all_pts2$neighborhood, na.rm = TRUE)
  message("✓ wrote combined: ", out_combined, "  (unmapped after combine = ", leftover, ")")
  
  if (leftover > 0) {
    bad <- all_pts2 %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(combo_label == neighborhood) %>%
      dplyr::select(title, neighborhood, section, src) %>%
      dplyr::arrange(neighborhood, title)
    
    bad_path <- file.path(out_dir, "UNMAPPED_rows.csv")
    readr::write_csv(bad, bad_path)
    message("• Unmapped rows kept after combine: ", nrow(bad),
            "\n  Wrote details to: ", bad_path)
  }
}

# helper to generate a skeleton combo CSV you can fill in once
write_combo_skeleton <- function(neighborhoods_path = "data/Neighborhood_Boundaries.geojson",
                                 out_csv = "data/neighborhood_combo_map.csv") {
  nb <- sf::st_read(neighborhoods_path, quiet = TRUE)
  names(nb) <- tolower(names(nb))
  nb_name_col <- pick_col(names(nb), c("^name$", "neigh", "nbrhd", "neighbor"))
  tibble(neighborhood = sort(unique(as.character(nb[[nb_name_col]]))),
         combo_label  = NA_character_) %>%
    readr::write_csv(out_csv)
  message("Wrote skeleton mapping CSV → ", out_csv,
          "\nFill 'combo_label' for any neighborhoods you want grouped (leave others blank).")
}
