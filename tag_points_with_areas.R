# tag_points_with_areas.R — v3.3 (robust combo join + auto-patch for Downtown)
# - Tags saved places with City of Portland Neighborhood + Section (sextant)
# - Normalized, case-insensitive join to your combo CSV (no NA combo_label)
# - Auto-adds "PORTLAND DOWNTOWN" → "Downtown" to the combo CSV if missing
# - Writes per-list *_tagged.geojson + a combined deduped ALL_combined_dedup_tagged.geojson

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
    combo_map_csv      = NULL  # optional CSV with columns: neighborhood, combo_label
){
  stopifnot(dir.exists(in_dir))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- load polygons
  nb <- sf::st_read(neighborhoods_path, quiet = TRUE)
  sx <- sf::st_read(sextants_path,      quiet = TRUE)
  
  # normalize column names
  names(nb) <- tolower(names(nb))
  names(sx) <- tolower(names(sx))
  
  # guess attribute columns
  nb_name_col <- pick_col(names(nb), c("^name$", "neigh", "nbrhd", "neighbor"))
  sx_name_col <- pick_col(names(sx), c("sextant", "section", "^name$"))
  
  # CRS sanity
  nb <- sf::st_make_valid(nb)
  sx <- sf::st_make_valid(sx)
  target_crs <- 3857
  nb <- sf::st_transform(nb, target_crs)
  sx <- sf::st_transform(sx, target_crs)
  
  files <- list.files(in_dir, pattern = "\\.(geojson|kml)$", full.names = TRUE, ignore.case = TRUE)
  stopifnot(length(files) > 0)
  
  for (f in files) {
    pts <- sf::st_read(f, quiet = TRUE)
    pts <- ensure_core_cols(pts)                 # ensure core attributes exist
    pts <- sf::st_transform(pts, target_crs)     # match polygon CRS
    
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
    
    # ---- apply your “combo” groups with normalized join + auto-patch ----
    if (!is.null(combo_map_csv) && file.exists(combo_map_csv)) {
      # Read raw CSV
      cm_raw <- readr::read_csv(combo_map_csv, show_col_types = FALSE) %>%
        mutate(
          neighborhood = as.character(.data$neighborhood),
          combo_label  = as.character(.data$combo_label)
        )
      # Auto-add PORTLAND DOWNTOWN → Downtown if missing
      need_row <- !any(norm_name(cm_raw$neighborhood) == norm_name("PORTLAND DOWNTOWN"))
      if (need_row) {
        cm_raw <- bind_rows(
          cm_raw,
          tibble(neighborhood = "PORTLAND DOWNTOWN", combo_label = "Downtown")
        ) %>%
          distinct(neighborhood, .keep_all = TRUE) %>%
          arrange(neighborhood)
        readr::write_csv(cm_raw, combo_map_csv)
        message("   • Added mapping to combo CSV: PORTLAND DOWNTOWN → Downtown")
      }
      # Build normalized lookup for join
      cm <- cm_raw %>%
        transmute(nkey = norm_name(.data$neighborhood),
                  combo_label = .data$combo_label)
      
      pts_sx <- pts_sx %>%
        mutate(nkey = norm_name(.data$neighborhood)) %>%
        left_join(cm, by = "nkey") %>%
        mutate(
          # never leave combo_label as NA — default to the neighborhood name
          combo_label = dplyr::coalesce(.data$combo_label, .data$neighborhood)
        ) %>%
        select(-nkey)
      
      # tiny summary (useful sanity check)
      matched <- sum(norm_name(pts_sx$neighborhood) %in% cm$nkey, na.rm = TRUE)
      message(sprintf("   • combo map matched %d of %d rows in %s",
                      matched, nrow(pts_sx), basename(f)))
    }
    
    # reorder/select core columns up front (keep all original too)
    geom_col <- attr(pts_sx, "sf_column")
    keep_first <- intersect(c("title","desc","address","url","lon","lat","neighborhood","section","combo_label"), names(pts_sx))
    other_cols <- setdiff(names(pts_sx), c(keep_first, geom_col))
    pts_out <- pts_sx[, c(keep_first, other_cols, geom_col)]
    
    # write to out_dir as GeoJSON (WGS84)
    pts_out <- sf::st_transform(pts_out, 4326)
    base <- tools::file_path_sans_ext(basename(f))
    out_path <- file.path(out_dir, paste0(base, "_tagged.geojson"))
    sf::st_write(pts_out, out_path, delete_dsn = TRUE, quiet = TRUE)
    message("✓ tagged → ", basename(out_path),
            " (", sum(!is.na(pts_out$neighborhood)), " with neighborhood, ",
            sum(!is.na(pts_out$section)), " with section)")
  }
  
  # ---- also produce a combined + deduped master for convenience ----
  tagged <- list.files(out_dir, pattern = "_tagged\\.geojson$", full.names = TRUE)
  
  read_one_tagged <- function(p) {
    g <- sf::st_read(p, quiet = TRUE)
    g <- ensure_core_cols(g)
    g$src <- basename(p)
    g
  }
  
  all_pts <- purrr::map_dfr(tagged, read_one_tagged)
  
  # build a stable dedupe key (prefer URL; else title+coords)
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
      )
    ) %>%
    dplyr::distinct(.data$key, .keep_all = TRUE) %>%
    dplyr::select(-url_norm, -title_norm, -lon_num, -lat_num)
  
  sf::st_write(all_pts2, file.path(out_dir, "ALL_combined_dedup_tagged.geojson"),
               delete_dsn = TRUE, quiet = TRUE)
  message("✓ wrote combined: ", file.path(out_dir, "ALL_combined_dedup_tagged.geojson"))
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
