# csv_saved_lists_to_kml.R — v10 (PDX-aware + URL preference + per-row geocode + robust overrides)
# - Trusts URL coords only if “Portland-ish” (~120 km of downtown)
# - URL parsing preference configurable: "place" (!3d/!4d) vs "viewport" (@lat,lon)
# - Skips unreliable URL types (/search, /dir, maps.app.goo.gl, ?cid=) → geocode
# - Geocoding fallback: Google (optional) → ArcGIS → OSM (US-biased, top-5, pick nearest)
# - Maintains numeric _geocode_cache.csv; writes <List>_MISSING.csv for unresolved rows
# - Per-list KML + GeoJSON outputs
# - Title/URL/Address-based coordinate overrides (applied before AND after geocoding)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(sf); library(stringr); library(tools); library(tibble)
})

`%||%` <- function(a,b) if (is.null(a)) b else a
safe <- function(x) gsub("[^A-Za-z0-9 _-]+","", x) |>
  str_squish() |>
  str_replace_all("\\s+","_")

ensure_cache <- function(path){
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(path)){
    readr::write_csv(tibble(query = character(), lat = double(), long = double()), path)
  }
}

normalize_latlon <- function(lat, lon){
  lat <- suppressWarnings(as.numeric(lat)); lon <- suppressWarnings(as.numeric(lon))
  bad <- any(abs(lat) > 90, na.rm=TRUE) || any(abs(lon) > 180, na.rm=TRUE)
  if (bad){ tmp <- lat; lat <- lon; lon <- tmp }
  list(lat=lat, lon=lon)
}

# --- Portland anchors ----------------------------------------------------------
PDX_CENTER <- c(lat = 45.523, lon = -122.676)

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  toRad <- function(x) x * pi/180
  dlat <- toRad(lat2 - lat1); dlon <- toRad(lon2 - lon1)
  a <- sin(dlat/2)^2 + cos(toRad(lat1))*cos(toRad(lat2))*sin(dlon/2)^2
  2*r*asin(pmin(1, sqrt(a)))
}

near_center <- function(lat, lon, center, max_km = 120) {
  if (is.null(center) || any(!is.finite(center))) return(rep(TRUE, length(lat)))
  d <- haversine_km(lat, lon, center["lat"], center["lon"])
  is.finite(d) & d <= max_km
}

# URL helpers -------------------------------------------------------------------
is_unreliable_url <- function(u) {
  if (is.na(u) || !nzchar(u)) return(TRUE)
  grepl("/search|/dir|maps\\.app\\.goo\\.gl|[?&]cid=", u, ignore.case = TRUE)
}

viewport_from_url <- function(url){
  if (is.na(url) || !nzchar(url)) return(c(NA_real_, NA_real_))
  u <- as.character(url)
  m <- stringr::str_match(u, "@(-?\\d+\\.?\\d*),(-?\\d+\\.?\\d*)")
  if (!is.na(m[1,2])) {
    sw <- normalize_latlon(m[1,2], m[1,3])
    return(c(lat = sw$lat, lon = sw$lon))
  }
  c(NA_real_, NA_real_)
}

# Strict URL coord parser (Portland-near only), with preference
coords_from_url_strict <- function(url, center, max_km = 120, prefer = c("place","viewport")) {
  prefer <- match.arg(prefer)
  if (is.na(url) || !nzchar(url)) return(c(NA_real_, NA_real_))
  u <- as.character(url); cand <- NULL
  
  # true place coords
  m <- stringr::str_match_all(u, "!3d(-?\\d+\\.?\\d*)!4d(-?\\d+\\.?\\d*)")[[1]]
  if (nrow(m)) cand <- rbind(cand, data.frame(lat = as.numeric(m[,2]), lon = as.numeric(m[,3]), src = "34", ord = seq_len(nrow(m))))
  m <- stringr::str_match_all(u, "!2d(-?\\d+\\.?\\d*)!3d(-?\\d+\\.?\\d*)")[[1]]
  if (nrow(m)) cand <- rbind(cand, data.frame(lat = as.numeric(m[,3]), lon = as.numeric(m[,2]), src = "23", ord = seq_len(nrow(m))))
  
  # viewport/query fallbacks
  m <- stringr::str_match(u, "@(-?\\d+\\.?\\d*),(-?\\d+\\.?\\d*)")
  if (!is.na(m[1,2])) cand <- rbind(cand, data.frame(lat = as.numeric(m[1,2]), lon = as.numeric(m[1,3]), src = "@",  ord = 1))
  m <- stringr::str_match(u, "(?:[?&]q=)(-?\\d+\\.?\\d*),(-?\\d+\\.?\\d*)")
  if (!is.na(m[1,2])) cand <- rbind(cand, data.frame(lat = as.numeric(m[1,2]), lon = as.numeric(m[1,3]), src = "q",  ord = 1))
  m <- stringr::str_match(u, "(?:[?&](?:ll|sll)=)(-?\\d+\\.?\\d*),(-?\\d+\\.?\\d*)")
  if (!is.na(m[1,2])) cand <- rbind(cand, data.frame(lat = as.numeric(m[1,2]), lon = as.numeric(m[1,3]), src = "ll", ord = 1))
  
  if (is.null(cand) || !nrow(cand)) return(c(NA_real_, NA_real_))
  
  sw <- normalize_latlon(cand$lat, cand$lon)
  cand$lat <- sw$lat; cand$lon <- sw$lon
  ok <- is.finite(cand$lat) & is.finite(cand$lon) & abs(cand$lat) <= 90 & abs(cand$lon) <= 180
  cand <- cand[ok, , drop = FALSE]
  if (!nrow(cand)) return(c(NA_real_, NA_real_))
  
  # must be Portland-ish
  near <- near_center(cand$lat, cand$lon, center, max_km = max_km)
  cand <- cand[near, , drop = FALSE]
  if (!nrow(cand)) return(c(NA_real_, NA_real_))
  
  # priority by preference
  if (prefer == "place") prio <- match(cand$src, c("34","23","@","q","ll"), nomatch = 99)
  else                   prio <- match(cand$src, c("@","34","23","q","ll"), nomatch = 99)
  cand <- cand[order(prio, cand$ord), , drop = FALSE]
  c(cand$lat[1], cand$lon[1])
}

# Name from /maps/place/<NAME>
name_from_url <- function(url){
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  u <- as.character(url)
  m <- stringr::str_match(u, "/maps/place/([^/?#]+)")
  if (!is.na(m[1,2])) {
    nm <- m[1,2]
    nm <- URLdecode(nm)
    nm <- gsub("\\+", " ", nm)
    return(nm)
  }
  NA_character_
}

# Read one CSV with (lat,lon) filled where possible
read_one <- function(path, center_hint = NULL, url_prefer = "viewport"){
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE, guess_max = 100000))
  cols <- names(df)
  
  name_col  <- intersect(cols, c("Title","Name","Place Name","Place name","Label","label"))[1]
  notes_col <- intersect(cols, c("Notes","Description","notes","description","Note","note","Comment","comment"))[1]
  url_col   <- intersect(cols, c("Google Maps URL","Link","URL","Maps URL","google_maps_url","Map link","Map Link"))[1]
  addr_col  <- intersect(cols, c("Address","address","Formatted Address","formatted_address"))[1]
  
  title <- if (!is.na(name_col)) as.character(df[[name_col]]) else file_path_sans_ext(basename(path))
  notes <- if (!is.na(notes_col)) as.character(df[[notes_col]]) else ""
  url   <- if (!is.na(url_col))   as.character(df[[url_col]])   else NA_character_
  addr  <- if (!is.na(addr_col))  as.character(df[[addr_col]])  else NA_character_
  
  # fill missing titles from URL
  missing_title <- is.na(title) | !nzchar(title)
  if (any(missing_title) && !is.na(url_col)) {
    guess <- vapply(url[missing_title], name_from_url, "", USE.NAMES = FALSE)
    title[missing_title & nzchar(guess)] <- guess[missing_title & nzchar(guess)]
    still <- missing_title & !nzchar(title)
    if (any(still)) title[still] <- file_path_sans_ext(basename(path))
  }
  
  lat <- rep(NA_real_, nrow(df)); lon <- rep(NA_real_, nrow(df))
  
  # explicit lat/lon columns
  lat_col <- intersect(cols, c("Latitude","latitude","lat","Lat","LAT"))[1]
  lon_col <- intersect(cols, c("Longitude","longitude","lon","Lon","LON","lng","LNG"))[1]
  if (!is.na(lat_col) && !is.na(lon_col)) {
    sw <- normalize_latlon(df[[lat_col]], df[[lon_col]])
    lat <- sw$lat; lon <- sw$lon
  }
  # "Coordinates" column
  if ("Coordinates" %in% cols) {
    parts <- strsplit(df$Coordinates, ",\\s*")
    lat2 <- vapply(parts, \(x) if (length(x)>=1) x[1] else NA_character_, "")
    lon2 <- vapply(parts, \(x) if (length(x)>=2) x[2] else NA_character_, "")
    sw   <- normalize_latlon(lat2, lon2)
    idx  <- is.na(lat) | is.na(lon)
    lat[idx] <- sw$lat[idx]; lon[idx] <- sw$lon[idx]
  }
  # URL-derived coords (try regardless of “unreliable” status; accept only if PDX-near)
  if (!is.na(url_col)) {
    idx <- which(is.na(lat) | is.na(lon))
    if (length(idx)) {
      for (i in idx) {
        c2 <- coords_from_url_strict(
          url[i],
          center = center_hint %||% PDX_CENTER,
          max_km = 120,
          prefer = url_prefer
        )
        if (is.finite(c2[1]) && is.finite(c2[2])) { lat[i] <- c2[1]; lon[i] <- c2[2] }
      }
    }
  }
  
  
  tibble(
    title = as.character(title),
    desc  = as.character(notes),
    address = as.character(addr),
    url   = as.character(url),
    lon   = as.numeric(lon),
    lat   = as.numeric(lat)
  )
}

# Post-filter (optional map radius)
filter_by_radius <- function(g, center = NULL, radius_km = NULL){
  if (is.null(center) || is.null(radius_km)) return(g)
  pt  <- sf::st_sfc(sf::st_point(c(center["lon"], center["lat"])), crs = 4326)
  g_m <- sf::st_transform(g, 3857)
  ptm <- sf::st_transform(pt, 3857)
  d_m <- as.numeric(sf::st_distance(sf::st_geometry(g_m), ptm))
  g[d_m <= radius_km * 1000, , drop = FALSE]
}

# --- Title/URL/Address-based overrides (fill exact coords before/after geocode)
OVERRIDES <- tribble(
  ~pattern,                                   ~lat,         ~lon,
  "COLLECTOR",                                45.526689,    -122.6519005,
  "Brave Neighbor Coffeehouse",               45.5475775,   -122.6848444,
  "El Salto PDX 100% Vegan Venezuelan",       45.4958909,   -122.6089242,
  "VeganO MexicanO pdx",                      45.512267,    -122.6323559,
  "Umami Sushi Pdx Food Cart",                45.5592714,   -122.6490367,
  "Junior's Cafe",                            45.5102111,   -122.6535237,
  "Yours Truly",                              45.4680613,   -122.6527505
)

apply_overrides <- function(dat, overrides, verbose = TRUE){
  if (is.null(overrides) || !nrow(overrides)) return(dat)
  
  # ensure character & replace NA with ""
  dat$title   <- ifelse(is.na(dat$title),   "", as.character(dat$title))
  dat$url     <- ifelse(is.na(dat$url),     "", as.character(dat$url))
  dat$address <- ifelse(is.na(dat$address), "", as.character(dat$address))
  
  for (k in seq_len(nrow(overrides))){
    pat  <- as.character(overrides$pattern[k])
    lato <- overrides$lat[k]
    lono <- overrides$lon[k]
    
    hit_title <- grepl(pat, dat$title,   ignore.case = TRUE, fixed = TRUE)
    hit_url   <- grepl(pat, dat$url,     ignore.case = TRUE, fixed = TRUE)
    hit_addr  <- grepl(pat, dat$address, ignore.case = TRUE, fixed = TRUE)
    hit <- (hit_title | hit_url | hit_addr)
    
    n_hit <- sum(hit, na.rm = TRUE)
    if (n_hit > 0){
      if (is.finite(lato)) dat$lat[hit] <- as.numeric(lato)
      if (is.finite(lono)) dat$lon[hit] <- as.numeric(lono)
      if (isTRUE(verbose)) message(sprintf("  • Override '%s' applied to %d row(s)", pat, n_hit))
    } else if (isTRUE(verbose)) {
      message(sprintf("  • Override '%s' matched 0 rows", pat))
    }
  }
  dat
}

# --- Per-row geocoder: choose best candidate, using viewport hint if present ---
geocode_one_best <- function(query, view_hint = NULL, center_hint = PDX_CENTER,
                             methods = c("google","arcgis","osm"),
                             pause_sec = 0.8, cq = list(countrycodes = "us")) {
  stopifnot(length(query) == 1)
  # Google
  if ("google" %in% methods) {
    key <- Sys.getenv("GOOGLE_MAPS_API_KEY", unset = NA)
    if (!is.na(key) && key != "") {
      g <- try(tidygeocoder::geocode(tibble(query = query), address = "query",
                                     method = "google", api_key = key, limit = 1,
                                     min_time = pause_sec, quiet = TRUE), silent = TRUE)
      if (!inherits(g, "try-error")) {
        lat <- suppressWarnings(as.numeric(g$lat)); lon <- suppressWarnings(as.numeric(g$long))
        if (isTRUE(near_center(lat, lon, center_hint, max_km = 120))) return(c(lat, lon))
      }
    }
  }
  # ArcGIS
  if ("arcgis" %in% methods) {
    g <- try(tidygeocoder::geocode(tibble(query = query), address = "query",
                                   method = "arcgis", limit = 1,
                                   min_time = pause_sec, quiet = TRUE), silent = TRUE)
    if (!inherits(g, "try-error")) {
      lat <- suppressWarnings(as.numeric(g$lat)); lon <- suppressWarnings(as.numeric(g$long))
      if (isTRUE(near_center(lat, lon, center_hint, max_km = 120))) return(c(lat, lon))
    }
  }
  # OSM (top-5; nearest to viewport if provided else nearest to PDX center)
  if ("osm" %in% methods) {
    g <- try(tidygeocoder::geocode(tibble(query = query), address = "query",
                                   method = "osm", limit = 5,
                                   min_time = pause_sec, quiet = TRUE,
                                   custom_query = cq), silent = TRUE)
    if (!inherits(g, "try-error") && nrow(g)) {
      lat <- suppressWarnings(as.numeric(g$lat)); lon <- suppressWarnings(as.numeric(g$long))
      keep <- is.finite(lat) & is.finite(lon)
      if (any(keep)) {
        lat <- lat[keep]; lon <- lon[keep]
        target <- if (!is.null(view_hint) && all(is.finite(view_hint))) view_hint else center_hint
        d2 <- (lat - target["lat"])^2 + (lon - target["lon"])^2
        i  <- which.min(d2)
        if (isTRUE(near_center(lat[i], lon[i], center_hint, max_km = 120))) return(c(lat[i], lon[i]))
      }
    }
  }
  c(NA_real_, NA_real_)
}

# --- Main converter ------------------------------------------------------------
convert_saved_csv_folder_to_kml <- function(
    in_dir,
    out_dir         = "out_lists_from_csv",
    include_files   = NULL,
    include_pattern = NULL,
    center          = NULL,           # optional post-filter c(lon=..., lat=...)
    radius_km       = NULL,           # optional post-filter radius
    dry_run         = FALSE,
    geocode_if_missing = FALSE,
    city_hint       = NULL,           # "Portland, OR" enables PDX behavior
    cache_csv       = NULL,           # "out_lists_portland/_geocode_cache.csv"
    pause_sec       = 1,
    geocode_methods = c("google","arcgis","osm"),
    report_missing_dir = NULL,
    url_prefer      = c("place","viewport"),
    overrides       = OVERRIDES       # pass NULL to disable or supply your own tribble
){
  url_prefer <- match.arg(url_prefer)
  
  stopifnot(dir.exists(in_dir))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(report_missing_dir)) dir.create(report_missing_dir, recursive = TRUE, showWarnings = FALSE)
  
  files <- list.files(in_dir, pattern="\\.csv$", full.names=TRUE, ignore.case=TRUE)
  if (!length(files)) stop("No CSVs found in: ", in_dir)
  
  bases <- basename(files)
  pick <- rep(TRUE, length(files))
  if (!is.null(include_files)) {
    pick <- bases %in% include_files
  } else if (!is.null(include_pattern)) {
    pick <- grepl(include_pattern, bases, perl = TRUE)
  }
  files <- files[pick]; bases <- bases[pick]
  if (!length(files)) stop("No CSVs matched your filter(s).")
  
  # cache (numeric)
  cache <- NULL
  if (!is.null(cache_csv)) {
    ensure_cache(cache_csv)
    cache <- suppressMessages(readr::read_csv(cache_csv, show_col_types = FALSE)) |>
      mutate(
        query = as.character(query),
        lat   = suppressWarnings(as.numeric(lat)),
        long  = suppressWarnings(as.numeric(long))
      )
    if (!all(c("query","lat","long") %in% names(cache))) stop("Cache CSV must have columns: query, lat, long")
  }
  if (isTRUE(geocode_if_missing) && !requireNamespace("tidygeocoder", quietly = TRUE)) {
    stop("Please install.packages('tidygeocoder') to enable geocoding.")
  }
  
  center_hint <- if (!is.null(city_hint) && grepl("(?i)portland", city_hint)) PDX_CENTER else NULL
  
  message("Converting ", length(files), " file(s):")
  message(paste0(" - ", bases), sep="\n")
  
  total_points <- 0L
  
  for (i in seq_along(files)) {
    f <- files[i]; base <- tools::file_path_sans_ext(bases[i])
    
    dat <- read_one(f, center_hint = center_hint, url_prefer = url_prefer)
    
    # apply title/URL/address-based overrides BEFORE geocoding
    dat <- apply_overrides(dat, overrides)
    
    # which rows still need geocoding?
    miss_idx <- which(!is.finite(dat$lat) | !is.finite(dat$lon))
    
    if (length(miss_idx) && isTRUE(geocode_if_missing)) {
      message(sprintf("  • Geocoding %d missing for '%s' (row-by-row)...", length(miss_idx), base))
      for (j in miss_idx) {
        qj <- if (!is.na(dat$address[j]) && nzchar(dat$address[j])) dat$address[j] else dat$title[j]
        if (!is.null(city_hint) && nzchar(city_hint) && !grepl("(?i)portland|or\\b|oregon|pdx", qj)) qj <- paste(qj, city_hint)
        
        # cache first
        if (!is.null(cache) && nrow(cache)) {
          k <- match(qj, cache$query)
          if (!is.na(k)) { dat$lat[j] <- cache$lat[k]; dat$lon[j] <- cache$long[k]; next }
        }
        
        # viewport hint if present
        vh <- viewport_from_url(dat$url[j])
        if (!all(is.finite(vh))) vh <- NULL
        
        # geocode best
        cxy <- geocode_one_best(qj,
                                view_hint   = vh,
                                center_hint = center_hint %||% PDX_CENTER,
                                methods     = geocode_methods,
                                pause_sec   = pause_sec,
                                cq          = list(countrycodes = "us"))
        if (is.finite(cxy[1]) && is.finite(cxy[2])) {
          dat$lat[j] <- cxy[1]; dat$lon[j] <- cxy[2]
          if (!is.null(cache_csv)) {
            # append to cache (in-memory + file)
            newrow <- tibble(query = qj, lat = cxy[1], long = cxy[2])
            if (is.null(cache) || !qj %in% cache$query) {
              cache <- bind_rows(cache, newrow)
              readr::write_csv(cache, cache_csv)
            }
          }
        }
      }
    }
    
    # apply overrides AGAIN after geocoding so manual pins always win
    dat <- apply_overrides(dat, overrides, verbose = FALSE)
    
    # unresolved → report
    still <- which(!is.finite(dat$lat) | !is.finite(dat$lon))
    if (length(still) && !is.null(report_missing_dir)) {
      miss_df <- dat[still, c("title","address","url")]
      out_miss <- file.path(report_missing_dir, paste0(safe(base), "_MISSING.csv"))
      readr::write_csv(miss_df, out_miss)
      message(sprintf("  • Wrote %d unresolved rows to %s", nrow(miss_df), out_miss))
    }
    
    # keep only rows with coords
    dat <- dat |> filter(is.finite(lat), is.finite(lon))
    if (!nrow(dat)) { message("⚠︎ No valid points for: ", bases[i]); next }
    
    # write KML + GeoJSON
    g <- sf::st_as_sf(dat, coords = c("lon","lat"), crs = 4326, remove = FALSE)
    g <- filter_by_radius(g, center, radius_km)
    if (!nrow(g)) { message("⚠︎ No points within radius for: ", bases[i]); next }
    
    if (dry_run) { message(sprintf("• %s → would write %d points", base, nrow(g))); next }
    
    base_safe <- safe(base)
    kml_path <- file.path(out_dir, paste0(base_safe, ".kml"))
    gj_path  <- file.path(out_dir, paste0(base_safe, ".geojson"))
    try(sf::st_write(g, kml_path, delete_dsn=TRUE, quiet=TRUE, driver="KML"))
    try(sf::st_write(g, gj_path,  delete_dsn=TRUE, quiet=TRUE))
    message(sprintf("✓ %s — %d places → %s, %s", base_safe, nrow(g), basename(kml_path), basename(gj_path)))
    total_points <- total_points + nrow(g)
  }
  
  if (!dry_run) {
    message("\nDone. Total places written: ", total_points,
            "\nOutput folder: ", normalizePath(out_dir))
  } else {
    message("\nDry run complete. No files were written.")
  }
  invisible(TRUE)
}
