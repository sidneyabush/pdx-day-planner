suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(readr)
  library(stringr)
  library(DT)
  library(httr)
  library(jsonlite)
  library(sf)
  library(base64enc)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------- Tag canonicalization & supercats ----------
canonicalize_tags <- function(x) {
  if (is.null(x)) return("")
  x <- tolower(x)
  x <- gsub("\\btheatre(s)?\\b", "theater\\1", x)
  x <- gsub("\\bmuseums\\b", "museum", x)
  x <- gsub("\\bgalleries\\b", "gallery", x)
  x <- gsub("\\btrails\\b", "trail", x)
  x <- gsub("\\bparks\\b", "park", x)
  stringr::str_squish(x)
}

derive_supercats <- function(x) {
  x <- tolower(x %||% "")
  add <- character(length(x))
  is_ent <- grepl("\\b(museum|gallery|theater|cinema|film|show|performance|movie theater)\\b", x)
  is_nat <- grepl("\\b(park|garden|trail|hike|nature|forest|arboretum|outdoor)\\b", x)
  add[is_ent & is_nat] <- "entertainment; nature"
  add[is_ent & !is_nat] <- "entertainment"
  add[!is_ent & is_nat] <- "nature"
  add
}

add_token <- function(tags, token, mask) {
  tags <- ifelse(is.na(tags), "", tags)
  need <- mask & !grepl(paste0("\\b", token, "\\b"), tags, ignore.case = TRUE)
  ifelse(need & nzchar(tags), paste(tags, token, sep = "; "),
         ifelse(need, token, tags))
}

# ---------- Backfill tags from other columns (museums/theaters/parks/trails) ----------
backfill_core_tags <- function(df) {
  if (!nrow(df)) return(df)
  if (!("tags" %in% names(df))) df$tags <- ""
  df$tags <- canonicalize_tags(df$tags)
  
  lc <- function(col) tolower(as.character(df[[col]] %||% ""))
  anytext <- paste(
    tolower(as.character(df$title %||% df$name %||% "")),
    lc("amenity"), lc("tourism"), lc("leisure"),
    lc("category"), lc("feature"), lc("note"), lc("notes"),
    lc("description"), lc("type"), lc("kinds"), lc("subcategory"),
    sep = " | "
  )
  
  looks_theater <- grepl("\\b(theater|theatre|cinema|movie|screening|playhouse|performing arts)\\b", anytext)
  looks_park    <- grepl("\\b(park|garden|arboretum|nature)\\b", anytext)
  looks_museum  <- grepl("\\b(museum)\\b", anytext)
  looks_trail   <- grepl("\\b(trail|greenway|rail trail|singletrack|loop|path)\\b", anytext)
  
  df$tags <- add_token(df$tags, "theater", looks_theater)
  df$tags <- add_token(df$tags, "park",    looks_park)
  df$tags <- add_token(df$tags, "museum",  looks_museum)
  df$tags <- add_token(df$tags, "trail",   looks_trail)
  
  df$tags <- canonicalize_tags(df$tags)
  supers <- derive_supercats(df$tags)
  df$tags <- ifelse(nzchar(supers), paste(df$tags, supers, sep = "; "), df$tags)
  df$tags <- stringr::str_squish(df$tags)
  df
}

# ---------- Address / distance helpers ----------
home_is_set <- function(addr, lat, lng) {
  !is.null(addr) && nzchar(addr) && !is.na(lat) && !is.na(lng)
}

calc_distance_miles <- function(lat1, lng1, lat2, lng2) {
  if (any(is.na(c(lat1, lng1, lat2, lng2)))) return(NA_real_)
  R <- 3959
  lat1 <- lat1 * pi/180; lat2 <- lat2 * pi/180
  dlat <- (lat2 - lat1); dlng <- (lng2 - lng1) * pi/180
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlng/2)^2
  round(R * 2 * atan2(sqrt(a), sqrt(1 - a)), 2)
}

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

is_within_radius <- function(lat1,lng1,lat2,lng2,radius_miles=1) {
  d <- calc_distance_miles(lat1,lng1,lat2,lng2)
  !is.na(d) && d <= radius_miles
}

# Single-hop "on the way back" check vs home
is_on_way_back <- function(home_lat, home_lng, v1_lat, v1_lng, cand_lat, cand_lng, detour_factor = 1.25) {
  if (any(is.na(c(home_lat, home_lng, v1_lat, v1_lng, cand_lat, cand_lng)))) return(FALSE)
  d_home_v1   <- calc_distance_miles(home_lat, home_lng, v1_lat, v1_lng)
  d_home_cand <- calc_distance_miles(home_lat, home_lng, cand_lat, cand_lng)
  d_v1_cand   <- calc_distance_miles(v1_lat, v1_lng, cand_lat, cand_lng)
  towards_home <- (!is.na(d_home_cand) && !is.na(d_home_v1) && d_home_cand <= d_home_v1)
  if (!towards_home) return(FALSE)
  baseline <- d_home_v1
  with_cand <- d_v1_cand + d_home_cand
  !any(is.na(c(baseline, with_cand))) && (with_cand <= detour_factor * baseline)
}

# Chain validator: up to 3 stops must be en route (short hops and/or trending toward home)
chain_ok <- function(home_lat, home_lng, pts, hop_radius = 1.0, detour_factor = 1.35) {
  # pts is a data.frame with columns lat, lng (order = planned order)
  if (nrow(pts) == 0) return(FALSE)
  if (nrow(pts) == 1) return(TRUE)
  
  # Hops must be close OR trending toward home
  for (i in 2:nrow(pts)) {
    p_prev <- pts[i-1,]; p_cur <- pts[i,]
    hop_ok <- is_within_radius(p_prev$lat, p_prev$lng, p_cur$lat, p_cur$lng, hop_radius)
    if (!hop_ok && !is.na(home_lat) && !is.na(home_lng)) {
      hop_ok <- is_on_way_back(home_lat, home_lng, p_prev$lat, p_prev$lng, p_cur$lat, p_cur$lng, detour_factor = 1.25)
    }
    if (!hop_ok) return(FALSE)
  }
  
  # Total path vs direct home->last
  if (!is.na(home_lat) && !is.na(home_lng)) {
    # path = home -> p1 -> p2 -> p3 -> home
    path_len <- 0
    # Start at home
    path_len <- path_len + calc_distance_miles(home_lat, home_lng, pts$lat[1], pts$lng[1])
    # Through each hop
    for (i in 2:nrow(pts)) {
      path_len <- path_len + calc_distance_miles(pts$lat[i-1], pts$lng[i-1], pts$lat[i], pts$lng[i])
    }
    # Back home
    path_len <- path_len + calc_distance_miles(pts$lat[nrow(pts)], pts$lng[nrow(pts)], home_lat, home_lng)
    
    direct <- calc_distance_miles(home_lat, home_lng, pts$lat[nrow(pts)], pts$lng[nrow(pts)]) * 2  # out-and-back baseline
    if (!is.na(path_len) && !is.na(direct)) {
      return(path_len <= detour_factor * direct)
    }
  }
  TRUE
}

# ---------- Natural-language suffix for “Surprise Me” card ----------
action_suffix_from_tags <- function(tags_text) {
  t <- tolower(tags_text %||% "")
  pick <- function(...) any(grepl(paste0("(", paste(list(...), collapse="|"), ")"), t))
  
  if (pick("mural","bridge","view","scenic","architecture","historic","photo","photos")) return("and take film photos")
  if (pick("bookstore","library")) return("and find a new book")
  if (pick("coffee","cafe")) return("and read")
  if (pick("museum","gallery","art")) return("and draw/sketch")
  if (pick("trail","hike","park","garden","arboretum","nature","forest")) return("and take a walk")
  if (pick("thrift","vintage","antique","market")) return("and thrift")
  if (pick("record","vinyl","music")) return("and dig for records")
  if (pick("brewery","bar","cocktail","wine","pub","beer")) return("and grab a drink")
  if (pick("bakery","dessert","sweet","donut","pastry","bagel")) return("and treat yourself")
  if (pick("restaurant","food","lunch","dinner","brunch","cart")) return("and grab a bite")
  "and explore"
}

# ---------- Transit selection by distance ----------
transit_by_distance <- function(d_mi) {
  if (is.na(d_mi)) return("🚌 Public Transit")
  if (d_mi <= 2) return("🚶 Walk")
  if (d_mi <= 8) return("🚲 Bicycle Rights!")
  if (d_mi > 10) return("🚗 Drive")
  "🚌 Public Transit"
}

# ======= DATA & MAP SETUP (paste this after your helpers, before ui <- fluidPage) =======

# --- App dir + header image  --------------------------
get_app_dir <- function() {
  tryCatch(normalizePath(dirname(sys.frame(1)$ofile)),
           error = function(e) normalizePath(getwd()))
}
APP_DIR <- get_app_dir()

HEADER_IMG <- tryCatch({
  img_path <- file.path(APP_DIR, "www:", "pdx_header.png")  # <- no colon
  if (file.exists(img_path)) base64enc::dataURI(file = img_path, mime = "image/png") else NULL
}, error = function(e) NULL)


# --- Load processed places --------------------------------------------------
processed_file <- "data/portland_places_processed.rds"
if (!file.exists(processed_file)) stop("❌ Missing processed data. Run process_data.R first.")
places <- readRDS(processed_file)
if (!is.data.frame(places) || nrow(places) == 0) stop("❌ Invalid processed data.")

# ensure lat/lng numeric and drop junk titles
if ("lat" %in% names(places)) places$lat <- suppressWarnings(as.numeric(places$lat))
if ("lng" %in% names(places)) places$lng <- suppressWarnings(as.numeric(places$lng))
if ("title" %in% names(places)) {
  places <- places[!grepl("^Unnamed place", places$title, ignore.case = TRUE), ]
  places <- places[!is.na(places$title) & nzchar(trimws(places$title)), ]
}

# --- Polygon helpers + load neighborhood/sextant boundaries ----------------
load_map_file <- function(paths) { for (p in paths) if (file.exists(p)) return(p); NULL }
safe_read_sf <- function(path) {
  if (is.null(path)) return(NULL)
  tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NULL)
}
pick_name_col <- function(sfobj, cands) {
  cands <- cands[cands %in% names(sfobj)]
  if (length(cands)) cands[[1]] else NULL
}

sextants_path <- load_map_file(c(
  "archive/Portland_Administrative_Sextants.geojson",
  "data/Portland_Administrative_Sextants.geojson"
))
sections_boundaries <- safe_read_sf(sextants_path)
SEC_NAME_COL <- if (!is.null(sections_boundaries)) pick_name_col(sections_boundaries,
                                                                 c("Sextant","SEXTANT","PREFIX","NAME")) else NULL

neighborhood_path <- load_map_file(c(
  "archive/Neighborhood_Boundaries.geojson",
  "archive/neighborhoods.geojson",
  "data/Neighborhood_Boundaries.geojson"
))
neighborhood_boundaries <- safe_read_sf(neighborhood_path)
NEI_NAME_COL <- if (!is.null(neighborhood_boundaries)) pick_name_col(neighborhood_boundaries,
                                                                     c("MAPLABEL","NAME","Label","Neighborhood","NEIGHBORHD","neigh","label")) else NULL

# --- Sextant name normalization & small label helpers ----------------------
normalize_sextant <- function(x) {
  x <- trimws(as.character(x)); if (!length(x)) return(character(0))
  alias <- c(
    "SW"="Southwest","S.W."="Southwest","South West"="Southwest",
    "SE"="Southeast","S.E."="Southeast","South East"="Southeast",
    "NW"="Northwest","N.W."="Northwest","North West"="Northwest",
    "NE"="Northeast","N.E."="Northeast","North East"="Northeast",
    "N" ="North","S"="South"
  )
  out <- ifelse(!is.na(alias[x]), alias[x], x)
  proper <- c("North","South","Northeast","Northwest","Southeast","Southwest")
  out <- ifelse(out %in% proper, out, tools::toTitleCase(gsub("\\s+"," ", out)))
  out
}
neigh_display_vec <- function(geo, txt) ifelse(!is.na(geo) & nzchar(geo), geo, txt)
label_with_neigh <- function(title, neigh) {
  if (!is.na(neigh) && nzchar(neigh)) paste0(title, " (", neigh, ")") else title
}

# --- Point-in-polygon enrichment ------------------------------------------
geo_enrich_places <- function(df, neighborhoods_sf, nei_col, sextants_sf, sec_col) {
  df$neighborhood_geo <- NA_character_
  df$section_geo <- NA_character_
  if (nrow(df) == 0) return(df)
  pts <- tryCatch(sf::st_as_sf(df, coords = c("lng","lat"), crs = 4326, remove = FALSE),
                  error = function(e) NULL)
  if (!is.null(pts) && !is.null(neighborhoods_sf) && !is.null(nei_col)) {
    nb <- neighborhoods_sf
    if (!is.na(sf::st_crs(nb)) && sf::st_crs(nb)$epsg != 4326) nb <- sf::st_transform(nb, 4326)
    joined_nb <- suppressWarnings(sf::st_join(pts, nb[, nei_col, drop = FALSE], join = sf::st_within, left = TRUE))
    df$neighborhood_geo <- as.character(joined_nb[[nei_col]])
  }
  if (!is.null(pts) && !is.null(sextants_sf) && !is.null(sec_col)) {
    sx <- sextants_sf
    if (!is.na(sf::st_crs(sx)) && sf::st_crs(sx)$epsg != 4326) sx <- sf::st_transform(sx, 4326)
    joined_sx <- suppressWarnings(sf::st_join(pts, sx[, sec_col, drop = FALSE], join = sf::st_within, left = TRUE))
    raw_sec <- as.character(joined_sx[[sec_col]])
    df$section_geo <- normalize_sextant(raw_sec)
  }
  df
}

# apply polygon labels
places <- geo_enrich_places(places, neighborhood_boundaries, NEI_NAME_COL,
                            sections_boundaries, SEC_NAME_COL)

# --- Backfill tags (museum/theater/park/trail) + supercats ----------------
places <- backfill_core_tags(places)

# --- Activity & mode matchers + tag cleaner (used later) -------------------
matches_activity <- function(tags_text, category_terms) {
  if (is.na(tags_text) || tags_text == "") return(FALSE)
  tags_lower <- tolower(tags_text)
  any(vapply(category_terms, function(term) {
    clean_term <- gsub("[^A-Za-z ]", "", term) |> stringr::str_trim() |> tolower()
    if (clean_term != "") stringr::str_detect(tags_lower, stringr::fixed(clean_term)) else FALSE
  }, logical(1)))
}
matches_activity_mode <- function(title, tags, note, mode_terms) {
  if (is.na(title)) title <- ""; if (is.na(tags)) tags <- ""; if (is.na(note)) note <- ""
  combined_text <- tolower(paste(title, tags, note))
  any(vapply(mode_terms, function(term) {
    clean_term <- gsub("[^A-Za-z ]", "", term) |> stringr::str_trim() |> tolower()
    stringr::str_detect(combined_text, stringr::fixed(clean_term))
  }, logical(1)))
}
clean_tags <- function(tags_text) {
  if (is.na(tags_text) || tags_text == "") return("")
  cleaned <- gsub("[^A-Za-z0-9 .,!?()-]", " ", tags_text)
  cleaned <- stringr::str_squish(cleaned)
  words <- strsplit(cleaned, " ")[[1]]
  words <- words[nchar(words) > 2 | words %in% c("&", "or", "of")]
  paste(words, collapse = " ")
}

# --- Geocoding + simple visited persistence -------------------------------
geocode_address <- function(address) {
  tryCatch({
    if (is.null(address) || stringr::str_trim(address) == "") {
      return(list(success = FALSE, error = "Please enter an address"))
    }
    encoded <- utils::URLencode(stringr::str_trim(address))
    url <- paste0("https://nominatim.openstreetmap.org/search?q=", encoded, "&format=json&limit=1")
    response <- httr::GET(url, httr::user_agent("Portland Day Planner App"))
    if (httr::status_code(response) != 200) return(list(success = FALSE, error = "Geocoding unavailable. Try later."))
    content <- httr::content(response, "text", encoding = "UTF-8")
    result <- jsonlite::fromJSON(content)
    if (length(result) > 0 && nrow(result) > 0) {
      return(list(
        success = TRUE,
        lat = as.numeric(result$lat[1]),
        lng = as.numeric(result$lon[1]),
        formatted_address = result$display_name[1]
      ))
    }
    list(success = FALSE, error = "Address not found. Try a different format.")
  }, error = function(e) list(success = FALSE, error = paste("Error:", e$message)))
}

if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b
if (!exists("load_completed", mode = "function")) {
  load_completed <- function() {
    f <- "data/completed_places.rds"
    if (file.exists(f)) { tryCatch(readRDS(f), error = function(e) character(0)) } else { character(0) }
  }
}
if (!exists("save_completed", mode = "function")) {
  save_completed <- function(ids) {
    dir.create("data", showWarnings = FALSE, recursive = TRUE)
    tryCatch(saveRDS(ids, "data/completed_places.rds"), error = function(e) invisible(NULL))
    invisible(TRUE)
  }
}


ui <- fluidPage(
  # Floating weather/time stack in top-right of app
  div(class="app-weather-corner", uiOutput("weather_ui")),
  
  tags$head(
    tags$style(HTML("
  @import url('https://fonts.googleapis.com/css2?family=Work+Sans:wght@300;400;500;600;700&family=Poppins:wght@300;400;500;600;700&display=swap');
  :root{ 
    --bg:#fefcfb; --bg-grad:#fcf8f6; --card:#ffffff; --border:#f4e8e1; --text:#2d2926; --muted:#8b7765; 
    --accent:#e8a083; --accent-600:#e08a63; --accent-50:#fef7f4; --accent-hover:#dc8965;
    --secondary:#f2c2a7; --secondary-600:#ee9f7a; --secondary-50:#fef9f6;
    --tertiary:#f7d4c4; --tertiary-600:#f3b8a0; --tertiary-50:#fefaf8;
    --success:#7fb069; --danger:#d67e7e; --warning:#e6c79c; 
    --shadow-soft:0 4px 20px rgba(164,180,148,0.08); --shadow-medium:0 8px 32px rgba(164,180,148,0.12);
    --radius-sm:8px; --radius-md:12px; --radius-lg:16px; --radius-xl:24px;
  }
  * { box-sizing: border-box; }
  body { 
    font-family:'Work Sans',system-ui,sans-serif !important; 
    background:var(--bg); 
    color:var(--text); margin:0; min-height:100vh; line-height:1.6;
  }
  
  /* Weather/time stack fixed in top-right (floats while scrolling) */
  .app-weather-corner {
    position:fixed; top:80px; right:20px; z-index:1000;
    display:flex; flex-direction:column; gap:8px;
  }

  /* Header card */
  .header { 
    background:var(--card); color:var(--text); padding:20px; margin:-15px -15px 0 -15px;
    box-shadow:var(--shadow-soft); border-radius:0 0 var(--radius-xl) var(--radius-xl); 
    border-bottom:1px solid var(--border);
  }

  /* Modern header layout */
  .header-grid { display:flex; flex-direction:column; gap:12px; align-items:center; }
  .header-content-row { display:grid; grid-template-columns: 1fr 1fr; gap:24px; width:80%; max-width:900px; align-items:start; margin:0 auto; }
  .header-image-section { display:flex; justify-content:center; }
  .header-suggestion-section { display:flex; align-items:center; }
  
  .image-container {
    position:relative; width:350px; height:280px; border-radius:var(--radius-xl); 
    overflow:hidden; box-shadow:var(--shadow-medium); 
    transition:transform 0.3s ease, box-shadow 0.3s ease;
  }
  .image-container:hover { transform:translateY(-4px); box-shadow:0 16px 48px rgba(0,0,0,0.15); }
  
  .header-photo { 
    width:100%; height:100%; object-fit:cover; display:block;
  }

  /* Overlay weather displays - compact and modern (NO emojis) */
  .time-display {
    background:rgba(255,255,255,0.95); backdrop-filter:blur(12px);
    border:1px solid rgba(255,255,255,0.6); border-radius:var(--radius-md);
    padding:12px 16px; transition:all 0.3s ease; min-width:140px;
    box-shadow:0 4px 20px rgba(0,0,0,0.1);
  }
  .time-display:hover { transform:translateY(-2px); box-shadow:0 6px 24px rgba(0,0,0,0.15); }
  .time-label {
    font-size:10px; text-transform:uppercase; letter-spacing:0.8px;
    color:var(--muted); font-weight:600; margin-bottom:2px;
  }
  .time-value {
    font-size:13px; font-weight:600; color:var(--text);
    font-family:'Poppins',sans-serif; line-height:1.2;
  }

  .weather-display {
    background:rgba(255,255,255,0.95); backdrop-filter:blur(12px);
    border:1px solid rgba(255,255,255,0.6); border-radius:var(--radius-md);
    padding:12px 16px; transition:all 0.3s ease; min-width:180px;
    box-shadow:0 4px 20px rgba(0,0,0,0.1);
  }
  .weather-display:hover { transform:translateY(-2px); box-shadow:0 6px 24px rgba(0,0,0,0.15); }
  .weather-main { display:flex; justify-content:space-between; align-items:center; margin-bottom:8px; }
  .weather-condition { font-size:12px; font-weight:600; color:var(--text); text-transform:capitalize; font-family:'Poppins',sans-serif; }
  .weather-temp { font-size:16px; font-weight:700; color:var(--text); font-family:'Poppins',sans-serif; }
  .weather-details { display:flex; gap:8px; flex-wrap:wrap; }
  .weather-detail {
    font-size:9px; color:var(--muted); font-weight:500;
    background:rgba(255,255,255,0.6); padding:2px 6px;
    border-radius:8px; text-transform:uppercase; letter-spacing:0.5px;
  }

  /* Mobile responsive */
  @media (max-width: 768px){
    .header { padding:16px; }
    .header-grid { gap:12px; }
    .header-content-row { grid-template-columns: 1fr; gap:16px; }
    .image-container { width:100%; max-width:300px; height:240px; }
    .hero-card { height:240px; padding:24px; }
    .header-controls { margin-top:12px; }
  }

  .header h1 { 
    font-family:'Poppins',sans-serif; font-weight:700; margin:0 0 16px 0; 
    font-size:3rem; letter-spacing:-0.02em; color:var(--text);
  }
  .homebase-line { color:var(--muted); margin:0 0 16px 0; font-size:0.95rem; font-weight:400; }

  .header-controls { margin-top:24px; }
  .cta-row { display:flex; justify-content:center; margin-bottom:20px; }
  .btn-big { 
    font-family:'Poppins',sans-serif; font-weight:600; padding:16px 32px; 
    border-radius:var(--radius-md); border:none;
    background:#e8a083 !important; color:white; box-shadow:var(--shadow-soft); 
    transition:all 0.3s cubic-bezier(0.4,0,0.2,1); cursor:pointer;
    text-decoration:none; display:inline-block; position:relative; overflow:hidden;
  }
  .btn-big::before {
    content:''; position:absolute; top:0; left:-100%; width:100%; height:100%;
    background:linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
    transition:left 0.6s; z-index:1; pointer-events:none;
  }
  .btn-big:hover { transform:translateY(-3px); box-shadow:0 8px 25px rgba(232,160,131,0.4); background:#e08a63 !important; }
  .btn-big:hover::before { left:100%; }
  .btn-big:active { transform:translateY(-1px); }
  .btn-surprise .btn-icon{ display:inline-block; margin-right:8px; position:relative; width:16px; height:16px; vertical-align:middle; }
  .btn-surprise .btn-icon::before, .btn-surprise .btn-icon::after{
    content:''; position:absolute; width:4px; height:4px; background:currentColor; border-radius:50%;
    animation:pulse 1.5s infinite; pointer-events:none;
  }
  .btn-surprise .btn-icon::before { top:0; left:0; animation-delay:0s; }
  .btn-surprise .btn-icon::after { bottom:0; right:0; animation-delay:0.75s; }
  @keyframes pulse { 0%,100%{opacity:.3; transform:scale(.8);} 50%{opacity:1; transform:scale(1.2);} }

  /* Address box */
  .address-box { margin-top:20px; padding:20px; border:1px solid var(--border); border-radius:var(--radius-md); background:var(--card); box-shadow:var(--shadow-soft); transition:all 0.3s ease; }
  .address-box:hover { box-shadow:var(--shadow-medium); }
  .address-grid { display:grid; grid-template-columns: 1fr auto; gap:16px; align-items:center; }
  .address-status { font-size:13px; color:var(--muted); margin-top:8px; font-weight:500; }

  /* Control panel */
  .control-panel { background:var(--card); border-radius:var(--radius-lg); padding:28px; border:1px solid var(--border); box-shadow:var(--shadow-soft); transition:all 0.3s ease; position:relative; }
  .control-panel:hover { box-shadow:var(--shadow-medium); }
  .control-panel h4, .control-panel h5 { color:var(--text); font-weight:600; margin:8px 0 12px 0; font-size:1.05em; letter-spacing:-0.01em; font-family:'Poppins',sans-serif; }

  /* Activity / transport chips */
  .activity-btn{ margin:6px 4px; padding:12px 16px; border-radius:var(--radius-md); border:1px solid var(--border); background:var(--card); color:var(--text); cursor:pointer; font-size:13px; font-weight:500; transition:all .3s cubic-bezier(.4,0,.2,1); display:inline-block; position:relative; overflow:hidden; }
  .activity-btn::before{ content:''; position:absolute; top:0; left:-100%; width:100%; height:100%; background:linear-gradient(90deg, transparent, rgba(108,123,92,0.1), transparent); transition:left .5s; z-index:0; pointer-events:none; }
  .activity-btn:hover{ border-color:var(--accent); transform:translateY(-2px); box-shadow:0 4px 12px rgba(232,160,131,0.2); background:var(--accent-50); }
  .activity-btn:hover::before{ left:100%; }
  .activity-btn.active{ background:var(--accent); color:white; border-color:var(--accent); box-shadow:0 6px 16px rgba(232,160,131,0.4); transform:translateY(-1px); }

  .transport-btn{ margin:6px 4px; padding:14px 18px; border-radius:var(--radius-md); border:1px solid var(--border); background:var(--card); cursor:pointer; font-weight:600; transition:all .3s cubic-bezier(.4,0,.2,1); color:var(--text); position:relative; overflow:hidden; }
  .transport-btn::before{ content:''; position:absolute; top:0; left:-100%; width:100%; height:100%; background:linear-gradient(90deg, transparent, rgba(155,139,122,0.1), transparent); transition:left .5s; z-index:0; pointer-events:none; }
  .transport-btn:hover{ border-color:var(--secondary); transform:translateY(-2px); box-shadow:0 4px 12px rgba(155,139,122,0.2); background:var(--secondary-50); }
  .transport-btn:hover::before{ left:100%; }
  .transport-btn.active{ background:var(--secondary-50); border-color:var(--secondary); color:var(--secondary-600); box-shadow:0 4px 12px rgba(155,139,122,0.25); transform:translateY(-1px); }

  /* Hero card */
  .hero-suggestion{ margin-bottom:32px; }
  .hero-card{
    background:var(--accent-50); color:var(--text); border-radius:var(--radius-xl); padding:32px; 
    box-shadow:var(--shadow-medium); position:relative; overflow:hidden; transition:all .4s cubic-bezier(.4,0,.2,1);
    border:none; height:280px; display:flex; flex-direction:column; justify-content:center;
  }
  .hero-card::before{ content:''; position:absolute; top:0; left:0; right:0; bottom:0; background:linear-gradient(45deg, transparent 0%, rgba(255,255,255,0.1) 50%, transparent 100%); transform:translateX(-100%); transition:transform .6s; pointer-events:none; }
  .hero-card:hover::before{ transform:translateX(100%); }
  .hero-card:hover{ transform:translateY(-4px) scale(1.02); box-shadow:0 12px 40px rgba(14,165,233,0.25); }
  .hero-card h2{ font-family:'Poppins',sans-serif; font-size:2.6rem; font-weight:700; margin:0 0 16px 0; line-height:1.2; }
  .hero-card .description{ font-size:1.4rem; font-weight:400; opacity:0.95; margin-bottom:20px; line-height:1.4; }
  .hero-card .details{ display:flex; gap:24px; flex-wrap:wrap; margin-top:auto; }
  .hero-card .detail-chip{ background:rgba(255,255,255,0.8); backdrop-filter:blur(10px); padding:8px 16px; border-radius:20px; font-size:14px; font-weight:500; color:var(--text); }

  .hero-empty{ background:var(--card); border:2px dashed var(--border); border-radius:var(--radius-xl); padding:60px 40px; text-align:center; color:var(--muted); min-height:200px; display:flex; flex-direction:column; justify-content:center; }
  .hero-empty h2{ font-family:'Poppins',sans-serif; font-size:1.8rem; margin-bottom:12px; color:var(--text); }

  /* Data table + map */
  .dataTables_wrapper{ background:var(--card); border-radius:var(--radius-lg); padding:24px; box-shadow:var(--shadow-soft); border:1px solid var(--border); transition:all .3s ease; }
  .dataTables_wrapper:hover{ box-shadow:var(--shadow-medium); }
  .leaflet-container{ border-radius:var(--radius-lg); box-shadow:var(--shadow-medium); transition:all .3s ease; }
  .leaflet-container:hover{ box-shadow:0 12px 40px rgba(0,0,0,0.15); }

  /* Forms */
  .form-control, .selectize-input, input[type=text], select{
    border:1px solid var(--border) !important; border-radius:var(--radius-sm) !important;
    padding:12px 16px !important; font-size:14px !important; transition:all .3s ease !important; background:var(--card) !important;
  }
  .form-control:focus, .selectize-input.focus, input[type=text]:focus, select:focus{
    border-color:var(--accent) !important; box-shadow:0 0 0 3px rgba(246,139,139,0.1) !important; outline:none !important;
  }
  .btn-primary, .btn-outline-primary{ border-radius:var(--radius-sm) !important; font-weight:500 !important; padding:12px 20px !important; transition:all .3s ease !important; border:1px solid var(--accent) !important; }
  .btn-primary{ background:var(--accent) !important; color:white !important; }
  .btn-primary:hover{ transform:translateY(-2px) !important; box-shadow:0 6px 16px rgba(246,139,139,0.3) !important; }
  .btn-outline-primary{ background:var(--card) !important; color:var(--accent) !important; }
  .btn-outline-primary:hover{ background:var(--accent-50) !important; transform:translateY(-1px) !important; }

  #sextant_label, #neighborhood_label { margin-bottom: 8px !important; }

  @media (max-width:1100px){
    .header-grid { grid-template-columns: 1fr; }
    .header-left { width:100%; align-items:center; }
    .header-photo { width:100%; max-width:420px; height:auto; }
    .control-panel { padding:20px; }
  }
")),
  ),
  
  # ======= HEADER =======
  div(class = "header",
      div(class = "header-grid",
          # Title + current home base string (server fills)
          div(class = "header-title-section", style = "text-align: center; width: 100%;",
              h1("The Dream of the 90s is Alive in Portland"),
              uiOutput("home_info_ui")
          ),
          # Image + Hero Suggestion
          div(class = "header-content-row",
              # Left: image (height matches hero card)
              div(class="header-image-section",
                  div(class="image-container",
                      if (!is.null(HEADER_IMG))
                        tags$img(src = HEADER_IMG, alt = "Portland", class = "header-photo")
                  )
              ),
              # Right: suggestion card
              div(class="header-suggestion-section",
                  div(class="hero-suggestion",
                      uiOutput("hero_suggestion_display")
                  )
              )
          ),
          # Big Surprise button (center)
          div(class = "header-controls", style = "text-align: center; width: 100%; margin-top: 8px;",
              actionButton("random_inspiration", HTML("<span class='btn-icon'></span>Surprise Me"),
                           class = "btn-big btn-surprise",
                           style = "font-size: 1.5rem; padding: 24px 48px; font-weight: 700;")
          )
      )
  ),
  
  # ======= MAIN CONTENT =======
  fluidRow(
    column(
      4,
      div(class = "control-panel",
          div(class="address-box",
              h5("Starting Address"),
              div(class="address-grid",
                  textInput("home_address", "", placeholder = "Enter your address (e.g., 123 Main St, Portland, OR)", value = "", width = "100%"),
                  actionButton("geocode_address", "Set Location", class = "btn-outline-primary", style = "min-width: 140px;")
              ),
              div(id = "address_status", class="address-status")
          ),
          br(),
          h5("What's the mood?"),
          selectizeInput("context_filter", "", choices = names(CONTEXT_FILTERS), selected = NULL, multiple = TRUE,
                         options = list(placeholder = 'Any context'), width = "100%"),
          
          h5("How much time ya got?"),
          selectInput("time_filter", "",
                      choices = list("Quick (1-2 hours)" = "quick",
                                     "Half day (3-4 hours)" = "half_day",
                                     "Full day (5+ hours)" = "full_day"),
                      selected = "quick", width = "100%"),
          
          h5(id="sextant_label","Quadrant"),
          uiOutput("section_selector"),
          
          h5(id="neighborhood_label","Neighborhood"),
          uiOutput("neighborhood_selector"),
          
          br(),
          h4("What kinds of places?"),
          div(id = "activity_buttons",
              lapply(names(ACTIVITY_CATEGORIES), function(cat) {
                actionButton(paste0("act_", gsub("[^A-Za-z0-9]", "", cat)), cat, class = "activity-btn")
              })
          ),
          
          br(),
          h5("Whatcha wanna do there?"),
          div(id = "activity_mode_buttons",
              lapply(names(ACTIVITY_MODES), function(mode) {
                actionButton(paste0("mode_", gsub("[^A-Za-z0-9]", "", mode)), mode, class = "activity-btn")
              })
          ),
          
          br(), br(),
          h5("How ya getting there?"),
          div(id = "transport_buttons",
              lapply(names(TRANSPORT_MODES), function(mode) {
                actionButton(paste0("trans_", gsub("[^A-Za-z0-9]", "", mode)), mode, class = "transport-btn")
              })
          ),
          
          br(), br(),
          textInput("keyword_filter", "Additional keywords:", ""),
          
          hr(),
          div(style = "text-align: center; margin-bottom: 12px;",
              actionButton("suggest_place", "Get Suggestions", class = "btn-primary", style = "width: 100%;")
          ),
          
          hr(),
          h5("Places Visited"),
          verbatimTextOutput("visited_count"),
          uiOutput("visited_preview")
      )
    ),
    
    column(
      8,
      div(class="map-container", leafletOutput("map", height = 420)),
      br(),
      div(class="data-container",
          h5("All Available Places"),
          DT::dataTableOutput("places_table")
      )
    )
  ),
  
  # JS toggles
  tags$script(HTML("
    $(document).on('click', '.activity-btn', function() {
      $(this).toggleClass('active');
      var a1 = $('#activity_buttons .activity-btn.active').map(function(){return $(this).text();}).get();
      Shiny.setInputValue('selected_activities', a1, {priority: 'event'});

      var a2 = $('#activity_mode_buttons .activity-btn.active').map(function(){return $(this).text();}).get();
      Shiny.setInputValue('selected_activity_modes', a2, {priority: 'event'});
    });

    $(document).on('click', '.transport-btn', function() {
      if ($(this).hasClass('active')) {
        $(this).removeClass('active');
        Shiny.setInputValue('selected_transport', '', {priority: 'event'});
      } else {
        $('.transport-btn').removeClass('active');
        $(this).addClass('active');
        Shiny.setInputValue('selected_transport', $(this).text(), {priority: 'event'});
      }
    });

    $(document).on('change', '#context_filter', function() {
      var contexts = $(this).val();
      $('#activity_mode_buttons .activity-btn').show();

      if (contexts && contexts.includes('☔ Rainy Weather')) {
        $('#activity_mode_buttons .activity-btn').each(function(){
          var t = $(this).text();
          if (t.includes('Bike Ride') || t.includes('Hiking') || t.includes('Walking Tour') || t.includes('Photography')) {
            $(this).hide().removeClass('active');
          }
        });
        var active_modes = $('#activity_mode_buttons .activity-btn.active:visible').map(function(){return $(this).text();}).get();
        Shiny.setInputValue('selected_activity_modes', active_modes, {priority: 'event'});
      }
    });
  "))
)

# ---- Coordinate helper ----
has_coords <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(rep(FALSE, 0))
  if (!all(c("lat","lng") %in% names(df))) return(rep(FALSE, nrow(df)))
  is.finite(df$lat) & is.finite(df$lng)
}

# --- Weather & time helpers (outside server) ---
get_weather_forecast <- function() {
  tryCatch({
    url <- "https://wttr.in/Portland,OR?format=%C|%t|%h|%w&u"
    response <- readLines(url, warn = FALSE)
    parts <- strsplit(response[1], "\\|")[[1]]
    if (length(parts) >= 4) {
      condition <- parts[1]
      temp <- parts[2]
      humidity <- parts[3]
      wind <- parts[4]
      is_rainy <- grepl("rain|drizzle|shower|storm", tolower(condition))
      return(list(condition=condition, temperature=temp, humidity=humidity, wind=wind, is_rainy=is_rainy, success=TRUE))
    }
    list(success=FALSE)
  }, error=function(e) list(success=FALSE))
}


# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  # ---- Weather/time (no emojis) ----
  # ---- Weather/time (no emojis) ----
  current_weather <- reactive({
    invalidateLater(15 * 60 * 1000, session)  # refresh theme + data every 15 min
    get_weather_forecast() %||% list(success = FALSE)
  })
  output$weather_ui <- renderUI({
    invalidateLater(60 * 1000, session)       # tick the clock each minute
    w <- current_weather()
    time_str <- pdx_time_string()
    if (isTRUE(w$success)) {
      tagList(
        div(class="time-display",
            div(class="time-label", "Current Time"),
            div(class="time-value", time_str)
        ),
        div(class="weather-display",
            div(class="weather-main",
                div(class="weather-condition", w$condition),
                div(class="weather-temp", w$temperature)
            ),
            div(class="weather-details",
                span(class="weather-detail", paste("Humidity:", w$humidity)),
                span(class="weather-detail", paste("Wind:", w$wind))
            )
        )
      )
    } else {
      div(class="time-display",
          div(class="time-label", "Current Time"),
          div(class="time-value", time_str)
      )
    }
  })
  
  # ---- State ----
  values <- reactiveValues(
    completed = load_completed(),
    suggested = NULL,
    inspiration_text = NULL,
    home_lat = DEFAULT_LAT,
    home_lng = DEFAULT_LNG,
    home_address = DEFAULT_ADDRESS
  )
  
  # Header: home info
  output$home_info_ui <- renderUI({
    if (!is.null(values$home_address) && nzchar(values$home_address)) {
      tags$p(sprintf("home base: %s", values$home_address), class = "homebase-line")
    } else NULL
  })
  
  # Geocoding
  observeEvent(input$geocode_address, {
    if (is.null(input$home_address) || input$home_address == "") {
      output$address_status <- renderText("Please enter an address."); return()
    }
    output$address_status <- renderText("Geocoding address...")
    result <- geocode_address(input$home_address)
    if (isTRUE(result$success)) {
      values$home_lat <- result$lat; values$home_lng <- result$lng; values$home_address <- result$formatted_address
      output$address_status <- renderText(paste("Location set:", result$formatted_address))
      showNotification("Address successfully geocoded! Distances updated.", type = "message")
    } else {
      output$address_status <- renderText(paste("Error:", result$error %||% "Geocoding failed"))
      showNotification(paste("Geocoding failed:", result$error %||% "Unknown error"), type = "error")
    }
  })
  
  # Distances only when home is set
  places_with_distances <- reactive({
    df <- places
    if (home_is_set(values$home_address, values$home_lat, values$home_lng)) {
      df$distance_mi <- mapply(function(lat, lng) calc_distance_miles(values$home_lat, values$home_lng, lat, lng), df$lat, df$lng)
    } else df$distance_mi <- NA_real_
    df
  })
  
  # Sextant helpers + selectors
  get_sextant_choices <- function(places, sections_boundaries, SEC_NAME_COL) {
    choices <- NULL
    if (!is.null(sections_boundaries) && !is.null(SEC_NAME_COL) && SEC_NAME_COL %in% names(sections_boundaries)) {
      choices <- sections_boundaries[[SEC_NAME_COL]] |> as.character()
    } else if ("section" %in% names(places)) {
      choices <- as.character(places$section)
    }
    if (is.null(choices) || !length(choices)) choices <- c("North","South","Northeast","Northwest","Southeast","Southwest")
    sort(unique(normalize_sextant(choices)))
  }
  safe_st_intersects_rows <- function(neigh_sf, sect_sf) {
    if (is.null(neigh_sf) || is.null(sect_sf) || nrow(sect_sf) == 0) return(integer(0))
    if (!is.na(sf::st_crs(sect_sf)) && !is.na(sf::st_crs(neigh_sf)) && sf::st_crs(sect_sf) != sf::st_crs(neigh_sf)) {
      neigh_sf <- sf::st_transform(neigh_sf, sf::st_crs(sect_sf))
    }
    suppressWarnings({ idx <- sf::st_intersects(neigh_sf, sect_sf, sparse = FALSE) })
    which(rowSums(idx) > 0)
  }
  output$section_selector <- renderUI({
    sextant_choices <- get_sextant_choices(places, sections_boundaries, SEC_NAME_COL)
    selectizeInput("section_filter", "", choices = sextant_choices, selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Choose Quadrant(s)"))
  })
  
  # Map clicks → selectors
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (is.null(click$id) || is.null(click$group)) return(NULL)
    if (identical(click$group, "sextants")) {
      raw_name <- sub("^sextant::", "", click$id)
      sec_name <- normalize_sextant(raw_name)
      sx_choices <- get_sextant_choices(places, sections_boundaries, SEC_NAME_COL)
      if (!sec_name %in% sx_choices) { showNotification(paste("Unrecognized Quadrant:", raw_name), type = "warning"); return(NULL) }
      cur <- isolate(input$section_filter); if (is.null(cur)) cur <- character(0)
      cur <- normalize_sextant(cur)
      if (sec_name %in% cur) cur <- setdiff(cur, sec_name) else cur <- unique(c(cur, sec_name))
      updateSelectizeInput(session, "section_filter", choices = sx_choices, selected = cur, server = TRUE)
      updateSelectizeInput(session, "neighborhood_filter", choices = character(0), selected = character(0), server = TRUE)
      showNotification(paste0("🧭 Quadrant: ", if (length(cur)) paste(cur, collapse = ", ") else "Any"), type = "message")
      return(invisible(NULL))
    }
    if (identical(click$group, "neighborhoods")) {
      if (is.null(isolate(input$section_filter)) || !length(isolate(input$section_filter))) {
        showNotification("Select a Quadrant first to choose Neighborhoods.", type = "message"); return(NULL)
      }
      nb <- sub("^neigh::", "", click$id)
      cur <- isolate(input$neighborhood_filter); if (is.null(cur)) cur <- character(0)
      if (nb %in% cur) cur <- setdiff(cur, nb) else cur <- unique(c(cur, nb))
      updateSelectizeInput(session, "neighborhood_filter", selected = cur, server = TRUE)
      showNotification(paste0("🏘️ Neighborhood: ", if (length(cur)) paste(cur, collapse = ", ") else "Any"), type = "message")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$section_filter, {
    updateSelectizeInput(session, "neighborhood_filter", selected = character(0))
  }, ignoreInit = TRUE)
  
  # Neighborhood selector based on selected Sextants
  output$neighborhood_selector <- renderUI({
    if (is.null(input$section_filter) || !length(input$section_filter) ||
        is.null(sections_boundaries) || is.null(SEC_NAME_COL) ||
        is.null(neighborhood_boundaries) || is.null(NEI_NAME_COL)) {
      return(selectizeInput("neighborhood_filter", "", choices = character(0), selected = NULL, multiple = TRUE,
                            options = list(placeholder = "Select a Quadrant first")))
    }
    sel_secs <- sections_boundaries[sections_boundaries[[SEC_NAME_COL]] %in% normalize_sextant(input$section_filter), , drop = FALSE]
    rows_to_draw <- safe_st_intersects_rows(neighborhood_boundaries, sel_secs)
    available_neighborhoods <- if (length(rows_to_draw)) {
      neighborhood_boundaries[rows_to_draw, ][[NEI_NAME_COL]] |> as.character() |> unique() |> sort()
    } else character(0)
    selectizeInput("neighborhood_filter", "", choices = available_neighborhoods, selected = NULL, multiple = TRUE,
                   options = list(placeholder = if (length(available_neighborhoods)) "Choose Neighborhoods (optional)" else "No Neighborhoods in the selected Quadrant(s)"))
  })
  
  # Filtering pipeline
  filtered_places <- reactive({
    df <- places_with_distances()
    df$neigh_disp <- neigh_display_vec(df$neighborhood_geo, df$neighborhood)
    
    # Quadrants
    if (!is.null(input$section_filter) && length(input$section_filter) > 0 && ("section_geo" %in% names(df) || "section" %in% names(df))) {
      wanted <- normalize_sextant(input$section_filter)
      if ("section_geo" %in% names(df)) {
        df <- df[!is.na(df$section_geo) & df$section_geo %in% wanted, , drop = FALSE]
      } else {
        df$section_norm <- normalize_sextant(df$section)
        df <- df[!is.na(df$section_norm) & df$section_norm %in% wanted, , drop = FALSE]
      }
    }
    # Neighborhoods
    if (!is.null(input$neighborhood_filter) && length(input$neighborhood_filter) > 0) {
      df <- df[!is.na(df$neigh_disp) & df$neigh_disp %in% input$neighborhood_filter, , drop = FALSE]
    }
    # Activity categories
    if (!is.null(input$selected_activities) && length(input$selected_activities) > 0) {
      activity_match <- rep(FALSE, nrow(df))
      for (activity in input$selected_activities) {
        if (activity %in% names(ACTIVITY_CATEGORIES)) {
          matches <- sapply(df$tags, function(tags) matches_activity(tags, ACTIVITY_CATEGORIES[[activity]]))
          activity_match <- activity_match | matches
        }
      }
      df <- df[activity_match, , drop = FALSE]
    }
    # Activity modes
    if (!is.null(input$selected_activity_modes) && length(input$selected_activity_modes) > 0) {
      mode_match <- rep(FALSE, nrow(df))
      for (mode in input$selected_activity_modes) {
        if (mode %in% names(ACTIVITY_MODES)) {
          matches <- mapply(function(title, tags, note) {
            matches_activity_mode(title, tags, note, ACTIVITY_MODES[[mode]])
          }, df$title, df$tags, df$note)
          mode_match <- mode_match | matches
        }
      }
      df <- df[mode_match, , drop = FALSE]
    }
    # Context filters
    if (!is.null(input$context_filter) && length(input$context_filter) > 0) {
      for (context_name in input$context_filter) {
        context <- CONTEXT_FILTERS[[context_name]]; if (is.null(context)) next
        if ("exclude_activities" %in% names(context)) {
          for (ex in context$exclude_activities) if (ex %in% names(ACTIVITY_MODES)) {
            terms <- ACTIVITY_MODES[[ex]]
            exm <- mapply(function(title, tags, note) matches_activity_mode(title, tags, note, terms),
                          df$title, df$tags, df$note)
            df <- df[!exm, , drop = FALSE]
          }
        }
        if ("exclude_venues" %in% names(context)) {
          for (ex in context$exclude_venues) if (ex %in% names(ACTIVITY_CATEGORIES)) {
            terms <- ACTIVITY_CATEGORIES[[ex]]
            exm <- sapply(df$tags, function(tags) matches_activity(tags, terms))
            df <- df[!exm, , drop = FALSE]
          }
        }
        if (home_is_set(values$home_address, values$home_lat, values$home_lng) &&
            "max_distance" %in% names(context)) {
          df <- df[!is.na(df$distance_mi) & df$distance_mi <= context$max_distance, , drop = FALSE]
        }
      }
    }
    # Transport constraints
    if (home_is_set(values$home_address, values$home_lat, values$home_lng) &&
        !is.null(input$selected_transport) && nzchar(input$selected_transport)) {
      transport_text <- input$selected_transport
      for (mode in names(TRANSPORT_MODES)) {
        if (stringr::str_detect(transport_text, stringr::fixed(stringr::str_sub(mode, 1, 10)))) {
          max_dist <- TRANSPORT_MODES[[mode]]
          if (max_dist < 999) df <- df[!is.na(df$distance_mi) & df$distance_mi <= max_dist, , drop = FALSE]
          break
        }
      }
    }
    # Free keywords
    if (!is.null(input$keyword_filter) && nzchar(input$keyword_filter)) {
      keywords <- stringr::str_split(input$keyword_filter, ",")[[1]] |> stringr::str_trim()
      for (kw in keywords[keywords != ""]) {
        kw_match <- stringr::str_detect(tolower(paste(df$title, df$tags, df$note)), stringr::fixed(tolower(kw)))
        df <- df[kw_match, , drop = FALSE]
      }
    }
    df
  })
  
  available_places <- reactive({
    df <- filtered_places()
    df[!(df$id %in% values$completed), , drop = FALSE]
  })
  
  # Suggestions
  observeEvent(input$suggest_place, {
    candidates <- available_places()
    if (nrow(candidates) == 0) { showNotification("No unvisited places match your criteria!", type = "warning"); values$suggested <- NULL; return() }
    context_for_plan <- if (length(input$context_filter) > 0) input$context_filter[1] else NULL
    plans <- generate_day_plan(candidates, context_for_plan, input$selected_activities, input$selected_activity_modes, input$time_filter)
    if (length(plans) > 0 && !is.null(plans[[1]])) {
      plan <- plans[[1]]
      values$suggested <- plan$places[1, , drop = FALSE]
      values$inspiration_text <- list(title = plan$title, description = plan$description, type = plan$type, estimated_time = plan$estimated_time)
    } else {
      values$suggested <- candidates[sample(nrow(candidates), 1), , drop = FALSE]
      values$inspiration_text <- NULL
    }
  })
  
  observeEvent(input$random_inspiration, {
    all_available <- places[!(places$id %in% values$completed), , drop = FALSE]
    if (nrow(all_available) == 0) { showNotification("You've visited everywhere! Time for new places.", type = "warning"); values$suggested <- NULL; return() }
    context_for_adventure <- if (length(input$context_filter) > 0) input$context_filter[1] else NULL
    adventures <- generate_surprise_adventure(
      available_places = all_available,
      time_available = input$time_filter,
      context = context_for_adventure,
      home_lat = values$home_lat, home_lng = values$home_lng, home_addr = values$home_address
    )
    if (length(adventures) > 0 && !is.null(adventures[[1]])) {
      adventure <- adventures[[1]]
      if (!is.null(adventure$places) && nrow(adventure$places) > 0) {
        values$suggested <- adventure$places[1, , drop = FALSE]
        values$inspiration_text <- list(
          title = adventure$title %||% "Adventure",
          description = adventure$description %||% "Explore something new!",
          type = adventure$type %||% "adventure",
          estimated_time = adventure$estimated_time %||% "1-2 hours",
          transit = adventure$transit %||% "Walk",
          neighborhood = adventure$neighborhood %||% "Portland"
        )
      } else adventure <- NULL
    } else {
      adventure <- NULL
    }
    
    if (is.null(adventure)) {
      interesting_places <- all_available[
        has_coords(all_available) &
          grepl("coffee|vintage|record|bookstore|gallery|museum|brewery",
                all_available$tags, ignore.case = TRUE),
        , drop = FALSE]
      if (nrow(interesting_places) > 0) {
        chosen_place <- interesting_places[sample(nrow(interesting_places), 1), , drop = FALSE]
        if (nrow(chosen_place) > 0 && !is.null(chosen_place$title) && nzchar(chosen_place$title[1])) {
          activities <- c("explore", "check out", "wander to")
          activity <- sample(activities, 1)
          values$suggested <- chosen_place
          values$inspiration_text <- list(
            title = "Mystery Adventure",
            description = paste(activity, chosen_place$title[1], "and try something new!"),
            type = "adventure",
            estimated_time = "1-2 hours"
          )
        } else interesting_places <- data.frame()
      }
      if (nrow(interesting_places) == 0) {
        coord_ok <- has_coords(all_available)
        if (sum(coord_ok) == 0) { showNotification("No mappable places have coordinates — adjust filters or set your address.", type = "warning"); values$suggested <- NULL; return(invisible(NULL)) }
        safe_places <- all_available[coord_ok, , drop = FALSE]
        values$suggested <- safe_places[sample(nrow(safe_places), 1), , drop = FALSE]
        values$inspiration_text <- list(
          title = "🎲 Random Adventure",
          description = "Go explore this place and see what happens!",
          type = "random",
          estimated_time = "1-3 hours"
        )
      }
    }
  })
  
  # Visited toggles
  observeEvent(input$mark_visited, {
    if (is.null(values$suggested)) { showNotification("Please suggest a place first!", type = "message") }
    else {
      values$completed <- unique(c(values$completed, values$suggested$id))
      save_completed(values$completed)
      showNotification(paste("✅", values$suggested$title, "marked as visited!"), type = "success")
    }
  })
  observeEvent(input$unmark, {
    if (is.null(values$suggested)) { showNotification("Please suggest a place first!", type = "message") }
    else {
      values$completed <- setdiff(values$completed, values$suggested$id)
      save_completed(values$completed)
      showNotification(paste("↩️", values$suggested$title, "unmarked!"), type = "success")
    }
  })
  observeEvent(input$reset_visited, {
    values$completed <- character(0); save_completed(values$completed)
    showNotification("♻️ All visited places reset!", type = "info")
  })
  
  # ---- MAP: panes so markers are above polygons (clickable) ----
  output$map <- renderLeaflet({
    center_lat <- if (!is.na(values$home_lat)) values$home_lat else 45.5152
    center_lng <- if (!is.na(values$home_lng)) values$home_lng else -122.6784
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMapPane("polygons", zIndex = 410) %>%
      addMapPane("markers",  zIndex = 420) %>%
      setView(lng = center_lng, lat = center_lat, zoom = 11)
  })
  
  # Layers
  observe({
    filtered <- filtered_places()
    visited <- places[places$id %in% values$completed, , drop = FALSE]
    suggested <- values$suggested
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    
    # Sextant polygons
    if (!is.null(sections_boundaries) && !is.null(SEC_NAME_COL) && SEC_NAME_COL %in% names(sections_boundaries)) {
      secs_all <- sections_boundaries
      sx_names_raw <- as.character(secs_all[[SEC_NAME_COL]]); sx_names <- normalize_sextant(sx_names_raw)
      selected <- isolate(input$section_filter); selected <- if (is.null(selected)) character(0) else normalize_sextant(selected)
      section_colors <- c("Southwest"="#ff6b6b","Northwest"="#4ecdc4","Southeast"="#45b7d1","Northeast"="#96ceb4","North"="#feca57","South"="#fd79a8","Other"="#a29bfe")
      for (i in seq_along(sx_names)) {
        sx <- sx_names[i]; is_selected <- sx %in% selected
        base_color <- section_colors[[sx]] %||% section_colors[["Other"]]
        proxy <- proxy %>% addPolygons(
          data = secs_all[i, ],
          fillColor = base_color, fillOpacity = if (is_selected) 0.08 else 0.05,
          color = base_color, weight = if (is_selected) 2.5 else 1.5, opacity = if (is_selected) 0.9 else 0.7,
          group = "sextants", layerId = paste0("sextant::", sx),
          options = pathOptions(pane = "polygons", interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
          highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.25, bringToFront = FALSE),
          popup = paste0("<b>", sx, "</b><br>", if (is_selected) "Click to remove" else "Click to add")
        )
      }
    }
    
    # Neighborhood polygons (only when Sextant selected)
    selected_sx <- input$section_filter
    if (!is.null(selected_sx) && length(selected_sx) > 0 &&
        !is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL) &&
        !is.null(sections_boundaries) && !is.null(SEC_NAME_COL)) {
      sel_secs <- sections_boundaries[sections_boundaries[[SEC_NAME_COL]] %in% normalize_sextant(selected_sx), , drop = FALSE]
      rows_to_draw <- safe_st_intersects_rows(neighborhood_boundaries, sel_secs)
      if (length(rows_to_draw) > 0) {
        neigh_to_draw <- neighborhood_boundaries[rows_to_draw, , drop = FALSE]
        nb_names <- as.character(neigh_to_draw[[NEI_NAME_COL]])
        selected_nb <- isolate(input$neighborhood_filter); if (is.null(selected_nb)) selected_nb <- character(0)
        for (i in seq_len(nrow(neigh_to_draw))) {
          nb_name <- nb_names[i]; is_selected <- nb_name %in% selected_nb
          proxy <- proxy %>% addPolygons(
            data = neigh_to_draw[i, ],
            fillColor = if (is_selected) "#10b981" else "#ccfbf1",
            fillOpacity = if (is_selected) 0.06 else 0.01,
            color = if (is_selected) "#10b981" else "#0ea5a8",
            weight = if (is_selected) 2 else 1.2,
            opacity = if (is_selected) 1 else 0.6,
            group = "neighborhoods", layerId = paste0("neigh::", nb_name),
            options = pathOptions(pane = "polygons", interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
            highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.25, bringToFront = TRUE),
            popup = paste0("<b>", nb_name, "</b><br>", if (is_selected) "Click to remove" else "Click to add")
          )
        }
      }
    }
    
    # Home marker
    if (home_is_set(values$home_address, values$home_lat, values$home_lng)) {
      proxy <- proxy %>% addCircleMarkers(
        lng = values$home_lng, lat = values$home_lat, label = "Home", popup = values$home_address,
        radius = 8, color = "#16a34a", fillColor = "#16a34a", opacity = 1, fillOpacity = 0.9,
        options = pathOptions(pane = "markers"), layerId = "home_marker"
      )
    }
    
    # Filtered places (markers on top)
    fp <- filtered[has_coords(filtered), , drop = FALSE]
    if (nrow(fp) > 0) {
      dist_txt <- ifelse(is.na(fp$distance_mi), "", paste0("<br>", round(fp$distance_mi, 1), " miles from home"))
      proxy <- proxy %>% addCircleMarkers(
        lng = fp$lng, lat = fp$lat, radius = 6,
        color = "#e08a63", fillColor = "#fef7f4", opacity = 0.95, fillOpacity = 0.85,
        popup = paste0("<b>", fp$title, "</b><br>", fp$tags, dist_txt),
        options = pathOptions(pane = "markers")
      )
    }
    
    # Visited
    if (nrow(visited) > 0) {
      proxy <- proxy %>% addCircleMarkers(
        lng = visited$lng, lat = visited$lat, radius = 5,
        color = "#9ca3af", fillColor = "#9ca3af", opacity = 0.95, fillOpacity = 0.75,
        options = pathOptions(pane = "markers")
      )
    }
    
    # Suggested highlight
    if (!is.null(suggested) && nrow(suggested) > 0 && has_coords(suggested)[1]) {
      proxy <- proxy %>% addCircleMarkers(
        lng = suggested$lng, lat = suggested$lat, radius = 12,
        color = "#e8a083", fillColor = "#e8a083", opacity = 1, fillOpacity = 0.9,
        options = pathOptions(pane = "markers")
      ) %>% setView(lng = suggested$lng, lat = suggested$lat, zoom = 15)
    }
  })
  
  # Auto-apply rainy context if detected and none chosen
  observe({
    weather <- current_weather()
    if (!is.null(weather) && isTRUE(weather$is_rainy) &&
        (is.null(input$context_filter) || length(input$context_filter) == 0)) {
      updateSelectizeInput(session, "context_filter", selected = "☔ Rainy Weather")
      showNotification("☔ Rainy weather detected - filtering to indoor activities", type = "message")
    }
  })
  
  # Hero suggestion display
  output$hero_suggestion_display <- renderUI({
    if (is.null(values$suggested)) {
      div(class = "hero-empty",
          h2("🎯 What should you do today?"),
          p("Hit 'Surprise Me' for a spontaneous adventure, or use 'Get Suggestions' for a tailored plan!"),
          p("Set your preferences on the left to get personalized recommendations.")
      )
    } else {
      place <- values$suggested
      clean_tags_display <- clean_tags(place$tags)
      nb_disp <- neigh_display_vec(place$neighborhood_geo, place$neighborhood)
      if (!is.null(values$inspiration_text) && is.list(values$inspiration_text)) {
        div(class = "hero-card",
            h2(values$inspiration_text$title),
            div(class = "description", values$inspiration_text$description),
            div(class = "details",
                if (!is.na(place$distance_mi)) div(class = "detail-chip", paste(round(place$distance_mi, 1), "miles away")),
                if (!is.na(nb_disp)) div(class = "detail-chip", nb_disp),
                if (!is.null(values$inspiration_text$estimated_time)) div(class = "detail-chip", paste("⏱️", values$inspiration_text$estimated_time)),
                if (nzchar(place$url)) tags$a("📍 View on Map", href = place$url, target = "_blank", class = "detail-chip", style = "color: var(--text); text-decoration: none;")
            )
        )
      } else {
        div(class = "hero-card",
            h2("✨ ", place$title),
            div(class = "description", if (nzchar(clean_tags_display)) clean_tags_display else "A great spot to explore!"),
            div(class = "details",
                if (!is.na(place$distance_mi)) div(class = "detail-chip", paste(round(place$distance_mi, 1), "miles away")),
                if (!is.na(nb_disp)) div(class = "detail-chip", nb_disp),
                if (nzchar(place$url)) tags$a("📍 View on Map", href = place$url, target = "_blank", class = "detail-chip", style = "color: var(--text); text-decoration: none;")
            )
        )
      }
    }
  })
  
  # Table + visited preview
  output$places_table <- DT::renderDataTable({
    df <- filtered_places()
    if (nrow(df) == 0) return(NULL)
    df$clean_tags <- sapply(df$tags, clean_tags)
    df$neighborhood_display <- neigh_display_vec(df$neighborhood_geo, df$neighborhood)
    display_df <- df %>%
      dplyr::mutate(distance_mi = round(distance_mi, 1)) %>%
      dplyr::select(dplyr::any_of(c("title", "clean_tags", "feature", "neighborhood_display", "distance_mi"))) %>%
      dplyr::rename(Place = title, Tags = clean_tags, Neighborhood = neighborhood_display, `Distance (mi)` = distance_mi)
    if ("feature" %in% names(df)) names(display_df)[names(display_df) == "feature"] <- "Feature"
    DT::datatable(display_df, options = list(pageLength = 8, scrollX = TRUE, dom = 'tip'))
  })
  output$visited_count <- renderText({ paste("Visited:", length(values$completed), "places") })
  output$visited_preview <- renderUI({
    if (length(values$completed) == 0) {
      div("None yet - start exploring!")
    } else {
      visited_places <- places[places$id %in% values$completed, , drop = FALSE]
      recent <- head(visited_places$title, 5)
      div(
        lapply(recent, function(name) {
          tags$span(
            style = "background: #dcfce7; padding: 2px 6px; margin: 2px; border-radius: 6px; font-size: 11px; display: inline-block;",
            name
          )
        }),
        if (length(values$completed) > 5) p(paste("...", length(values$completed) - 5, "more"))

  )
}
})
}

# ---------------- LAUNCH ----------------
shinyApp(ui, server)





