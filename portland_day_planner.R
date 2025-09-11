# portland_day_planner.R — Portland Day-Off Planner
# Upgraded: distance only after geocode, radius-aware adventures, polygon neighborhoods

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

# --- App dir + header image ------------------------------------------
get_app_dir <- function() {
  tryCatch(
    normalizePath(dirname(sys.frame(1)$ofile)),
    error = function(e) normalizePath(getwd())
  )
}
APP_DIR <- get_app_dir()
HEADER_IMG <- tryCatch({
  img_path <- file.path(APP_DIR, "www:", "pdx_header.png")
  if (file.exists(img_path)) base64enc::dataURI(file = img_path, mime = "image/png") else NULL
}, error = function(e) NULL)

# --- Defaults: start blank until geocoded ----------------------------
DEFAULT_ADDRESS <- ""
DEFAULT_LAT <- NA_real_
DEFAULT_LNG <- NA_real_

# --- Weather & time --------------------------------------------------
get_weather_forecast <- function() {
  tryCatch({
    url <- "http://wttr.in/Portland,OR?format=%C|%t|%h|%w&u"
    response <- readLines(url, warn = FALSE)
    parts <- strsplit(response[1], "\\|")[[1]]
    if (length(parts) >= 4) {
      condition <- parts[1]
      temp <- parts[2]
      humidity <- parts[3]
      wind <- parts[4]
      is_rainy <- grepl("rain|drizzle|shower|storm", tolower(condition))
      return(list(condition=condition,temperature=temp,humidity=humidity,
                  wind=wind,is_rainy=is_rainy,success=TRUE))
    }
    list(success=FALSE)
  }, error=function(e) list(success=FALSE))
}
pdx_time_string <- function() {
  format(as.POSIXct(Sys.time(), tz="America/Los_Angeles"), "%a %b %d, %I:%M %p")
}

# --- Geo helpers for neighborhoods & sextants ------------------------
home_is_set <- function(addr, lat, lng) {
  !is.null(addr) && nzchar(addr) && !is.na(lat) && !is.na(lng)
}

geo_enrich_places <- function(df, neighborhoods_sf, nei_col, sextants_sf, sec_col) {
  df$neighborhood_geo <- NA_character_
  df$section_geo <- NA_character_
  if (nrow(df) == 0) return(df)
  pts <- tryCatch(st_as_sf(df, coords=c("lng","lat"), crs=4326, remove=FALSE),
                  error=function(e) NULL)
  if (!is.null(pts) && !is.null(neighborhoods_sf) && !is.null(nei_col)) {
    nb <- neighborhoods_sf
    if (!is.na(st_crs(nb)) && st_crs(nb)$epsg != 4326) nb <- st_transform(nb,4326)
    joined_nb <- suppressWarnings(st_join(pts, nb[,nei_col,drop=FALSE], join=st_within, left=TRUE))
    df$neighborhood_geo <- as.character(joined_nb[[nei_col]])
  }
  if (!is.null(pts) && !is.null(sextants_sf) && !is.null(sec_col)) {
    sx <- sextants_sf
    if (!is.na(st_crs(sx)) && st_crs(sx)$epsg != 4326) sx <- st_transform(sx,4326)
    joined_sx <- suppressWarnings(st_join(pts, sx[,sec_col,drop=FALSE], join=st_within, left=TRUE))
    raw_sec <- as.character(joined_sx[[sec_col]])
    if (exists("normalize_sextant")) {
      df$section_geo <- normalize_sextant(raw_sec)
    } else df$section_geo <- raw_sec
  }
  df
}
neigh_display_vec <- function(geo, txt) {
  ifelse(!is.na(geo) & nzchar(geo), geo, txt)
}
label_with_neigh <- function(title, neigh) {
  if (!is.na(neigh) && nzchar(neigh)) paste0(title, " (", neigh, ")") else title
}
is_within_radius <- function(lat1,lng1,lat2,lng2,radius_miles=1) {
  d <- calc_distance_miles(lat1,lng1,lat2,lng2)
  !is.na(d) && d <= radius_miles
}

# --- DATA LOAD -------------------------------------------------------
processed_file <- "data/portland_places_processed.rds"
if (!file.exists(processed_file)) stop("❌ Missing processed data. Run process_data.R first.")
places <- readRDS(processed_file)
if (!is.data.frame(places) || nrow(places)==0) stop("❌ Invalid processed data.")
if ("title" %in% names(places)) {
  places <- places[!grepl("^Unnamed place", places$title, ignore.case=TRUE), ]
}

# Map boundaries
load_map_file <- function(paths) { for (p in paths) if (file.exists(p)) return(p); NULL }
safe_read_sf <- function(path) {
  if (is.null(path)) return(NULL)
  tryCatch(st_read(path, quiet=TRUE), error=function(e) NULL)
}
pick_name_col <- function(sfobj, cands) {
  cands <- cands[cands %in% names(sfobj)]
  if (length(cands)) cands[[1]] else NULL
}
sextants_path <- load_map_file(c(
  "archive/Portland_Administrative_Sextants.geojson",
  "data/Portland_Administrative_Sextants.geojson"))
sections_boundaries <- safe_read_sf(sextants_path)
SEC_NAME_COL <- if (!is.null(sections_boundaries)) pick_name_col(sections_boundaries,
                                                                 c("Sextant","SEXTANT","PREFIX","NAME")) else NULL
neighborhood_path <- load_map_file(c(
  "archive/Neighborhood_Boundaries.geojson",
  "archive/neighborhoods.geojson",
  "data/Neighborhood_Boundaries.geojson"))
neighborhood_boundaries <- safe_read_sf(neighborhood_path)
NEI_NAME_COL <- if (!is.null(neighborhood_boundaries)) pick_name_col(neighborhood_boundaries,
                                                                     c("MAPLABEL","NAME","Label","Neighborhood","NEIGHBORHD","neigh","label")) else NULL

# Enrich places with polygon-derived labels
places <- geo_enrich_places(places, neighborhood_boundaries, NEI_NAME_COL,
                            sections_boundaries, SEC_NAME_COL)

# =========================
# Part 2 — Helpers, Generators, UI
# =========================

# ---- Remaining helpers ----
has_coords <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(rep(FALSE, 0))
  is.finite(df$lat) & is.finite(df$lng)
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

matches_activity <- function(tags_text, category_terms) {
  if (is.na(tags_text) || tags_text == "") return(FALSE)
  tags_lower <- tolower(tags_text)
  any(vapply(category_terms, function(term) {
    clean_term <- gsub("[^A-Za-z ]", "", term) |> str_trim() |> tolower()
    if (clean_term != "") stringr::str_detect(tags_lower, fixed(clean_term)) else FALSE
  }, logical(1)))
}
matches_activity_mode <- function(title, tags, note, mode_terms) {
  if (is.na(title)) title <- ""; if (is.na(tags)) tags <- ""; if (is.na(note)) note <- ""
  combined_text <- tolower(paste(title, tags, note))
  any(vapply(mode_terms, function(term) {
    clean_term <- gsub("[^A-Za-z ]", "", term) |> str_trim() |> tolower()
    stringr::str_detect(combined_text, fixed(clean_term))
  }, logical(1)))
}
clean_tags <- function(tags_text) {
  if (is.na(tags_text) || tags_text == "") return("")
  cleaned <- gsub("[^A-Za-z0-9 .,!?()-]", " ", tags_text)
  cleaned <- str_squish(cleaned)
  words <- strsplit(cleaned, " ")[[1]]
  words <- words[nchar(words) > 2 | words %in% c("&", "or", "of")]
  paste(words, collapse = " ")
}

# “On the way back” heuristic: candidate is closer to home than venue1 and
# v1 -> candidate -> home is ≤ 25% longer than v1 -> home.
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

# ---- Activity taxonomy ----
ACTIVITY_CATEGORIES <- list(
  "☕️ Coffee & Cafes" = c("coffee","cafe","espresso","latte"),
  "🍰 Sweet Treats" = c("sweet treats","bagels","dessert","bakery","donut","pastry"),
  "🍃 Food & Restaurants" = c("vegan","breakfast","restaurant","food","lunch","dinner","brunch","food cart"),
  "🍸 Drinks & Bars" = c("beer","cocktails","bar","brewery","wine","spirits"),
  "📚 Bookstores" = c("bookstore","books","reading","literature"),
  "💽 Music & Records" = c("record stores","music","records","vinyl","cd"),
  "🏷️ Thrift & Vintage" = c("thrift","vintage","antique","secondhand"),
  "📔 Stationery & Art" = c("stationery","art supplies","paper","pens","notebooks"),
  "🛴 Fun Shopping" = c("toys","games","gifts","novelty"),
  "🌲 Parks & Nature" = c("park","garden","nature","trail","hike","outdoor","forest"),
  "🥕 Markets" = c("market","farmers","produce","fresh food","grocery"),
  "🎭 Entertainment" = c("movies","theater","cinema","show","performance"),
  "🖋️ Creative" = c("tattoo","art studio","art")
)
TRANSPORT_MODES <- list("🚶 Walk"=2, "🚲 Bicycle Rights!"=10, "🚌 Public Transit"=15, "🚗 Drive"=30, "🌍 Any Distance"=999)
ACTIVITY_MODES <- list(
  "📖 Reading"=c("coffee","cafe","bookstore","quiet","library","park"),
  "🎨 Drawing/Sketching"=c("coffee","cafe","park","outdoor","scenic","garden","museum"),
  "🚲 Bike Ride"=c("trail","path","route","bike path","greenway"),
  "🥾 Hiking"=c("trail","hike","nature","forest","mountain","waterfall"),
  "📸 Photography"=c("scenic","architecture","vintage","historic","art","bridge","view","mural"),
  "🚶 Walking Tour"=c("neighborhood","historic","architecture","street art","district"),
  "🛍️ Shopping Spree"=c("thrift","vintage","bookstore","record","shopping","market"),
  "🍽️ Food Adventure"=c("restaurant","food cart","market","bakery","brewery","cafe")
)
CONTEXT_FILTERS <- list(
  "☔ Rainy Weather"=list(
    exclude_activities=c("🚲 Bike Ride","🥾 Hiking","🚶 Walking Tour","📸 Photography"),
    exclude_venues=c("🌲 Parks & Nature"),
    hide_activity_buttons=c("🚲 Bike Ride","🥾 Hiking","🚶 Walking Tour","📸 Photography"),
    prefer_close=TRUE, max_distance=3, suggestion_prefix="Stay dry with indoor activities:"
  ),
  "😴 Low Energy"=list(
    exclude_activities=c("🚲 Bike Ride","🥾 Hiking"),
    prefer_activities=c("📖 Reading","🎨 Drawing/Sketching"),
    prefer_venues=c("☕️ Coffee & Cafes","📚 Bookstores"),
    prefer_close=TRUE, max_distance=2, suggestion_prefix="Take it easy with relaxing activities:"
  ),
  "🌞 Perfect Weather"=list(
    prefer_activities=c("🚲 Bike Ride","🥾 Hiking","📸 Photography","🚶 Walking Tour"),
    prefer_venues=c("🌲 Parks & Nature"), boost_outdoor=TRUE,
    suggestion_prefix="Beautiful day for outdoor adventures:"
  ),
  "🎯 Focused Mission"=list(
    prefer_close=TRUE, max_distance=5,
    suggestion_prefix="Let's find exactly what you're looking for:"
  )
)

# ---- Sextant name normalization (used by joins/UI) ----
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

# ---- Plan generators ----
generate_day_plan <- function(available_places, context=NULL, selected_activities=NULL, selected_modes=NULL, time_available="quick") {
  if (nrow(available_places) == 0) return(list())
  
  # Prefer polygon-derived neighborhood labels
  available_places$neigh_disp <- neigh_display_vec(available_places$neighborhood_geo, available_places$neighborhood)
  places_with_neighborhoods <- available_places[!is.na(available_places$neigh_disp) & nzchar(available_places$neigh_disp), , drop = FALSE]
  if (nrow(places_with_neighborhoods) == 0) return(list())
  
  chosen_neighborhood <- sample(unique(places_with_neighborhoods$neigh_disp), 1)
  neighborhood_places <- places_with_neighborhoods[places_with_neighborhoods$neigh_disp == chosen_neighborhood, , drop = FALSE]
  avg_distance <- mean(neighborhood_places$distance_mi, na.rm = TRUE)
  
  force_drive <- FALSE; max_allowed_distance <- Inf
  if (!is.null(context)) {
    context_config <- CONTEXT_FILTERS[[context]]
    if (!is.null(context_config)) {
      if (context == "☔ Rainy Weather") {
        force_drive <- TRUE; max_allowed_distance <- context_config$max_distance %||% 3
      } else if (context == "😴 Low Energy") {
        if (!is.na(avg_distance) && avg_distance > 1) force_drive <- TRUE
        max_allowed_distance <- context_config$max_distance %||% 2
      } else if ("max_distance" %in% names(context_config)) {
        max_allowed_distance <- context_config$max_distance
      }
    }
  }
  if (force_drive || (!is.infinite(max_allowed_distance) && !is.na(avg_distance) && avg_distance > max_allowed_distance)) {
    transit_mode <- "🚗 Drive"
  } else if (!is.na(avg_distance) && avg_distance <= 2) {
    transit_mode <- "🚶 Walk"
  } else if (!is.na(avg_distance) && avg_distance <= 8 && context != "😴 Low Energy") {
    transit_mode <- "🚲 Bike"
  } else {
    transit_mode <- "🚗 Drive"
  }
  
  activity_plan <- if (!is.null(selected_modes) && length(selected_modes) > 0) {
    mode <- sample(selected_modes, 1)
    if (mode == "📖 Reading") {
      reading_spots <- neighborhood_places[grepl("coffee|cafe|bookstore|library|park", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      if (nrow(reading_spots) > 0) {
        chosen_spot <- reading_spots[sample(nrow(reading_spots), 1), , drop = FALSE]
        list(activity="reading", venue=chosen_spot$title, description=paste("go to", chosen_spot$title, "and read"), places=chosen_spot)
      } else list(activity="reading", venue="a quiet spot", description="find a quiet spot and read", places=neighborhood_places[1, , drop = FALSE])
    } else if (mode == "🎨 Drawing/Sketching") {
      scenic_spots <- neighborhood_places[grepl("park|coffee|cafe|garden|scenic|view", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      if (nrow(scenic_spots) > 0) {
        chosen_spot <- scenic_spots[sample(nrow(scenic_spots), 1), , drop = FALSE]
        list(activity="sketching", venue=chosen_spot$title, description=paste("go to", chosen_spot$title, "and sketch"), places=chosen_spot)
      } else list(activity="sketching", venue="a scenic spot", description="find a scenic spot and sketch", places=neighborhood_places[1, , drop = FALSE])
    } else if (mode == "🛍️ Shopping Spree") {
      shopping_spots <- neighborhood_places[grepl("thrift|vintage|bookstore|record|market|shop", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      if (nrow(shopping_spots) > 0) {
        list(activity="shopping",
             venue=if (nrow(shopping_spots) > 1) "multiple shops" else shopping_spots$title[1],
             description=paste("browse", if (nrow(shopping_spots) > 1) "the shops" else shopping_spots$title[1]),
             places=shopping_spots)
      } else list(activity="exploring", venue="the area", description="wander and explore", places=neighborhood_places[1, , drop = FALSE])
    } else if (mode == "📸 Photography") {
      photo_spots <- neighborhood_places[grepl("park|bridge|view|historic|architecture|mural|art", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      if (nrow(photo_spots) > 0) {
        chosen_spot <- photo_spots[sample(nrow(photo_spots), 1), , drop = FALSE]
        list(activity="photography", venue=chosen_spot$title, description=paste("go to", chosen_spot$title, "and take photos"), places=chosen_spot)
      } else list(activity="photography", venue="around the neighborhood", description="walk around and take photos", places=neighborhood_places[1, , drop = FALSE])
    } else {
      list(activity="exploring", venue="the area", description="explore what's available", places=neighborhood_places[1, , drop = FALSE])
    }
  } else {
    if (any(grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE))) {
      cafe_spots <- neighborhood_places[grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      chosen_cafe <- cafe_spots[sample(nrow(cafe_spots), 1), , drop = FALSE]
      list(activity="coffee and reading", venue=chosen_cafe$title, description=paste("get coffee at", chosen_cafe$title, "and read"), places=chosen_cafe)
    } else if (any(grepl("bookstore", neighborhood_places$tags, ignore.case = TRUE))) {
      bookstore <- neighborhood_places[grepl("bookstore", neighborhood_places$tags, ignore.case = TRUE), ][1, , drop = FALSE]
      list(activity="book browsing", venue=bookstore$title, description=paste("browse books at", bookstore$title), places=bookstore)
    } else if (any(grepl("park|nature", neighborhood_places$tags, ignore.case = TRUE))) {
      park <- neighborhood_places[grepl("park|nature", neighborhood_places$tags, ignore.case = TRUE), ][1, , drop = FALSE]
      list(activity="nature walk", venue=park$title, description=paste("take a walk through", park$title), places=park)
    } else if (any(grepl("thrift|vintage", neighborhood_places$tags, ignore.case = TRUE))) {
      thrift <- neighborhood_places[grepl("thrift|vintage", neighborhood_places$tags, ignore.case = TRUE), ][1, , drop = FALSE]
      list(activity="thrifting", venue=thrift$title, description=paste("browse vintage finds at", thrift$title), places=thrift)
    } else {
      chosen_place <- neighborhood_places[sample(nrow(neighborhood_places), 1), , drop = FALSE]
      list(activity="exploring", venue=chosen_place$title, description=paste("visit", chosen_place$title), places=chosen_place)
    }
  }
  
  estimated_time <- if (activity_plan$activity %in% c("reading", "coffee and reading")) "2-3 hours"
  else if (activity_plan$activity %in% c("shopping", "thrifting")) "1-3 hours"
  else if (activity_plan$activity %in% c("photography", "nature walk")) "1-2 hours"
  else "1-2 hours"
  
  plan_title <- paste("🏡", chosen_neighborhood)
  plan_description <- paste(transit_mode, "to", chosen_neighborhood, "and", activity_plan$description)
  list(list(
    type="structured",
    title=plan_title,
    description=plan_description,
    neighborhood=chosen_neighborhood,
    transit=transit_mode,
    activity=activity_plan$activity,
    places=activity_plan$places,
    estimated_time=estimated_time
  ))
}

generate_surprise_adventure <- function(available_places, time_available="quick", context=NULL,
                                        home_lat=NA_real_, home_lng=NA_real_, home_addr="") {
  if (nrow(available_places) == 0) return(list())
  
  # Adventure types
  adventure_types <- list(
    "Coffee + Photography" = list(
      activities=c("coffee","photography"),
      venues=list(c("coffee","cafe"), c("park","bridge","view","mural","art")),
      description_templates=c(
        "start with coffee at {venue1} and wander to {venue2} to take film photos of the area",
        "grab coffee at {venue1}, then take photos around {venue2}"
      )
    ),
    "Bike + Shopping" = list(
      activities=c("biking","shopping"),
      venues=list(c("bike","trail","path"), c("thrift","vintage","record","bookstore")),
      description_templates=c("bike to {venue1}, then browse {venue2}",
                              "cycle near {venue1} and check out {venue2}")
    ),
    "Draw + Coffee" = list(
      activities=c("drawing","coffee"),
      venues=list(c("park","garden","scenic","view"), c("coffee","cafe")),
      description_templates=c("sketch at {venue1}, then get coffee at {venue2}",
                              "find a spot to draw at {venue1} and refuel at {venue2}")
    ),
    "Thrift + Food" = list(
      activities=c("shopping","food"),
      venues=list(c("thrift","vintage","shop"), c("food","restaurant","bar","bakery")),
      description_templates=c("hunt for treasures at {venue1}, then grab a bite at {venue2}",
                              "browse {venue1} and treat yourself at {venue2}")
    ),
    "Music + Drinks" = list(
      activities=c("music","drinks"),
      venues=list(c("record","music","vinyl"), c("bar","brewery","pub")),
      description_templates=c("dig through records at {venue1}, then drinks at {venue2}",
                              "browse music at {venue1} and unwind at {venue2}")
    ),
    "Nature + Reading" = list(
      activities=c("nature","reading"),
      venues=list(c("park","garden","trail","nature"), c("bookstore","library","cafe")),
      description_templates=c("take a nature walk at {venue1}, then read at {venue2}",
                              "explore {venue1} and settle in with a book at {venue2}")
    )
  )
  
  # Prefer polygon neighborhoods for display
  available_places$neigh_disp <- neigh_display_vec(available_places$neighborhood_geo, available_places$neighborhood)
  
  adv_name <- sample(names(adventure_types), 1)
  adv <- adventure_types[[adv_name]]
  
  # Venue 1
  v1_candidates <- available_places[
    has_coords(available_places) &
      grepl(paste(adv$venues[[1]], collapse="|"), available_places$tags, ignore.case=TRUE),
    , drop=FALSE]
  if (nrow(v1_candidates) == 0) return(list())
  v1 <- v1_candidates[sample(nrow(v1_candidates), 1), , drop=FALSE]
  
  # Venue 2 (must have coords)
  v2_all <- available_places[
    has_coords(available_places) &
      grepl(paste(adv$venues[[2]], collapse="|"), available_places$tags, ignore.case=TRUE),
    , drop=FALSE]
  if (nrow(v2_all) > 0) {
    v2_all$ok_prox <- mapply(function(lat,lng) is_within_radius(v1$lat, v1$lng, lat, lng, 1), v2_all$lat, v2_all$lng)
    v2_all$ok_on_way <- FALSE
    if (home_is_set(home_addr, home_lat, home_lng)) {
      v2_all$ok_on_way <- mapply(function(lat,lng) {
        is_on_way_back(home_lat, home_lng, v1$lat, v1$lng, lat, lng, detour_factor=1.25)
      }, v2_all$lat, v2_all$lng)
    }
    v2_all <- v2_all[v2_all$ok_prox | v2_all$ok_on_way, , drop=FALSE]
  }
  if (nrow(v2_all) == 0) return(list())
  v2 <- v2_all[sample(nrow(v2_all), 1), , drop=FALSE]
  
  # Description uses polygon neighborhood names
  v1_nb <- neigh_display_vec(v1$neighborhood_geo, v1$neighborhood)
  v2_nb <- neigh_display_vec(v2$neighborhood_geo, v2$neighborhood)
  templ <- sample(adv$description_templates, 1)
  v1_label <- label_with_neigh(v1$title, v1_nb)
  v2_label <- label_with_neigh(v2$title, v2_nb)
  desc <- gsub("\\{venue1\\}", v1_label, templ)
  desc <- gsub("\\{venue2\\}", v2_label, desc)
  
  # Transit mode from avg distance (if computed)
  avg_d <- safe_mean(c(v1$distance_mi, v2$distance_mi))
  transit_mode <- "🚗 Drive"
  if (!is.na(avg_d)) {
    if (avg_d <= 2) transit_mode <- "🚶 Walk"
    else if (avg_d <= 8 && context != "😴 Low Energy") transit_mode <- "🚲 Bike"
  }
  
  plan <- list(
    type="adventure",
    title=paste("🎲", adv_name, "Adventure"),
    description=desc,
    neighborhood=if (!is.na(v1_nb)) v1_nb else "Portland",
    transit=transit_mode,
    activity=adv_name,
    places=rbind(v1, v2),
    estimated_time=if (time_available=="quick") "2-3 hours" else "3-5 hours"
  )
  
  # ensure all rows in plan$places have valid coordinates
  plan$places <- plan$places[has_coords(plan$places), , drop = FALSE]
  if (nrow(plan$places) == 0) return(list())  # bail if we lost both rows
  
  
  # Optional food add-on (must obey the same proximity/on-the-way rule)
  if (time_available %in% c("half_day","full_day")) {
    food_places <- available_places[
      grepl("coffee|cafe|food|bakery|restaurant", available_places$tags, ignore.case=TRUE),
      , drop=FALSE]
    if (nrow(food_places) > 0) {
      food_places$ok_prox <- mapply(function(lat,lng) is_within_radius(v1$lat, v1$lng, lat, lng, 1), food_places$lat, food_places$lng)
      food_places$ok_on_way <- FALSE
      if (home_is_set(home_addr, home_lat, home_lng)) {
        food_places$ok_on_way <- mapply(function(lat,lng) {
          is_on_way_back(home_lat, home_lng, v1$lat, v1$lng, lat, lng, detour_factor=1.25)
        }, food_places$lat, food_places$lng)
      }
      food_ok <- food_places[food_places$ok_prox | food_places$ok_on_way, , drop=FALSE]
      if (nrow(food_ok) > 0) {
        fv <- food_ok[sample(nrow(food_ok), 1), , drop=FALSE]
        fv_nb <- neigh_display_vec(fv$neighborhood_geo, fv$neighborhood)
        fv_label <- label_with_neigh(fv$title, fv_nb)
        plan$description <- paste(plan$description, "and finish with a stop at", fv_label)
        plan$places <- rbind(plan$places, fv)
        plan$estimated_time <- if (time_available=="half_day") "3-4 hours" else "5-6 hours"
      }
    }
  }
  
  list(plan)
}

ui <- fluidPage(
  # Weather/time in top-right corner of app
  div(class="app-weather-corner", uiOutput("weather_ui")),
  
  tags$head(
    tags$style(HTML("
  @import url('https://fonts.googleapis.com/css2?family=Work+Sans:wght@300;400;500;600;700&family=Poppins:wght@300;400;500;600;700&display=swap');
  :root{ 
    --bg:#fefefe; --bg-grad:#fdfdfd; --card:#ffffff; --border:#f0f0f0; --text:#1a1a1a; --muted:#888888; 
    --accent:#000000; --accent-600:#000000; --accent-50:#f8f8f8; --accent-hover:#333333;
    --secondary:#666666; --secondary-600:#555555; --secondary-50:#f9f9f9;
    --tertiary:#999999; --tertiary-600:#777777; --tertiary-50:#fafafa;
    --danger:#ff4444; --success:#00cc88; --warning:#ffaa00; 
    --shadow-soft:0 2px 16px rgba(0,0,0,0.04); --shadow-medium:0 4px 24px rgba(0,0,0,0.08);
    --radius-sm:4px; --radius-md:8px; --radius-lg:12px; --radius-xl:16px;
  }
  * { box-sizing: border-box; }
  body { 
    font-family:'Work Sans',system-ui,sans-serif !important; 
    background:linear-gradient(135deg,var(--bg) 0%,var(--bg-grad) 100%); 
    color:var(--text); margin:0; min-height:100vh; line-height:1.6;
  }
  
  /* Weather in top-right corner of app */
  .app-weather-corner {
    position:fixed; top:80px; right:20px; z-index:1000;
    display:flex; flex-direction:column; gap:8px;
  }

  /* Header card */
  .header { 
    background:var(--card); color:var(--text); padding:32px; margin:-15px -15px 0 -15px;
    box-shadow:var(--shadow-soft); border-radius:0 0 var(--radius-xl) var(--radius-xl); 
    border-bottom:1px solid var(--border);
  }

  /* Modern header layout */
  .header-grid { display:grid; grid-template-columns: 420px 1fr; gap:40px; align-items:start; }
  .header-left { width:420px; }
  
  .image-container {
    position:relative; width:400px; height:400px; border-radius:var(--radius-xl); 
    overflow:hidden; box-shadow:var(--shadow-medium); 
    transition:transform 0.3s ease, box-shadow 0.3s ease;
  }
  .image-container:hover { transform:translateY(-4px); box-shadow:0 16px 48px rgba(0,0,0,0.15); }
  
  .header-photo { 
    width:100%; height:100%; object-fit:cover; display:block;
  }
  

  /* Modern weather/time display */
  .weather-under { 
    width:340px; display:flex; flex-direction:column; gap:16px; 
    align-items:stretch; justify-content:flex-start; 
  }

  /* Overlay weather displays - compact and modern */
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

  /* Weather Display */
  .weather-display {
    background:rgba(255,255,255,0.95); backdrop-filter:blur(12px);
    border:1px solid rgba(255,255,255,0.6); border-radius:var(--radius-md);
    padding:12px 16px; transition:all 0.3s ease; min-width:180px;
    box-shadow:0 4px 20px rgba(0,0,0,0.1);
  }
  .weather-display:hover { transform:translateY(-2px); box-shadow:0 6px 24px rgba(0,0,0,0.15); }
  
  .weather-main {
    display:flex; justify-content:space-between; align-items:center;
    margin-bottom:8px;
  }
  .weather-condition {
    font-size:12px; font-weight:600; color:var(--text);
    text-transform:capitalize; font-family:'Poppins',sans-serif;
  }
  .weather-temp {
    font-size:16px; font-weight:700; color:var(--text);
    font-family:'Poppins',sans-serif;
  }
  
  .weather-details {
    display:flex; gap:8px; flex-wrap:wrap;
  }
  .weather-detail {
    font-size:9px; color:var(--muted); font-weight:500;
    background:rgba(255,255,255,0.6); padding:2px 6px;
    border-radius:8px; text-transform:uppercase; letter-spacing:0.5px;
  }

  /* Mobile responsive */
  @media (max-width: 1100px){
    .header { padding:24px; }
    .header-grid { grid-template-columns: 1fr; gap:24px; }
    .header-left { width:100%; display:flex; justify-content:center; }
    .image-container { width:100%; max-width:400px; height:400px; }
  }

  .header-right h1 { 
    font-family:'Poppins',sans-serif; font-weight:700; margin:0 0 8px 0; 
    font-size:2.5rem; letter-spacing:-0.02em; color:var(--text);
    background:linear-gradient(135deg, var(--text) 0%, var(--muted) 100%);
    -webkit-background-clip:text; background-clip:text;
  }
  .homebase-line { 
    color:var(--muted); margin:0 0 16px 0; font-size:0.95rem; font-weight:400;
  }

  /* Explainer + centered random button, then address box */
  .explainer { 
    background:var(--card); border:1px solid var(--border); border-radius:var(--radius-lg);
    padding:24px; box-shadow:var(--shadow-soft); margin-top:16px; 
    transition:all 0.3s ease; position:relative; overflow:hidden;
  }
  .explainer::before {
    content:''; position:absolute; top:0; left:0; right:0; height:3px;
    background:linear-gradient(90deg, var(--accent) 0%, var(--secondary) 100%);
    pointer-events:none;
  }
  .explainer:hover { transform:translateY(-2px); box-shadow:var(--shadow-medium); }
  
  .explainer h4 { 
    margin:0 0 12px 0; font-weight:600; font-size:1.1rem; 
    font-family:'Poppins',sans-serif; color:var(--text);
  }
  .explainer .muted { color:var(--muted); line-height:1.5; }

  .cta-row-inside { display:flex; justify-content:center; padding-top:20px; }
  .btn-big { 
    font-family:'Poppins',sans-serif; font-weight:600; padding:16px 32px; 
    border-radius:var(--radius-md); border:none;
    background:linear-gradient(135deg, var(--accent) 0%, var(--accent-hover) 100%); 
    color:white; box-shadow:var(--shadow-soft); 
    transition:all 0.3s cubic-bezier(0.4,0,0.2,1); cursor:pointer;
    text-decoration:none; display:inline-block; position:relative; overflow:hidden;
  }
  .btn-big::before {
    content:''; position:absolute; top:0; left:-100%; width:100%; height:100%;
    background:linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
    transition:left 0.6s; z-index:1; pointer-events:none;
  }
  .btn-big:hover { 
    transform:translateY(-3px); box-shadow:0 8px 25px rgba(108,123,92,0.4);
    background:linear-gradient(135deg, var(--accent-hover) 0%, var(--accent) 100%);
  }
  .btn-big:hover::before { left:100%; }
  .btn-big:active { transform:translateY(-1px); }
  
  /* Surprise button with animated dots */
  .btn-surprise .btn-icon {
    display:inline-block; margin-right:8px; position:relative;
    width:16px; height:16px; vertical-align:middle;
  }
  .btn-surprise .btn-icon::before,
  .btn-surprise .btn-icon::after {
    content:''; position:absolute; width:4px; height:4px;
    background:currentColor; border-radius:50%; animation:pulse 1.5s infinite;
    pointer-events:none;
  }
  .btn-surprise .btn-icon::before {
    top:0; left:0; animation-delay:0s;
  }
  .btn-surprise .btn-icon::after {
    bottom:0; right:0; animation-delay:0.75s;
  }
  @keyframes pulse {
    0%, 100% { opacity:0.3; transform:scale(0.8); }
    50% { opacity:1; transform:scale(1.2); }
  }

  /* Address box improvements */
  .address-box { 
    margin-top:20px; padding:20px; border:1px solid var(--border); 
    border-radius:var(--radius-md); background:var(--card); 
    box-shadow:var(--shadow-soft); transition:all 0.3s ease;
  }
  .address-box:hover { box-shadow:var(--shadow-medium); }
  .address-grid { display:grid; grid-template-columns: 1fr auto; gap:16px; align-items:center; }
  .address-status { font-size:13px; color:var(--muted); margin-top:8px; font-weight:500; }

  /* Control panel improvements */
  .control-panel { 
    background:var(--card); border-radius:var(--radius-lg); padding:28px; 
    border:1px solid var(--border); box-shadow:var(--shadow-soft);
    transition:all 0.3s ease; position:relative;
  }
  .control-panel:hover { box-shadow:var(--shadow-medium); }
  
  .control-panel h4, .control-panel h5 { 
    color:var(--text); font-weight:600; margin:8px 0 12px 0; 
    font-size:1.05em; letter-spacing:-0.01em; font-family:'Poppins',sans-serif;
  }

  /* Activity and transport button improvements */
  .activity-btn { 
    margin:6px 4px; padding:12px 16px; border-radius:var(--radius-md); 
    border:1px solid var(--border); background:var(--card); color:var(--text); 
    cursor:pointer; font-size:13px; font-weight:500; 
    transition:all 0.3s cubic-bezier(0.4,0,0.2,1); display:inline-block;
    position:relative; overflow:hidden;
  }
  .activity-btn::before {
    content:''; position:absolute; top:0; left:-100%; width:100%; height:100%;
    background:linear-gradient(90deg, transparent, rgba(108,123,92,0.1), transparent);
    transition:left 0.5s; z-index:0; pointer-events:none;
  }
  .activity-btn:hover { 
    border-color:var(--accent); transform:translateY(-2px); 
    box-shadow:0 4px 12px rgba(108,123,92,0.2); background:var(--accent-50);
  }
  .activity-btn:hover::before { left:100%; }
  .activity-btn.active { 
    background:linear-gradient(135deg, var(--accent) 0%, var(--accent-hover) 100%); 
    color:white; border-color:var(--accent); 
    box-shadow:0 6px 16px rgba(108,123,92,0.4); transform:translateY(-1px);
  }

  .transport-btn { 
    margin:6px 4px; padding:14px 18px; border-radius:var(--radius-md); 
    border:1px solid var(--border); background:var(--card); cursor:pointer; 
    font-weight:600; transition:all 0.3s cubic-bezier(0.4,0,0.2,1); 
    color:var(--text); position:relative; overflow:hidden;
  }
  .transport-btn::before {
    content:''; position:absolute; top:0; left:-100%; width:100%; height:100%;
    background:linear-gradient(90deg, transparent, rgba(155,139,122,0.1), transparent);
    transition:left 0.5s; z-index:0; pointer-events:none;
  }
  .transport-btn:hover { 
    border-color:var(--secondary); transform:translateY(-2px); 
    box-shadow:0 4px 12px rgba(155,139,122,0.2); background:var(--secondary-50);
  }
  .transport-btn:hover::before { left:100%; }
  .transport-btn.active { 
    background:linear-gradient(135deg, var(--secondary-50) 0%, #f7f6f4 100%); 
    border-color:var(--secondary); color:var(--secondary-600); 
    box-shadow:0 4px 12px rgba(155,139,122,0.25); transform:translateY(-1px);
  }

  /* Suggestion box improvements */
  .suggestion-box { 
    background:var(--card); border-radius:var(--radius-lg); padding:28px; 
    border-left:4px solid var(--accent); margin:24px 0; 
    box-shadow:var(--shadow-soft); border:1px solid var(--border);
    transition:all 0.3s ease; position:relative; overflow:hidden;
  }
  .suggestion-box::before {
    content:''; position:absolute; top:0; right:0; bottom:0; width:2px;
    background:linear-gradient(180deg, var(--accent) 0%, var(--secondary) 100%);
  }
  .suggestion-box:hover { 
    transform:translateY(-2px); box-shadow:var(--shadow-medium); 
  }
  .suggestion-box h3, .suggestion-box h4 { 
    font-family:'Poppins',sans-serif; margin-bottom:12px; 
  }
  
  /* Data table improvements */
  .dataTables_wrapper { 
    background:var(--card); border-radius:var(--radius-lg); padding:24px; 
    box-shadow:var(--shadow-soft); border:1px solid var(--border);
    transition:all 0.3s ease;
  }
  .dataTables_wrapper:hover { box-shadow:var(--shadow-medium); }
  
  /* Map improvements */
  .leaflet-container { 
    border-radius:var(--radius-lg); box-shadow:var(--shadow-medium);
    transition:all 0.3s ease;
  }
  .leaflet-container:hover { box-shadow:0 12px 40px rgba(0,0,0,0.15); }

  /* Form improvements */
  .form-control, .selectize-input, input[type=text], select {
    border:1px solid var(--border) !important; border-radius:var(--radius-sm) !important;
    padding:12px 16px !important; font-size:14px !important; 
    transition:all 0.3s ease !important; background:var(--card) !important;
  }
  .form-control:focus, .selectize-input.focus, input[type=text]:focus, select:focus {
    border-color:var(--accent) !important; 
    box-shadow:0 0 0 3px rgba(246,139,139,0.1) !important;
    outline:none !important;
  }
  
  .btn-primary, .btn-outline-primary {
    border-radius:var(--radius-sm) !important; font-weight:500 !important;
    padding:12px 20px !important; transition:all 0.3s ease !important;
    border:1px solid var(--accent) !important;
  }
  .btn-primary {
    background:linear-gradient(135deg, var(--accent) 0%, var(--accent-hover) 100%) !important;
    color:white !important;
  }
  .btn-primary:hover {
    transform:translateY(-2px) !important; 
    box-shadow:0 6px 16px rgba(246,139,139,0.3) !important;
  }
  .btn-outline-primary {
    background:var(--card) !important; color:var(--accent) !important;
  }
  .btn-outline-primary:hover {
    background:var(--accent-50) !important; transform:translateY(-1px) !important;
  }

  #sextant_label, #neighborhood_label { margin-bottom: 8px !important; }

  /* Responsive improvements */
  @media (max-width: 1100px){
    .header-grid { grid-template-columns: 1fr; }
    .header-left { width:100%; align-items:center; }
    .header-photo { width:100%; max-width:420px; height:auto; }
    .weather-under { width:100%; max-width:420px; }
    .control-panel { padding:20px; }
    .suggestion-box { padding:20px; }
  }
")),
    uiOutput("theme_override")
  ),
  
  # ======= HEADER (bigger image left; weather under image; title/explainer/random + address on right) =======
  div(class = "header",
      div(class = "header-grid",
          # Left: just the image, clean
          div(class="header-left",
              div(class="image-container",
                  if (!is.null(HEADER_IMG))
                    tags$img(src = HEADER_IMG, alt = "Portland", class = "header-photo")
              )
          ),
          # Right: title, home info, explainer, centered random, then address box
          div(class = "header-right",
              h1("The Dream of the 90s is Alive in Portland"),
              uiOutput("home_info_ui"),
              div(class = "explainer",
                  h4("What Are We Doing Today?"),
                  div(style="margin-top:6px; color:#475569;",
                      "Use “Random Suggestion” for a spontaneous idea, or use “Get Suggestions” for a tailored plan."
                  ),
                  div(class = "muted",
                      "Pick the kind of place you want to go to, add which part of town you want to visit, or how you want to get there.",
                      "Enter your home address to enable distance-aware filters."
                  ),
                  div(class="cta-row-inside",
                      actionButton("random_inspiration", HTML("<span class='btn-icon'></span>Surprise Me"), class = "btn-big btn-surprise")
                  ),
                  div(class="address-box",
                      h5("Address (optional)"),
                      div(class="address-grid",
                          textInput("home_address", "", placeholder = "Enter your address (e.g., 123 Main St, Portland, OR)", value = "", width = "100%"),
                          actionButton("geocode_address", "Set Location", class = "btn-outline-primary", style = "min-width: 140px;")
                      ),
                      div(id = "address_status", class="address-status")
                  )
              )
          )
      )
  ),
  
  # ======= MAIN CONTENT =======
  fluidRow(
    column(
      4,
      div(class = "control-panel",
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
      leafletOutput("map", height = 520),
      br(),
      uiOutput("suggestion_display"),
      br(),
      h5("📋 Filtered Places"),
      DT::dataTableOutput("places_table")
    )
  ),
  
  # JS for toggles (unchanged)
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


#  Server + Launch


# Geocoding util (used by server)
geocode_address <- function(address) {
  tryCatch({
    if (is.null(address) || str_trim(address) == "") {
      return(list(success = FALSE, error = "Please enter an address"))
    }
    encoded <- utils::URLencode(str_trim(address))
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

# Simple persistence for visited
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
weather_emoji <- function(condition) {
  c <- tolower(condition %||% "")
  if (grepl("sun|clear", c)) return("☀️")
  if (grepl("partly|cloud", c)) return("🌤")
  if (grepl("rain|drizzle|shower|storm", c)) return("🌧")
  if (grepl("snow|sleet", c)) return("❄️")
  if (grepl("fog|mist|haze", c)) return("🌫")
  "⛅"
}
weather_palette <- function(condition) {
  c <- tolower(condition %||% "")
  if (grepl("sun|clear", c)) return(list(accent="#ff8a00", accent600="#e67a00", accent50="#fff2e6"))
  if (grepl("rain|drizzle|shower|storm", c)) return(list(accent="#3b82f6", accent600="#2563eb", accent50="#e8f0ff"))
  if (grepl("snow|sleet", c)) return(list(accent="#64748b", accent600="#475569", accent50="#eef2f6"))
  if (grepl("fog|mist|haze", c)) return(list(accent="#94a3b8", accent600="#64748b", accent50="#f1f5f9"))
  list(accent="#000000", accent600="#000000", accent50="#f8f8f8")
}

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  # Put this near the top of `server`
  current_weather <- reactive({
    # refresh every 15 minutes so the theme can update
    invalidateLater(15 * 60 * 1000, session)
    get_weather_forecast() %||% list(success = FALSE)
  })
  
  # Modern weather/time display without emojis
  output$weather_ui <- renderUI({
    w <- current_weather(); time_str <- pdx_time_string()
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
  
  # Dynamic theme override (injected into <head>)
  output$theme_override <- renderUI({
    w <- current_weather()
    pal <- weather_palette(if (isTRUE(w$success)) w$condition else NULL)
    tags$style(HTML(sprintf(
      ":root{ --accent:%s; --accent-600:%s; --accent-50:%s; }",
      pal$accent, pal$accent600, pal$accent50
    )))
  })
  
  
  # ---------- State ----------
  values <- reactiveValues(
    completed = load_completed(),
    suggested = NULL,
    inspiration_text = NULL,
    home_lat = NA_real_,
    home_lng = NA_real_,
    home_address = ""
  )
  
  # Header: home info
  output$home_info_ui <- renderUI({
    if (!is.null(values$home_address) && nzchar(values$home_address)) {
      tags$p(sprintf("home base: %s", values$home_address), class = "homebase-line")
    } else NULL
  })
  

  # Concise header explainer (keep existing)
  output$explainer_ui <- renderUI({
    div(class = "explainer",
        div(style="margin-top:6px; color:#475569;",
            "Use “Get Suggestions” for a tailored plan or “Random Suggestion” above for a spontaneous idea."
        ),
        div(class = "muted",
            "Pick a Sextant, then optionally a Neighborhood within it. Add context and transport. ",
            "Enter your home address to enable distance-aware filters."
        )
    )
  })
  
  # Geocode
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
  
  # Sextant helpers
  get_sextant_choices <- function(places, sections_boundaries, SEC_NAME_COL) {
    choices <- NULL
    if (!is.null(sections_boundaries) && !is.null(SEC_NAME_COL) && SEC_NAME_COL %in% names(sections_boundaries)) {
      choices <- sections_boundaries[[SEC_NAME_COL]] |> as.character()
    } else if ("section" %in% names(places)) {
      choices <- as.character(places$section)
    }
    if (is.null(choices) || !length(choices)) choices <- c("North","Northeast","Northwest","South","Southeast","Southwest")
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
  
  # Sextant selector
  output$section_selector <- renderUI({
    sextant_choices <- get_sextant_choices(places, sections_boundaries, SEC_NAME_COL)
    selectizeInput("section_filter", "", choices = sextant_choices, selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Choose Sextant(s)"))
  })
  
  # Map clicks -> left panel sync (Sextants & Neighborhoods)
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (is.null(click$id) || is.null(click$group)) return(NULL)
    
    if (identical(click$group, "sextants")) {
      raw_name <- sub("^sextant::", "", click$id)
      sec_name <- normalize_sextant(raw_name)
      sx_choices <- get_sextant_choices(places, sections_boundaries, SEC_NAME_COL)
      if (!sec_name %in% sx_choices) {
        showNotification(paste("Unrecognized Sextant:", raw_name), type = "warning"); return(NULL)
      }
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
        showNotification("Select a Sextant first to choose Neighborhoods.", type = "message"); return(NULL)
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
  
  # Neighborhood selector populated by selected Sextants
  output$neighborhood_selector <- renderUI({
    if (is.null(input$section_filter) || !length(input$section_filter) ||
        is.null(sections_boundaries) || is.null(SEC_NAME_COL) ||
        is.null(neighborhood_boundaries) || is.null(NEI_NAME_COL)) {
      return(selectizeInput("neighborhood_filter", "", choices = character(0), selected = NULL, multiple = TRUE,
                            options = list(placeholder = "Select a Sextant first")))
    }
    sel_secs <- sections_boundaries[sections_boundaries[[SEC_NAME_COL]] %in% normalize_sextant(input$section_filter), , drop = FALSE]
    rows_to_draw <- safe_st_intersects_rows(neighborhood_boundaries, sel_secs)
    available_neighborhoods <- if (length(rows_to_draw)) {
      neighborhood_boundaries[rows_to_draw, ][[NEI_NAME_COL]] |> as.character() |> unique() |> sort()
    } else character(0)
    selectizeInput("neighborhood_filter", "", choices = available_neighborhoods, selected = NULL, multiple = TRUE,
                   options = list(placeholder = if (length(available_neighborhoods)) "Choose Neighborhoods (optional)" else "No Neighborhoods in the selected Sextant(s)"))
  })
  
  # Filtering pipeline (unchanged except for distances reactive)
  filtered_places <- reactive({
    df <- places_with_distances()
    df$neigh_disp <- neigh_display_vec(df$neighborhood_geo, df$neighborhood)
    
    if (!is.null(input$section_filter) && length(input$section_filter) > 0 && ("section_geo" %in% names(df) || "section" %in% names(df))) {
      wanted <- normalize_sextant(input$section_filter)
      if ("section_geo" %in% names(df)) {
        df <- df[!is.na(df$section_geo) & df$section_geo %in% wanted, , drop = FALSE]
      } else {
        df$section_norm <- normalize_sextant(df$section)
        df <- df[!is.na(df$section_norm) & df$section_norm %in% wanted, , drop = FALSE]
      }
    }
    
    if (!is.null(input$neighborhood_filter) && length(input$neighborhood_filter) > 0) {
      df <- df[!is.na(df$neigh_disp) & df$neigh_disp %in% input$neighborhood_filter, , drop = FALSE]
    }
    
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
    
    if (home_is_set(values$home_address, values$home_lat, values$home_lng) &&
        !is.null(input$selected_transport) && nzchar(input$selected_transport)) {
      transport_text <- input$selected_transport
      for (mode in names(TRANSPORT_MODES)) {
        if (stringr::str_detect(transport_text, fixed(stringr::str_sub(mode, 1, 10)))) {
          max_dist <- TRANSPORT_MODES[[mode]]
          if (max_dist < 999) df <- df[!is.na(df$distance_mi) & df$distance_mi <= max_dist, , drop = FALSE]
          break
        }
      }
    }
    
    if (!is.null(input$keyword_filter) && nzchar(input$keyword_filter)) {
      keywords <- stringr::str_split(input$keyword_filter, ",")[[1]] |> stringr::str_trim()
      for (kw in keywords[keywords != ""]) {
        kw_match <- stringr::str_detect(tolower(paste(df$title, df$tags, df$note)), fixed(tolower(kw)))
        df <- df[kw_match, , drop = FALSE]
      }
    }
    df
  })
  
  available_places <- reactive({
    df <- filtered_places()
    df[!(df$id %in% values$completed), , drop = FALSE]
  })
  
  # Suggestions (unchanged)
  observeEvent(input$suggest_place, {
    candidates <- available_places()
    if (nrow(candidates) == 0) {
      showNotification("No unvisited places match your criteria!", type = "warning")
      values$suggested <- NULL; return()
    }
    context_for_plan <- if (length(input$context_filter) > 0) input$context_filter[1] else NULL
    plans <- generate_day_plan(candidates, context_for_plan, input$selected_activities, input$selected_activity_modes, input$time_filter)
    if (length(plans) > 0 && !is.null(plans[[1]])) {
      plan <- plans[[1]]
      values$suggested <- plan$places[1, , drop = FALSE]
      values$inspiration_text <- list(
        title = plan$title, description = plan$description, type = plan$type, estimated_time = plan$estimated_time
      )
    } else {
      values$suggested <- candidates[sample(nrow(candidates), 1), , drop = FALSE]
      values$inspiration_text <- NULL
    }
  })
  
  observeEvent(input$random_inspiration, {
    all_available <- places[!(places$id %in% values$completed), , drop = FALSE]
    if (nrow(all_available) == 0) {
      showNotification("You've visited everywhere! Time for new places.", type = "warning")
      values$suggested <- NULL; return()
    }
    context_for_adventure <- if (length(input$context_filter) > 0) input$context_filter[1] else NULL
    adventures <- generate_surprise_adventure(
      available_places = all_available,
      time_available = input$time_filter,
      context = context_for_adventure,
      home_lat = values$home_lat, home_lng = values$home_lng, home_addr = values$home_address
    )
    if (length(adventures) > 0 && !is.null(adventures[[1]])) {
      adventure <- adventures[[1]]
      values$suggested <- adventure$places[1, , drop = FALSE]
      values$inspiration_text <- list(
        title = adventure$title, description = adventure$description, type = adventure$type,
        estimated_time = adventure$estimated_time, transit = adventure$transit, neighborhood = adventure$neighborhood
      )
    } else {
      interesting_places <- all_available[
        has_coords(all_available) &
          grepl("coffee|vintage|record|bookstore|gallery|museum|brewery",
                all_available$tags, ignore.case = TRUE),
        , drop = FALSE]
      if (nrow(interesting_places) > 0) {
        chosen_place <- interesting_places[sample(nrow(interesting_places), 1), , drop = FALSE]
        activities <- c("explore", "discover", "check out", "investigate", "wander around")
        activity <- sample(activities, 1)
        values$suggested <- all_available[
          has_coords(all_available),
          , drop = FALSE
        ][sample(sum(has_coords(all_available)), 1), , drop = FALSE]
        values$inspiration_text <- list(
          title = "🎲 Mystery Adventure",
          description = paste(activity, chosen_place$title, "and find out!"),
          type = "adventure",
          estimated_time = "1-2 hours"
        )
      } else {
        # pure random fallback — but only from rows with valid lat/lng
        coord_ok <- has_coords(all_available)
        if (sum(coord_ok) == 0) {
          showNotification("No mappable places have coordinates — adjust filters or set your address.", type = "warning")
          values$suggested <- NULL
          return(invisible(NULL))
        }
        values$suggested <- all_available[coord_ok, , drop = FALSE][
          sample(sum(coord_ok), 1), , drop = FALSE
        ]
        values$inspiration_text <- list(
          title = "🎲 Random Adventure",
          description = "Go explore this place and see what happens!",
          type = "random",
          estimated_time = "1-3 hours"
        )
      }
    }
  })
  
  # Visited toggles (unchanged)
  observeEvent(input$mark_visited, {
    if (is.null(values$suggested)) {
      showNotification("Please suggest a place first!", type = "message")
    } else {
      values$completed <- unique(c(values$completed, values$suggested$id))
      save_completed(values$completed)
      showNotification(paste("✅", values$suggested$title, "marked as visited!"), type = "success")
    }
  })
  observeEvent(input$unmark, {
    if (is.null(values$suggested)) {
      showNotification("Please suggest a place first!", type = "message")
    } else {
      values$completed <- setdiff(values$completed, values$suggested$id)
      save_completed(values$completed)
      showNotification(paste("↩️", values$suggested$title, "unmarked!"), type = "success")
    }
  })
  observeEvent(input$reset_visited, {
    values$completed <- character(0)
    save_completed(values$completed)
    showNotification("♻️ All visited places reset!", type = "info")
  })
  
  # Map base
  output$map <- renderLeaflet({
    center_lat <- if (!is.na(values$home_lat)) values$home_lat else 45.5152
    center_lng <- if (!is.na(values$home_lng)) values$home_lng else -122.6784
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = center_lng, lat = center_lat, zoom = 11)
  })
  
  # Map layers (unchanged)
  observe({
    filtered <- filtered_places()
    visited <- places[places$id %in% values$completed, , drop = FALSE]
    suggested <- values$suggested
    
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    
    # Sextants
    if (!is.null(sections_boundaries) && !is.null(SEC_NAME_COL) && SEC_NAME_COL %in% names(sections_boundaries)) {
      secs_all <- sections_boundaries
      sx_names_raw <- as.character(secs_all[[SEC_NAME_COL]]); sx_names <- normalize_sextant(sx_names_raw)
      selected <- isolate(input$section_filter); selected <- if (is.null(selected)) character(0) else normalize_sextant(selected)
      section_colors <- c(
        "Southwest" = "#ff6b6b", "Northwest" = "#666666", "Southeast" = "#888888",
        "Northeast" = "#96ceb4", "North" = "#feca57", "South" = "#fd79a8", "Other" = "#a29bfe"
      )
      for (i in seq_along(sx_names)) {
        sx <- sx_names[i]; is_selected <- sx %in% selected
        base_color <- section_colors[[sx]] %||% section_colors[["Other"]]
        proxy <- proxy %>% addPolygons(
          data = secs_all[i, ],
          fillColor = base_color, fillOpacity = if (is_selected) 0.08 else 0.05,
          color = base_color, weight = if (is_selected) 2.5 else 1.5, opacity = if (is_selected) 0.9 else 0.7,
          group = "sextants", layerId = paste0("sextant::", sx),
          options = pathOptions(interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
          highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.25, bringToFront = FALSE),
          popup = paste0("<b>", sx, "</b><br>", if (is_selected) "Click to remove" else "Click to add")
        )
      }
    }
    
    # Neighborhoods (only when Sextant selected)
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
            color = if (is_selected) "#000000" else "#666666",
            weight = if (is_selected) 2 else 1.2,
            opacity = if (is_selected) 1 else 0.6,
            group = "neighborhoods", layerId = paste0("neigh::", nb_name),
            options = pathOptions(interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
            highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.25, bringToFront = TRUE),
            popup = paste0("<b>", nb_name, "</b><br>", if (is_selected) "Click to remove" else "Click to add")
          )
        }
      }
    }
    
    # Home
    if (home_is_set(values$home_address, values$home_lat, values$home_lng)) {
      proxy <- proxy %>% addCircleMarkers(
        lng = values$home_lng, lat = values$home_lat, label = "Home", popup = values$home_address,
        radius = 8, color = "#16a34a", fillColor = "#16a34a", opacity = 1, fillOpacity = 0.9
      )
    }
    
    # Filtered places (only those with coords)
    fp <- filtered[has_coords(filtered), , drop = FALSE]
    if (nrow(fp) > 0) {
      dist_txt <- ifelse(is.na(fp$distance_mi), "", paste0("<br>", round(fp$distance_mi, 1), " miles from home"))
      proxy <- proxy %>% addCircleMarkers(
        lng = fp$lng, lat = fp$lat, radius = 6,
        color = "#666666", fillColor = "#f0f0f0", opacity = 0.9, fillOpacity = 0.6,
        popup = paste0("<b>", fp$title, "</b><br>", fp$tags, dist_txt),
        options = pathOptions()
      )
    }
    
    # Visited
    if (nrow(visited) > 0) {
      proxy <- proxy %>% addCircleMarkers(
        lng = visited$lng, lat = visited$lat, radius = 5,
        color = "#9ca3af", fillColor = "#9ca3af", opacity = 0.9, fillOpacity = 0.7,
        options = pathOptions(interactive = TRUE, zIndexOffset = 1100)
      )
    }
    
    # Suggested (only if coords exist)
    if (!is.null(suggested) && nrow(suggested) > 0 && has_coords(suggested)[1]) {
      proxy <- proxy %>% addCircleMarkers(
        lng = suggested$lng, lat = suggested$lat, radius = 12,
        color = "#dc2626", fillColor = "#dc2626", opacity = 1, fillOpacity = 0.85,
        options = pathOptions()
      ) %>% setView(lng = suggested$lng, lat = suggested$lat, zoom = 15)
    }
  })
  
  observe({
    weather <- current_weather()
    if (!is.null(weather) && isTRUE(weather$is_rainy) &&
        (is.null(input$context_filter) || length(input$context_filter) == 0)) {
      updateSelectizeInput(session, "context_filter", selected = "☔ Rainy Weather")
      showNotification("☔ Rainy weather detected - filtering to indoor activities", type = "message")
    }
  })
  
  
  # Suggestion display (unchanged)
  output$suggestion_display <- renderUI({
    if (is.null(values$suggested)) {
      div(class = "suggestion-box",
          h4("🎯 Ready to explore?"),
          p("Use 'Get Suggestions' for an intelligent plan, or 'Random Suggestion' for spontaneous ideas!")
      )
    } else {
      place <- values$suggested
      clean_tags_display <- clean_tags(place$tags)
      nb_disp <- neigh_display_vec(place$neighborhood_geo, place$neighborhood)
      
      suggestion_header <- if (!is.null(values$inspiration_text) && is.list(values$inspiration_text)) {
        div(
          h4(values$inspiration_text$title, style = "color: var(--accent); margin-bottom: 8px;"),
          p(style = "color: #666; font-style: italic; margin-bottom: 12px;", values$inspiration_text$description),
          if (!is.null(values$inspiration_text$estimated_time))
            p(style = "color: #666; font-size: 12px;", "⏱️ Estimated time: ", values$inspiration_text$estimated_time),
          h3("✨ ", place$title)
        )
      } else {
        h3("🌟 ", place$title)
      }
      
      div(class = "suggestion-box",
          suggestion_header,
          if (nzchar(clean_tags_display)) p(strong("Features: "), clean_tags_display),
          if (!is.na(place$distance_mi)) p(strong("Distance: "), round(place$distance_mi, 1), " miles from home"),
          if (!is.na(nb_disp)) p(strong("Neighborhood: "), nb_disp),
          if (nzchar(place$note)) p(strong("Note: "), place$note),
          if (nzchar(place$url)) tags$a("📍 View on Google Maps", href = place$url, target = "_blank")
      )
    }
  })
  
  # Table + visited preview (unchanged)
  output$places_table <- DT::renderDataTable({
    df <- filtered_places()
    if (nrow(df) == 0) return(NULL)
    
    df$clean_tags <- sapply(df$tags, clean_tags)
    df$neighborhood_display <- neigh_display_vec(df$neighborhood_geo, df$neighborhood)
    
    display_df <- df %>%
      mutate(distance_mi = round(distance_mi, 1)) %>%
      dplyr::select(dplyr::any_of(c("title", "clean_tags", "feature", "neighborhood_display", "distance_mi"))) %>%
      dplyr::rename(
        Place = title,
        Tags = clean_tags,
        Neighborhood = neighborhood_display,
        `Distance (mi)` = distance_mi
      )
    
    if (!"feature" %in% names(df) && !"Feature" %in% names(display_df)) {
      # no feature col
    } else {
      names(display_df)[names(display_df) == "feature"] <- "Feature"
    }
    
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
