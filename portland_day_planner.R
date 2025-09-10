# portland_day_planner.R — Portland Day-Off Planner (Prettier w/ Explainer)
# - Sextants & Neighborhoods
# - Walk/Bike/Drive labels
# - Map click <-> left panel sync
# - Neighborhoods hidden until a Sextant is chosen
# - New header image + dedicated "Explainer" card with Portland time & weather
# -------------------------------------------------------------------
# Put a header image at: ./www/pdx_header.png  (or change src below)

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

# --- Static assets path (works with runApp() OR source()) --------------------
get_app_dir <- function() {
  # If the file is being sourced, use its folder; otherwise use getwd()
  tryCatch(
    normalizePath(dirname(sys.frame(1)$ofile)),
    error = function(e) normalizePath(getwd())
  )
}

# ---- Header image (base64-embed if present) ----
HEADER_IMG <- tryCatch({
  img_path <- file.path(APP_DIR, "www", "pdx_header.png")
  if (file.exists(img_path)) base64enc::dataURI(file = img_path, mime = "image/png") else NULL
}, error = function(e) NULL)


# ---------------- DEFAULT CONFIGURATION ----------------
DEFAULT_ADDRESS <- "Portland, OR"
DEFAULT_LAT <- 45.5152
DEFAULT_LNG <- -122.6784

# ---------------- WEATHER (best-effort; never crash UI) ----------------
# wttr.in tokens: %C=condition, %t=temp with unit, %h=humidity, %w=wind; &u forces Fahrenheit
get_weather_forecast <- function() {
  tryCatch({
    url <- "http://wttr.in/Portland,OR?format=%C|%t|%h|%w&u"
    response <- readLines(url, warn = FALSE)
    parts <- strsplit(response[1], "\\|")[[1]]
    if (length(parts) >= 4) {
      condition <- parts[1]
      temp      <- parts[2]  # already like "+63°F"
      humidity  <- parts[3]
      wind      <- parts[4]
      is_rainy  <- grepl("rain|drizzle|shower|storm", tolower(condition))
      return(list(
        condition = condition,
        temperature = temp,
        humidity = humidity,
        wind = wind,
        is_rainy = is_rainy,
        success = TRUE
      ))
    }
    list(success = FALSE)
  }, error = function(e) list(success = FALSE))
}

# Portland local time string
pdx_time_string <- function() {
  format(as.POSIXct(Sys.time(), tz = "America/Los_Angeles"), "%a %b %d, %I:%M %p")
}

# ---------------- CATEGORIES & MODES ----------------
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

TRANSPORT_MODES <- list(
  "🚶 Walk"   = 2,
  "🚲 Bike"   = 10,
  "🚌 Transit"= 15,
  "🚗 Drive"  = 30,
  "🌍 Any Distance" = 999
)

ACTIVITY_MODES <- list(
  "📖 Reading"           = c("coffee","cafe","bookstore","quiet","library","park"),
  "🎨 Drawing/Sketching" = c("coffee","cafe","park","outdoor","scenic","garden","museum"),
  "🚲 Bike Ride"         = c("trail","path","route","bike path","greenway"),
  "🥾 Hiking"            = c("trail","hike","nature","forest","mountain","waterfall"),
  "📸 Photography"       = c("scenic","architecture","vintage","historic","art","bridge","view","mural"),
  "🚶 Walking Tour"      = c("neighborhood","historic","architecture","street art","district"),
  "🛍️ Shopping Spree"    = c("thrift","vintage","bookstore","record","shopping","market"),
  "🍽️ Food Adventure"    = c("restaurant","food cart","market","bakery","brewery","cafe")
)

CONTEXT_FILTERS <- list(
  "☔ Rainy Weather" = list(
    exclude_activities    = c("🚲 Bike Ride","🥾 Hiking","🚶 Walking Tour","📸 Photography"),
    exclude_venues        = c("🌲 Parks & Nature"),
    hide_activity_buttons = c("🚲 Bike Ride","🥾 Hiking","🚶 Walking Tour","📸 Photography"),
    prefer_close = TRUE, max_distance = 3,
    suggestion_prefix = "Stay dry with indoor activities:"
  ),
  "😴 Low Energy" = list(
    exclude_activities = c("🚲 Bike Ride","🥾 Hiking"),
    prefer_activities  = c("📖 Reading","🎨 Drawing/Sketching"),
    prefer_venues      = c("☕️ Coffee & Cafes","📚 Bookstores"),
    prefer_close = TRUE, max_distance = 2,
    suggestion_prefix = "Take it easy with relaxing activities:"
  ),
  "🌞 Perfect Weather" = list(
    prefer_activities = c("🚲 Bike Ride","🥾 Hiking","📸 Photography","🚶 Walking Tour"),
    prefer_venues     = c("🌲 Parks & Nature"),
    boost_outdoor = TRUE,
    suggestion_prefix = "Beautiful day for outdoor adventures:"
  ),
  "🎯 Focused Mission" = list(
    prefer_close = TRUE, max_distance = 5,
    suggestion_prefix = "Let's find exactly what you're looking for:"
  )
)

# ---------------- HELPERS ----------------
calc_distance_miles <- function(lat1, lng1, lat2, lng2) {
  if (any(is.na(c(lat1, lng1, lat2, lng2)))) return(NA_real_)
  R <- 3959
  lat1 <- lat1 * pi/180; lat2 <- lat2 * pi/180
  dlat <- (lat2 - lat1); dlng <- (lng2 - lng1) * pi/180
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlng/2)^2
  round(R * 2 * atan2(sqrt(a), sqrt(1 - a)), 1)
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

# ---------------- GEOCODING ----------------
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

# ---------------- DATA LOAD ----------------
processed_file <- "data/portland_places_processed.rds"
if (!file.exists(processed_file)) stop("❌ Missing processed data. Run process_data.R first.")
places <- readRDS(processed_file)
if (!is.data.frame(places) || nrow(places) == 0) stop("❌ Invalid processed data.")
if ("title" %in% names(places)) {
  places <- places[!grepl("^Unnamed place", places$title, ignore.case = TRUE), ]
}

# ---------------- MAP LAYERS ----------------
load_map_file <- function(paths) { for (p in paths) if (file.exists(p)) return(p); NULL }
safe_read_sf <- function(path) {
  if (is.null(path)) return(NULL)
  tryCatch(sf::st_read(path, quiet = TRUE),
           error = function(e) { message("⚠️ Could not load ", path, ": ", e$message); NULL })
}
pick_name_col <- function(sfobj, candidates) {
  cands <- candidates[candidates %in% names(sfobj)]
  if (length(cands)) cands[[1]] else NULL
}

# Sextants
sextants_path <- load_map_file(c(
  "archive/Portland_Administrative_Sextants.geojson",
  "data/Portland_Administrative_Sextants.geojson"
))
sections_boundaries <- safe_read_sf(sextants_path)
SEC_NAME_COL <- if (!is.null(sections_boundaries)) pick_name_col(sections_boundaries, c("Sextant","SEXTANT","PREFIX","NAME")) else NULL

# Neighborhoods
neighborhood_path <- load_map_file(c(
  "archive/Neighborhood_Boundaries.geojson",
  "archive/neighborhoods.geojson",
  "data/Neighborhood_Boundaries.geojson"
))
neighborhood_boundaries <- safe_read_sf(neighborhood_path)
NEI_NAME_COL <- if (!is.null(neighborhood_boundaries)) pick_name_col(neighborhood_boundaries, c("MAPLABEL","NAME","Label","Neighborhood","NEIGHBORHD","neigh","label")) else NULL

# ---------------- PLAN GENERATORS ----------------
generate_day_plan <- function(available_places, context = NULL, selected_activities = NULL, selected_modes = NULL, time_available = "quick") {
  if (nrow(available_places) == 0) return(list())
  places_with_neighborhoods <- available_places[!is.na(available_places$neighborhood), , drop = FALSE]
  if (nrow(places_with_neighborhoods) == 0) return(list())
  neighborhood_counts <- table(places_with_neighborhoods$neighborhood)
  good_neighborhoods <- names(neighborhood_counts[neighborhood_counts >= 1])
  chosen_neighborhood <- sample(good_neighborhoods, 1)
  neighborhood_places <- places_with_neighborhoods[places_with_neighborhoods$neighborhood == chosen_neighborhood, , drop = FALSE]
  avg_distance <- mean(neighborhood_places$distance_mi, na.rm = TRUE)
  
  force_drive <- FALSE; max_allowed_distance <- Inf
  if (!is.null(context)) {
    context_config <- CONTEXT_FILTERS[[context]]
    if (!is.null(context_config)) {
      if (context == "☔ Rainy Weather") {
        force_drive <- TRUE; max_allowed_distance <- context_config$max_distance %||% 3
      } else if (context == "😴 Low Energy") {
        if (avg_distance > 1) force_drive <- TRUE
        max_allowed_distance <- context_config$max_distance %||% 2
      } else if ("max_distance" %in% names(context_config)) {
        max_allowed_distance <- context_config$max_distance
      }
    }
  }
  if (force_drive || avg_distance > max_allowed_distance) {
    transit_mode <- "🚗 Drive"
  } else if (avg_distance <= 2) {
    transit_mode <- "🚶 Walk"
  } else if (avg_distance <= 8 && context != "😴 Low Energy") {
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
        list(activity = "reading", venue = chosen_spot$title, description = paste("go to", chosen_spot$title, "and read"), places = chosen_spot)
      } else list(activity = "reading", venue = "a quiet spot", description = "find a quiet spot and read", places = neighborhood_places[1, , drop = FALSE])
    } else if (mode == "🎨 Drawing/Sketching") {
      scenic_spots <- neighborhood_places[grepl("park|coffee|cafe|garden|scenic|view", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      if (nrow(scenic_spots) > 0) {
        chosen_spot <- scenic_spots[sample(nrow(scenic_spots), 1), , drop = FALSE]
        list(activity = "sketching", venue = chosen_spot$title, description = paste("go to", chosen_spot$title, "and sketch"), places = chosen_spot)
      } else list(activity = "sketching", venue = "a scenic spot", description = "find a scenic spot and sketch", places = neighborhood_places[1, , drop = FALSE])
    } else if (mode == "🛍️ Shopping Spree") {
      shopping_spots <- neighborhood_places[grepl("thrift|vintage|bookstore|record|market|shop", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      if (nrow(shopping_spots) > 0) {
        list(activity = "shopping",
             venue = if (nrow(shopping_spots) > 1) "multiple shops" else shopping_spots$title[1],
             description = paste("browse", if (nrow(shopping_spots) > 1) "the shops" else shopping_spots$title[1]),
             places = shopping_spots)
      } else list(activity = "exploring", venue = "the area", description = "wander and explore", places = neighborhood_places[1, , drop = FALSE])
    } else if (mode == "📸 Photography") {
      photo_spots <- neighborhood_places[grepl("park|bridge|view|historic|architecture|mural|art", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      if (nrow(photo_spots) > 0) {
        chosen_spot <- photo_spots[sample(nrow(photo_spots), 1), , drop = FALSE]
        list(activity = "photography", venue = chosen_spot$title, description = paste("go to", chosen_spot$title, "and take photos"), places = chosen_spot)
      } else list(activity = "photography", venue = "around the neighborhood", description = "walk around and take photos", places = neighborhood_places[1, , drop = FALSE])
    } else {
      list(activity = "exploring", venue = "the area", description = "explore what's available", places = neighborhood_places[1, , drop = FALSE])
    }
  } else {
    if (any(grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE))) {
      cafe_spots <- neighborhood_places[grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      chosen_cafe <- cafe_spots[sample(nrow(cafe_spots), 1), , drop = FALSE]
      list(activity = "coffee and reading", venue = chosen_cafe$title, description = paste("get coffee at", chosen_cafe$title, "and read"), places = chosen_cafe)
    } else if (any(grepl("bookstore", neighborhood_places$tags, ignore.case = TRUE))) {
      bookstore <- neighborhood_places[grepl("bookstore", neighborhood_places$tags, ignore.case = TRUE), ][1, , drop = FALSE]
      list(activity = "book browsing", venue = bookstore$title, description = paste("browse books at", bookstore$title), places = bookstore)
    } else if (any(grepl("park|nature", neighborhood_places$tags, ignore.case = TRUE))) {
      park <- neighborhood_places[grepl("park|nature", neighborhood_places$tags, ignore.case = TRUE), ][1, , drop = FALSE]
      list(activity = "nature walk", venue = park$title, description = paste("take a walk through", park$title), places = park)
    } else if (any(grepl("thrift|vintage", neighborhood_places$tags, ignore.case = TRUE))) {
      thrift <- neighborhood_places[grepl("thrift|vintage", neighborhood_places$tags, ignore.case = TRUE), ][1, , drop = FALSE]
      list(activity = "thrifting", venue = thrift$title, description = paste("browse vintage finds at", thrift$title), places = thrift)
    } else {
      chosen_place <- neighborhood_places[sample(nrow(neighborhood_places), 1), , drop = FALSE]
      list(activity = "exploring", venue = chosen_place$title, description = paste("visit", chosen_place$title), places = chosen_place)
    }
  }
  
  estimated_time <- if (activity_plan$activity %in% c("reading", "coffee and reading")) {
    "2-3 hours"
  } else if (activity_plan$activity %in% c("shopping", "thrifting")) {
    "1-3 hours"
  } else if (activity_plan$activity %in% c("photography", "nature walk")) {
    "1-2 hours"
  } else "1-2 hours"
  
  plan_title <- paste("🏡", chosen_neighborhood)
  plan_description <- paste(transit_mode, "to", chosen_neighborhood, "and", activity_plan$description)
  list(list(
    type = "structured",
    title = plan_title,
    description = plan_description,
    neighborhood = chosen_neighborhood,
    transit = transit_mode,
    activity = activity_plan$activity,
    places = activity_plan$places,
    estimated_time = estimated_time
  ))
}

generate_surprise_adventure <- function(available_places, time_available = "quick", context = NULL) {
  if (nrow(available_places) == 0) return(list())
  adventure_types <- list(
    "Coffee + Photography" = list(
      activities = c("coffee", "photography"),
      venues = list(c("coffee", "cafe"), c("park", "bridge", "view", "mural", "art")),
      description_templates = c(
        "grab coffee at {venue1}, then take film photos of the area around {venue2}",
        "start with coffee at {venue1} and wander to {venue2} to take film photos of the area"
      )
    ),
    "Bike + Shopping" = list(
      activities = c("biking", "shopping"),
      venues = list(c("bike", "trail", "path"), c("thrift", "vintage", "record", "bookstore")),
      description_templates = c(
        "bike to {venue1}, then browse {venue2}",
        "cycle around {venue1} area and check out {venue2}"
      )
    ),
    "Draw + Coffee" = list(
      activities = c("drawing", "coffee"),
      venues = list(c("park", "garden", "scenic", "view"), c("coffee", "cafe")),
      description_templates = c(
        "sketch at {venue1}, then get coffee at {venue2}",
        "find a spot to draw at {venue1} and refuel at {venue2}"
      )
    ),
    "Thrift + Food" = list(
      activities = c("shopping", "food"),
      venues = list(c("thrift", "vintage", "shop"), c("food", "restaurant", "bar", "bakery")),
      description_templates = c(
        "hunt for treasures at {venue1}, then grab a bite at {venue2}",
        "browse {venue1} and treat yourself at {venue2}"
      )
    ),
    "Music + Drinks" = list(
      activities = c("music", "drinks"),
      venues = list(c("record", "music", "vinyl"), c("bar", "brewery", "pub")),
      description_templates = c(
        "dig through records at {venue1}, then drinks at {venue2}",
        "browse music at {venue1} and unwind at {venue2}"
      )
    ),
    "Nature + Reading" = list(
      activities = c("nature", "reading"),
      venues = list(c("park", "garden", "trail", "nature"), c("bookstore", "library", "cafe")),
      description_templates = c(
        "take a nature walk at {venue1}, then read at {venue2}",
        "explore {venue1} and settle in with a book at {venue2}"
      )
    )
  )
  suitable_adventures <- adventure_types
  if (!is.null(context)) {
    if (context == "☔ Rainy Weather") {
      suitable_adventures <- suitable_adventures[!names(suitable_adventures) %in% c("Bike + Shopping", "Nature + Reading")]
    } else if (context == "😴 Low Energy") {
      suitable_adventures <- suitable_adventures[!names(suitable_adventures) %in% c("Bike + Shopping")]
    }
  }
  if (length(suitable_adventures) == 0) suitable_adventures <- adventure_types
  
  adventure_name <- sample(names(suitable_adventures), 1)
  adventure <- suitable_adventures[[adventure_name]]
  
  venues_found <- list()
  for (i in seq_along(adventure$venues)) {
    venue_tags <- adventure$venues[[i]]
    matching_places <- available_places[
      grepl(paste(venue_tags, collapse = "|"), available_places$tags, ignore.case = TRUE),
      , drop = FALSE]
    if (nrow(matching_places) > 0) {
      venues_found[[i]] <- matching_places[sample(nrow(matching_places), 1), , drop = FALSE]
    }
  }
  if (length(venues_found) >= 2) {
    venue1 <- venues_found[[1]]; venue2 <- venues_found[[2]]
    template <- sample(adventure$description_templates, 1)
    description <- gsub("\\{venue1\\}", venue1$title, template)
    description <- gsub("\\{venue2\\}", venue2$title, description)
    neighborhood <- if (!is.na(venue1$neighborhood)) venue1$neighborhood else "Portland"
    avg_distance <- safe_mean(c(venue1$distance_mi, venue2$distance_mi))
    if (!is.null(context)) {
      if (context == "☔ Rainy Weather") {
        transit_mode <- "🚗 Drive"
      } else if (context == "😴 Low Energy" && avg_distance > 1) {
        transit_mode <- "🚗 Drive"
      } else {
        if (avg_distance <= 2) transit_mode <- "🚶 Walk"
        else if (avg_distance <= 8 && context != "😴 Low Energy") transit_mode <- "🚲 Bike"
        else transit_mode <- "🚗 Drive"
      }
    } else {
      transit_mode <- if (avg_distance <= 2) "🚶 Walk" else if (avg_distance <= 8) "🚲 Bike" else "🚗 Drive"
    }
    plan <- list(
      type = "adventure",
      title = paste("🎲", adventure_name, "Adventure"),
      description = description,
      neighborhood = neighborhood,
      transit = transit_mode,
      activity = adventure_name,
      places = rbind(venue1, venue2),
      estimated_time = if (time_available == "quick") "2-3 hours" else "3-5 hours"
    )
    if (time_available %in% c("half_day","full_day")) {
      food_places <- available_places[
        grepl("coffee|cafe|food|bakery|restaurant", available_places$tags, ignore.case = TRUE),
        , drop = FALSE]
      if (nrow(food_places) > 0) {
        food_venue <- food_places[sample(nrow(food_places), 1), , drop = FALSE]
        plan$description <- paste(plan$description, "and finish with a stop at", food_venue$title)
        plan$places <- rbind(plan$places, food_venue)
        plan$estimated_time <- if (time_available == "half_day") "3-4 hours" else "5-6 hours"
      }
    }
    return(list(plan))
  }
  interesting_tags <- c("vintage", "record", "bookstore", "gallery", "museum", "brewery", "coffee")
  interesting_places <- available_places[
    grepl(paste(interesting_tags, collapse = "|"), available_places$tags, ignore.case = TRUE),
    , drop = FALSE]
  if (nrow(interesting_places) > 0) {
    chosen_place <- interesting_places[sample(nrow(interesting_places), 1), , drop = FALSE]
    fallback_activities <- c("explore", "discover", "check out", "investigate", "wander around")
    activity_word <- sample(fallback_activities, 1)
    transit_mode <- if (is.na(chosen_place$distance_mi)) {
      "🚗 Drive"
    } else if (chosen_place$distance_mi <= 2) {
      "🚶 Walk"
    } else if (chosen_place$distance_mi <= 8) {
      "🚲 Bike"
    } else {
      "🚗 Drive"
    }
    return(list(list(
      type = "adventure",
      title = "🎲 Mystery Adventure",
      description = paste(activity_word, chosen_place$title),
      neighborhood = if (!is.na(chosen_place$neighborhood)) chosen_place$neighborhood else "Portland",
      transit = transit_mode,
      activity = "exploration",
      places = chosen_place,
      estimated_time = "1-2 hours"
    )))
  }
  return(list())
}

# ---------------- UI ----------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      :root{ --bg:#f6f7f9; --bg-grad:#eef2f6; --card:#ffffff; --border:#e5e7eb; --text:#111827; --muted:#6b7280; --accent:#0ea5a8; --accent-600:#0b8f92; --accent-50:#e6f7f7; --danger:#dc2626; --success:#16a34a; --warning:#f59e0b; }
      body { font-family:'Inter',sans-serif !important; background:linear-gradient(135deg,var(--bg) 0%,var(--bg-grad) 100%); color:var(--text); margin:0; min-height:100vh;}
      .header { background:var(--card); color:var(--text); padding:28px; margin:-15px -15px 16px -15px; box-shadow: 0 2px 0 0 var(--border), 0 8px 24px rgba(0,0,0,0.04); border-radius:0 0 18px 18px;}
      .header h1 { font-weight:700; margin:0 0 6px 0; font-size:2rem; letter-spacing:-0.01em;}
      .header p { color:var(--muted); margin:0;}
      .header-with-image { display:flex; align-items:center; gap:18px; }
      .header-logo { width:72px; height:auto; border-radius:12px; box-shadow:0 6px 16px rgba(0,0,0,.08); }

      .explainer { background:var(--card); border:1.5px solid var(--border); border-radius:16px;
                   padding:16px 18px; box-shadow:0 8px 24px rgba(0,0,0,.05); margin:0 0 18px 0; }
      .explainer h4 { margin:0 0 6px 0; font-weight:650; }
      .explainer .muted { color:var(--muted); }

      .control-panel { background:var(--card); border-radius:16px; padding:22px; border:1.5px solid var(--border); box-shadow:0 6px 18px rgba(0,0,0,0.05);}
      .control-panel h4, .control-panel h5 { color:var(--text); font-weight:600; margin:6px 0 8px 0; font-size:1.02em; letter-spacing:-0.01em; }

      .activity-btn { margin:4px; padding:10px 14px; border-radius:14px; border:1.5px solid var(--border); background:#fff; color:#334155; cursor:pointer; font-size:13px; font-weight:500; transition:all .18s ease; display:inline-block;}
      .activity-btn:hover { border-color:var(--accent); transform:translateY(-1px); box-shadow:0 3px 10px rgba(14,165,168,.15); background:#fafafa;}
      .activity-btn.active { background:var(--accent); color:#fff; border-color:var(--accent); box-shadow:0 4px 12px rgba(14,165,168,.35);}

      .transport-btn { margin:4px; padding:11px 16px; border-radius:14px; border:1.5px solid var(--border); background:#fff; cursor:pointer; font-weight:600; transition:all .18s ease; color:#334155;}
      .transport-btn:hover { border-color:var(--accent); transform:translateY(-1px); box-shadow:0 3px 10px rgba(14,165,168,.15);}
      .transport-btn.active { background:var(--accent-50); border-color:var(--accent); color:#0b8f92; box-shadow:0 3px 12px rgba(14,165,168,.18);}

      .suggestion-box { background:#fff; border-radius:14px; padding:20px; border-left:4px solid var(--accent); margin:18px 0; box-shadow:0 8px 20px rgba(0,0,0,.06); border:1px solid var(--border);}

      .dataTables_wrapper { background:#fff; border-radius:14px; padding:16px; box-shadow:0 8px 20px rgba(0,0,0,.06); border:1px solid var(--border);}
      .leaflet-container { border-radius:14px; box-shadow:0 8px 20px rgba(0,0,0,.06);}

      /* Tighten label spacing specifically under Sextant and Neighborhood */
      #sextant_label, #neighborhood_label { margin-bottom: 4px !important; }
    "))
  ),
  
  # Header with embedded image
  div(class = "header",
      div(class = "header-with-image",
          if (!is.null(HEADER_IMG))
            tags$img(src = HEADER_IMG, alt = "Portland", class = "header-logo"),
          div(
            h1("Portland Day-Off Planner"),
            p(textOutput("home_info", inline = TRUE))
          )
      )
  ),
  
  # New explainer card under the header
  uiOutput("explainer_ui"),
  
  fluidRow(
    column(
      4,
      div(class = "control-panel",
          h5("📍 Your Address"),
          div(style = "display:flex; gap:10px; margin-bottom:10px;",
              textInput("home_address", "", placeholder = "Enter your address (e.g., 123 Main St, Portland, OR)", value = "", width = "100%"),
              actionButton("geocode_address", "Set Location", class = "btn-outline-primary", style = "min-width: 120px;")
          ),
          div(id = "address_status", style = "margin-bottom: 8px; font-size: 12px; color: #666;"),
          
          div(style = "text-align:center; margin-bottom:14px;",
              actionButton("random_inspiration", "Random Suggestion", class = "btn-info", style = "width:100%;")
          ),
          
          h5("🌦️ Context"),
          selectizeInput("context_filter", "", choices = names(CONTEXT_FILTERS), selected = NULL, multiple = TRUE,
                         options = list(placeholder = 'Any context'), width = "100%"),
          
          h5("⏰ Time Available"),
          selectInput("time_filter", "",
                      choices = list("Quick (1-2 hours)" = "quick",
                                     "Half day (3-4 hours)" = "half_day",
                                     "Full day (5+ hours)" = "full_day"),
                      selected = "quick", width = "100%"),
          
          h5(id="sextant_label","🧭 Sextant"),
          uiOutput("section_selector"),
          
          h5(id="neighborhood_label","🏘️ Neighborhood"),
          uiOutput("neighborhood_selector"),
          
          br(),
          h4("🏪 What kind of places?"),
          div(id = "activity_buttons",
              lapply(names(ACTIVITY_CATEGORIES), function(cat) {
                actionButton(paste0("act_", gsub("[^A-Za-z0-9]", "", cat)), cat, class = "activity-btn")
              })
          ),
          
          br(),
          h5("🎯 What do you want to do?"),
          div(id = "activity_mode_buttons",
              lapply(names(ACTIVITY_MODES), function(mode) {
                actionButton(paste0("mode_", gsub("[^A-Za-z0-9]", "", mode)), mode, class = "activity-btn")
              })
          ),
          
          br(), br(),
          h5("🚶 How are you getting there?"),
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
          h5("✅ Places Visited"),
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
  
  # JS: toggle active classes + sync selected arrays + rainy context hides certain modes
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

# ---------------- SERVER ----------------
# Glue helpers required by server
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

server <- function(input, output, session) {
  
  # ---------- Helpers (scoped to server) ----------
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
    if (!is.na(sf::st_crs(sect_sf)) && !is.na(sf::st_crs(neigh_sf)) &&
        sf::st_crs(sect_sf) != sf::st_crs(neigh_sf)) {
      neigh_sf <- sf::st_transform(neigh_sf, sf::st_crs(sect_sf))
    }
    suppressWarnings({ idx <- sf::st_intersects(neigh_sf, sect_sf, sparse = FALSE) })
    which(rowSums(idx) > 0)
  }
  
  # ---------- State ----------
  values <- reactiveValues(
    completed = load_completed(),
    suggested = NULL,
    inspiration_text = NULL,
    home_lat = DEFAULT_LAT,
    home_lng = DEFAULT_LNG,
    home_address = DEFAULT_ADDRESS
  )
  
  # ---------- Header info ----------
  output$home_info <- renderText({
    paste0("Home base: ", values$home_address, " • Loaded ", nrow(places), " places")
  })
  
  # ---------- Geocoding ----------
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
  
  # ---------- Distances ----------
  places_with_distances <- reactive({
    df <- places
    if (!is.null(values$home_lat) && !is.null(values$home_lng)) {
      df$distance_mi <- mapply(function(lat, lng) {
        calc_distance_miles(values$home_lat, values$home_lng, lat, lng)
      }, df$lat, df$lng)
    }
    df
  })
  
  # ---------- Sextant (left) ----------
  output$section_selector <- renderUI({
    sextant_choices <- get_sextant_choices(places, sections_boundaries, SEC_NAME_COL)
    selectizeInput("section_filter", "", choices = sextant_choices, selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Choose Sextant(s)"))
  })
  observeEvent(input$section_filter, {
    updateSelectizeInput(session, "neighborhood_filter", selected = character(0))
  }, ignoreInit = TRUE)
  
  # ---------- Neighborhood (left) ----------
  output$neighborhood_selector <- renderUI({
    if (is.null(input$section_filter) || !length(input$section_filter) ||
        is.null(sections_boundaries) || is.null(SEC_NAME_COL) ||
        is.null(neighborhood_boundaries) || is.null(NEI_NAME_COL)) {
      return(selectizeInput("neighborhood_filter", "", choices = character(0), selected = NULL, multiple = TRUE,
                            options = list(placeholder = "Select a Sextant first")))
    }
    sel_secs <- sections_boundaries[sections_boundaries[[SEC_NAME_COL]] %in% input$section_filter, , drop = FALSE]
    rows_to_draw <- safe_st_intersects_rows(neighborhood_boundaries, sel_secs)
    available_neighborhoods <- if (length(rows_to_draw)) {
      neighborhood_boundaries[rows_to_draw, ][[NEI_NAME_COL]] |> as.character() |> unique() |> sort()
    } else character(0)
    selectizeInput("neighborhood_filter", "", choices = available_neighborhoods, selected = NULL, multiple = TRUE,
                   options = list(placeholder = if (length(available_neighborhoods)) "Choose Neighborhoods (optional)" else "No Neighborhoods in the selected Sextant(s)"))
  })
  
  # ---------- Filtering ----------
  filtered_places <- reactive({
    df <- places_with_distances()
    if (!is.null(input$section_filter) && length(input$section_filter) > 0 && ("section" %in% names(df))) {
      wanted <- normalize_sextant(input$section_filter)
      df$section_norm <- normalize_sextant(df$section)
      df <- df[!is.na(df$section_norm) & df$section_norm %in% wanted, , drop = FALSE]
    }
    if (!is.null(input$neighborhood_filter) && length(input$neighborhood_filter) > 0 && ("neighborhood" %in% names(df))) {
      df <- df[!is.na(df$neighborhood) & df$neighborhood %in% input$neighborhood_filter, , drop = FALSE]
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
        if ("max_distance" %in% names(context)) {
          df <- df[!is.na(df$distance_mi) & df$distance_mi <= context$max_distance, , drop = FALSE]
        }
      }
    }
    if (!is.null(input$selected_transport) && nzchar(input$selected_transport)) {
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
  
  # ---------- Suggestions ----------
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
    adventures <- generate_surprise_adventure(all_available, input$time_filter, context_for_adventure)
    if (length(adventures) > 0 && !is.null(adventures[[1]])) {
      adventure <- adventures[[1]]
      values$suggested <- adventure$places[1, , drop = FALSE]
      values$inspiration_text <- list(
        title = adventure$title, description = adventure$description, type = adventure$type,
        estimated_time = adventure$estimated_time, transit = adventure$transit, neighborhood = adventure$neighborhood
      )
    } else {
      interesting_places <- all_available[grepl("coffee|vintage|record|bookstore|gallery|museum|brewery", all_available$tags, ignore.case = TRUE), , drop = FALSE]
      if (nrow(interesting_places) > 0) {
        chosen_place <- interesting_places[sample(nrow(interesting_places), 1), , drop = FALSE]
        activities <- c("explore", "discover", "check out", "investigate", "wander around")
        activity <- sample(activities, 1)
        values$suggested <- chosen_place
        values$inspiration_text <- list(
          title = "🎲 Mystery Adventure",
          description = paste(activity, chosen_place$title, "and see what happens!"),
          type = "adventure",
          estimated_time = "1-2 hours"
        )
      } else {
        values$suggested <- all_available[sample(nrow(all_available), 1), , drop = FALSE]
        values$inspiration_text <- list(
          title = "🎲 Random Adventure",
          description = "Go explore this place and see what happens!",
          type = "random",
          estimated_time = "1-3 hours"
        )
      }
    }
  })
  
  # ---------- Visited toggles ----------
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
  
  # ---------- Map base ----------
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = values$home_lng, lat = values$home_lat, zoom = 11)
  })
  
  # ---------- Map layers ----------
  observe({
    filtered <- filtered_places()
    visited <- places[places$id %in% values$completed, , drop = FALSE]
    suggested <- values$suggested
    
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    
    # Sextants layer
    if (!is.null(sections_boundaries) && !is.null(SEC_NAME_COL) && SEC_NAME_COL %in% names(sections_boundaries)) {
      secs_all <- sections_boundaries
      sx_names_raw <- as.character(secs_all[[SEC_NAME_COL]]); sx_names <- normalize_sextant(sx_names_raw)
      selected <- isolate(input$section_filter); selected <- if (is.null(selected)) character(0) else normalize_sextant(selected)
      section_colors <- c(
        "Southwest" = "#ff6b6b", "Northwest" = "#4ecdc4", "Southeast" = "#45b7d1",
        "Northeast" = "#96ceb4", "North" = "#feca57", "South" = "#fd79a8", "Other" = "#a29bfe"
      )
      for (i in seq_along(sx_names)) {
        sx <- sx_names[i]; is_selected <- sx %in% selected
        base_color <- section_colors[[sx]] %||% section_colors[["Other"]]
        proxy <- proxy %>% addPolygons(
          data = secs_all[i, ],
          fillColor = base_color, fillOpacity = if (is_selected) 0.08 else 0.03,
          color = base_color, weight = if (is_selected) 2.5 else 1.5, opacity = if (is_selected) 0.9 else 0.7,
          group = "sextants", layerId = paste0("sextant::", sx),
          options = pathOptions(interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
          highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.25, bringToFront = FALSE),
          popup = paste0("<b>", sx, "</b><br>", if (is_selected) "Click to remove" else "Click to add")
        )
      }
    }
    
    # Neighborhoods layer — ONLY when ≥1 Sextant selected
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
            options = pathOptions(interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
            highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.25, bringToFront = TRUE),
            popup = paste0("<b>", nb_name, "</b><br>", if (is_selected) "Click to remove" else "Click to add")
          )
        }
      }
    }
    
    # Home marker
    proxy <- proxy %>% addCircleMarkers(
      lng = values$home_lng, lat = values$home_lat, label = "Home", popup = values$home_address,
      radius = 8, color = "#16a34a", fillColor = "#16a34a", opacity = 1, fillOpacity = 0.9
    )
    
    # Filtered places
    if (nrow(filtered) > 0) {
      proxy <- proxy %>% addCircleMarkers(
        lng = filtered$lng, lat = filtered$lat, radius = 6,
        color = "#0ea5a8", fillColor = "#99f6e4", opacity = 0.9, fillOpacity = 0.6,
        popup = paste0("<b>", filtered$title, "</b><br>", filtered$tags, "<br>", round(filtered$distance_mi, 1), " miles from home"),
        options = markerOptions(interactive = TRUE, zIndexOffset = 1000)
      )
    }
    
    # Visited places
    if (nrow(visited) > 0) {
      proxy <- proxy %>% addCircleMarkers(
        lng = visited$lng, lat = visited$lat, radius = 5,
        color = "#9ca3af", fillColor = "#9ca3af", opacity = 0.9, fillOpacity = 0.7,
        options = markerOptions(interactive = TRUE, zIndexOffset = 1100)
      )
    }
    
    # Suggested place
    if (!is.null(suggested)) {
      proxy <- proxy %>% addCircleMarkers(
        lng = suggested$lng, lat = suggested$lat, radius = 12,
        color = "#dc2626", fillColor = "#dc2626", opacity = 1, fillOpacity = 0.85,
        options = markerOptions(interactive = TRUE, zIndexOffset = 1200)
      ) %>% setView(lng = suggested$lng, lat = suggested$lat, zoom = 15)
    }
  })
  
  # ---------- Map clicks: toggle & sync left inputs ----------
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click; if (is.null(click$id) || is.null(click$group)) return(NULL)
    grp <- click$group
    if (grp == "sextants") {
      raw_name <- sub("^sextant::", "", click$id); sec_name <- normalize_sextant(raw_name)
      sx_choices <- get_sextant_choices(places, sections_boundaries, SEC_NAME_COL)
      if (!sec_name %in% sx_choices) { showNotification(paste("Unrecognized Sextant:", raw_name), type = "warning"); return(NULL) }
      cur <- isolate(input$section_filter); if (is.null(cur)) cur <- character(0); cur <- normalize_sextant(cur)
      if (sec_name %in% cur) cur <- setdiff(cur, sec_name) else cur <- c(cur, sec_name)
      updateSelectizeInput(session, "section_filter", choices = sx_choices, selected = cur, server = TRUE)
      updateSelectizeInput(session, "neighborhood_filter", choices = character(0), selected = character(0), server = TRUE)
      showNotification(paste("🧭 Sextant:", if (length(cur)) paste(cur, collapse = ", ") else "Any"), type = "message")
      return(invisible(NULL))
    }
    if (grp == "neighborhoods") {
      cur_sx <- isolate(input$section_filter)
      if (is.null(cur_sx) || !length(cur_sx)) { showNotification("Select a Sextant first to choose Neighborhoods.", type = "message"); return(NULL) }
      nb <- sub("^neigh::", "", click$id)
      cur <- isolate(input$neighborhood_filter); if (is.null(cur)) cur <- character(0)
      if (nb %in% cur) cur <- setdiff(cur, nb) else cur <- c(cur, nb)
      updateSelectizeInput(session, "neighborhood_filter", selected = cur, server = TRUE)
      showNotification(paste("🏘️ Neighborhood:", if (length(cur)) paste(cur, collapse = ", ") else "Any"), type = "message")
    }
  }, ignoreInit = TRUE)
  
  # ---------- Weather + Explainer ----------
  current_weather <- reactive({ get_weather_forecast() %||% NULL })
  
  observe({
    weather <- current_weather()
    if (!is.null(weather) && isTRUE(weather$is_rainy) &&
        (is.null(input$context_filter) || length(input$context_filter) == 0)) {
      updateSelectizeInput(session, "context_filter", selected = "☔ Rainy Weather")
      showNotification("☔ Rainy weather detected - filtering to indoor activities", type = "message")
    }
  })
  
  output$explainer_ui <- renderUI({
    w <- current_weather(); time_str <- pdx_time_string()
    div(class = "explainer",
        h4("🧭 Today’s Explorer Panel"),
        div(class = "muted",
            if (!is.null(w) && isTRUE(w$success)) {
              tags$span(
                strong("Portland time: "), time_str, " • ",
                strong("Weather: "), w$condition, " • ",
                strong("Temp: "), w$temperature
              )
            } else {
              tags$span(strong("Portland time: "), time_str, " • Weather unavailable")
            }
        ),
        div(style = "margin-top:6px; color:#475569;",
            "Pick a Sextant on the map (or from the left), then choose Neighborhoods. ",
            "Add a vibe (activities/modes), and I’ll suggest a plan. ",
            "Walk/Bike/Drive limits the distance from your home base."
        )
    )
  })
  
  # ---------- Suggestion display ----------
  output$suggestion_display <- renderUI({
    if (is.null(values$suggested)) {
      div(class = "suggestion-box",
          h4("🎯 Ready to explore?"),
          p("Use 'Get Suggestions' for an intelligent plan, or 'Random Suggestion' for spontaneous ideas!")
      )
    } else {
      place <- values$suggested
      clean_tags_display <- clean_tags(place$tags)
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
          p(strong("Distance: "), place$distance_mi, " miles from home"),
          if (!is.na(place$neighborhood)) p(strong("Neighborhood: "), place$neighborhood),
          if (nzchar(place$note)) p(strong("Note: "), place$note),
          if (nzchar(place$url)) tags$a("📍 View on Google Maps", href = place$url, target = "_blank")
      )
    }
  })
  
  # ---------- Table & visited ----------
  output$places_table <- DT::renderDataTable({
    df <- filtered_places()
    if (nrow(df) == 0) return(NULL)
    
    df$clean_tags <- sapply(df$tags, clean_tags)
    
    display_df <- df %>%
      mutate(distance_mi = round(distance_mi, 1)) %>%
      dplyr::select(dplyr::any_of(c("title", "clean_tags", "feature", "neighborhood", "distance_mi"))) %>%
      dplyr::rename(
        Place = title,
        Tags = clean_tags,
        Neighborhood = neighborhood,
        `Distance (mi)` = distance_mi
      )
    
    # Fallback if 'feature' column was absent and we ended up with only 3-4 cols:
    if (!"feature" %in% names(df) && !"Feature" %in% names(display_df)) {
      # nothing to do — we simply won't show the Feature column
    } else {
      names(display_df)[names(display_df) == "feature"] <- "Feature"
    }
    
    DT::datatable(
      display_df,
      options = list(pageLength = 8, scrollX = TRUE, dom = 'tip')
    )
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
