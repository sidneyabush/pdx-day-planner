# portland_day_planner.R — Portland Day-Off Planner (Modern UI, Sextant/Neighborhood flow)
# WORKFLOW:
# 1) Run source("process_data.R") once to build data/portland_places_processed.rds
# 2) Run source("portland_day_planner.R") to launch the app
# 3) Add new places? Re-run step 1, then step 2

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
})

`%||%` <- function(a,b) if(!is.null(a)) a else b
safe_mean <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) return(Inf); mean(x) }

# ---------------- DEFAULT CONFIG ----------------
DEFAULT_ADDRESS <- "Portland, OR"
DEFAULT_LAT <- 45.5152
DEFAULT_LNG <- -122.6784
SEXTANT_LEVELS <- c("North","Northeast","Northwest","Southeast","Southwest","South")

# ---------------- Weather ----------------
get_weather_forecast <- function() {
  tryCatch({
    url <- "http://wttr.in/Portland,OR?format=%C+%t+%h+%w+%S+%s"
    response <- readLines(url, warn = FALSE)
    parts <- strsplit(response[1], " ")[[1]]
    if (length(parts) >= 4) {
      condition <- parts[1]; temp <- parts[2]; humidity <- parts[3]
      wind <- paste(parts[4:length(parts)], collapse = " ")
      is_rainy <- grepl("rain|drizzle|shower|storm", tolower(condition)) ||
        grepl("☔|🌧️|⛈️|🌦️", condition)
      return(list(condition=condition, temperature=temp, humidity=humidity,
                  wind=wind, is_rainy=is_rainy, success=TRUE))
    }
    list(success = FALSE)
  }, error = function(e) list(success = FALSE, error = e$message))
}

# ---------------- Activities/Filters ----------------
ACTIVITY_CATEGORIES <- list(
  "☕️ Coffee & Cafes" = c("coffee","cafe","espresso","latte"),
  "🍰 Sweet Treats" = c("sweet treats","bagels","dessert","bakery","sweet","donut","pastry"),
  "🍃 Food & Restaurants" = c("vegan","breakfast","food carts","restaurant","food","lunch","dinner","brunch"),
  "🍸 Drinks & Bars" = c("beer","cocktails","bar","brewery","drinks","wine","spirits"),
  "📚 Bookstores" = c("bookstore","books","reading","literature"),
  "💽 Music & Records" = c("record stores","music","records","vinyl","cd"),
  "🏷️ Thrift & Vintage" = c("thrift","vintage","antique","secondhand","consignment"),
  "📔 Stationery & Art" = c("stationery","art supplies","paper","pens","notebooks"),
  "🛴 Fun Shopping" = c("fun stores","toys","games","gifts","novelty"),
  "🌲 Parks & Nature" = c("park","garden","nature","trail","hike","outdoor","forest"),
  "🥕 Markets" = c("farmers markets","market","farmers","produce","fresh food","grocery"),
  "🎭 Entertainment" = c("movies","theater","entertainment","cinema","show","performance"),
  "🖋️ Creative" = c("tattoo","art studio","art")
)
TRANSPORT_MODES <- list("🚶 Walking"=2, "🚲 Biking"=10, "🚌 Public Transit"=15, "🚗 Driving"=30, "🌍 Any Distance"=999)
ACTIVITY_MODES <- list(
  "📖 Reading" = c("coffee","cafe","bookstore","quiet","library","park"),
  "🎨 Drawing/Sketching" = c("coffee","cafe","park","outdoor","scenic","garden","museum"),
  "🚲 Biking Route" = c("trail","path","route","bike path","greenway"),
  "🥾 Hiking" = c("trail","hike","nature","forest","mountain","waterfall"),
  "📸 Photography" = c("scenic","architecture","vintage","historic","art","bridge","view","mural"),
  "🚶 Walking Tour" = c("neighborhood","historic","architecture","street art","district"),
  "🛍️ Shopping Spree" = c("thrift","vintage","bookstore","record","shopping","market"),
  "🍽️ Food Adventure" = c("restaurant","food cart","market","bakery","brewery","cafe")
)
CONTEXT_FILTERS <- list(
  "☔ Rainy Weather" = list(
    exclude_activities = c("🚲 Biking Route","🥾 Hiking","🚶 Walking Tour","📸 Photography"),
    exclude_venues = c("🌲 Parks & Nature"),
    hide_activity_buttons = c("🚲 Biking Route","🥾 Hiking","🚶 Walking Tour","📸 Photography"),
    prefer_close = TRUE, max_distance = 3,
    suggestion_prefix = "Stay dry with indoor activities:"
  ),
  "😴 Low Energy" = list(
    exclude_activities = c("🚲 Biking Route","🥾 Hiking"),
    prefer_activities = c("📖 Reading","🎨 Drawing/Sketching"),
    prefer_venues = c("☕️ Coffee & Cafes","📚 Bookstores"),
    prefer_close = TRUE, max_distance = 2,
    suggestion_prefix = "Take it easy with relaxing activities:"
  ),
  "🌞 Perfect Weather" = list(
    prefer_activities = c("🚲 Biking Route","🥾 Hiking","📸 Photography","🚶 Walking Tour"),
    prefer_venues = c("🌲 Parks & Nature"),
    boost_outdoor = TRUE,
    suggestion_prefix = "Beautiful day for outdoor adventures:"
  ),
  "🎯 Focused Mission" = list(
    prefer_close = TRUE, max_distance = 5,
    suggestion_prefix = "Let's find exactly what you're looking for:"
  )
)

# ---------------- Helpers ----------------
geocode_address <- function(address) {
  tryCatch({
    if(is.null(address) || address == "" || nchar(str_trim(address)) == 0)
      return(list(success = FALSE, error = "Please enter an address"))
    url <- paste0("https://nominatim.openstreetmap.org/search?q=", utils::URLencode(str_trim(address)), "&format=json&limit=1")
    response <- httr::GET(url, httr::user_agent("Portland Day Planner App"))
    if(httr::status_code(response) == 200) {
      result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      if(length(result) > 0 && nrow(result) > 0) {
        return(list(success=TRUE, lat=as.numeric(result$lat[1]), lng=as.numeric(result$lon[1]), formatted_address=result$display_name[1]))
      } else return(list(success=FALSE, error="Address not found. Please try a different format."))
    } else list(success=FALSE, error="Geocoding service unavailable. Please try again later.")
  }, error = function(e) list(success=FALSE, error = paste("Error:", e$message)))
}

calc_distance_miles <- function(lat1, lng1, lat2, lng2) {
  if(is.na(lat1) || is.na(lng1) || is.na(lat2) || is.na(lng2)) return(NA)
  R <- 3959
  lat1_rad <- lat1 * pi/180; lat2_rad <- lat2 * pi/180
  dlat <- (lat2 - lat1) * pi/180; dlng <- (lng2 - lng1) * pi/180
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlng/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  round(R * c, 1)
}

matches_activity <- function(tags_text, category_terms) {
  if(is.na(tags_text) || tags_text == "") return(FALSE)
  tags_lower <- tolower(tags_text)
  any(sapply(category_terms, function(term) {
    clean_term <- gsub("[^A-Za-z ]", "", term) |> str_trim() |> tolower()
    if(clean_term != "") stringr::str_detect(tags_lower, fixed(clean_term))
    else stringr::str_detect(tags_text, fixed(term))
  }))
}
matches_activity_mode <- function(title, tags, note, mode_terms) {
  if(is.na(title)) title <- ""; if(is.na(tags)) tags <- ""; if(is.na(note)) note <- ""
  combined_text <- tolower(paste(title, tags, note))
  any(sapply(mode_terms, function(term) {
    clean_term <- gsub("[^A-Za-z ]", "", term) |> str_trim() |> tolower()
    stringr::str_detect(combined_text, fixed(clean_term))
  }))
}

load_completed <- function() { f <- "data/completed_places.rds"; if(file.exists(f)) readRDS(f) else character(0) }
save_completed <- function(ids) { dir.create("data", showWarnings = FALSE); saveRDS(ids, "data/completed_places.rds") }

clean_tags <- function(tags_text) {
  if(is.na(tags_text) || tags_text == "") return("")
  cleaned <- gsub("[^A-Za-z0-9 .,!?()-]", " ", tags_text)
  cleaned <- str_squish(cleaned)
  words <- strsplit(cleaned, " ")[[1]]
  words <- words[nchar(words) > 2 | words %in% c("&","or","of")]
  paste(words, collapse = " ")
}

# Canonicalize Sextant names
canon_sextant <- function(x) {
  x0 <- tolower(trimws(as.character(x)))
  if (grepl("northwest|\\bnw\\b", x0)) return("Northwest")
  if (grepl("northeast|\\bne\\b", x0)) return("Northeast")
  if (grepl("southwest|\\bsw\\b", x0)) return("Southwest")
  if (grepl("southeast|\\bse\\b", x0)) return("Southeast")
  if (grepl("^north(?!east|west)|\\bn\\b", x0, perl = TRUE)) return("North")
  if (grepl("^south(?!east|west)|\\bs\\b", x0, perl = TRUE)) return("South")
  NA_character_
}

# ---------------- Load processed data ----------------
processed_file <- "data/portland_places_processed.rds"
if(!file.exists(processed_file)) { cat("❌ No processed data. Run process_data.R first.\n"); stop("Missing processed data file") }
cat("🚀 Loading Portland Day Planner...\n")
places <- readRDS(processed_file)
if(!is.data.frame(places) || nrow(places) == 0) { cat("❌ Invalid processed data.\n"); stop("Invalid processed data") }

if("title" %in% names(places)) {
  original_count <- nrow(places)
  places <- places[!grepl("^Unnamed place", places$title, ignore.case = TRUE), ]
  removed_count <- original_count - nrow(places)
  if(removed_count > 0) cat("🗑️ Removed", removed_count, "unnamed places\n")
}
cat("✅ Loaded", nrow(places), "places\n")
if("processed_date" %in% names(places)) cat("📅 Data processed on:", as.character(places$processed_date[1]), "\n")

# ---------------- Map data (Neighborhoods + Sextants) ----------------
load_map_file <- function(paths) { for (p in paths) if (file.exists(p)) return(p); NULL }
safe_read_sf <- function(path) {
  if (is.null(path)) return(NULL)
  tryCatch({ sf::st_read(path, quiet = TRUE) },
           error = function(e) { cat("⚠️ Could not load", path, ":", e$message, "\n"); NULL })
}
pick_name_col <- function(sfobj, candidates) { cands <- candidates[candidates %in% names(sfobj)]; if (length(cands)) cands[[1]] else NULL }

# Neighborhood polygons (sub-neighborhoods)
neighborhood_path <- load_map_file(c(
  "archive/Neighborhood_Boundaries.geojson",
  "archive/neighborhoods.geojson",
  "data/Neighborhood_Boundaries.geojson"
))
neighborhood_boundaries <- safe_read_sf(neighborhood_path)
NEI_NAME_COL <- if (!is.null(neighborhood_boundaries)) pick_name_col(neighborhood_boundaries, c("MAPLABEL","NAME","Label","Neighborhood","NEIGHBORHD","neigh","label")) else NULL

# Sextant polygons (administrative sextants)
sextants_path <- load_map_file(c(
  "archive/Portland_Administrative_Sextants.geojson",
  "data/Portland_Administrative_Sextants.geojson"
))
sextant_boundaries <- safe_read_sf(sextants_path)
SEXT_NAME_COL <- if (!is.null(sextant_boundaries)) pick_name_col(sextant_boundaries, c("Sextant","SEXTANT","PREFIX","NAME")) else NULL

# Build Neighborhood -> Sextant mapping for quick filtering
NB_TO_SEXTANT <- NULL
if (!is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL) &&
    !is.null(sextant_boundaries)    && !is.null(SEXT_NAME_COL)) {
  idx_list <- sf::st_intersects(neighborhood_boundaries, sextant_boundaries)
  nb_names <- as.character(neighborhood_boundaries[[NEI_NAME_COL]])
  sext_raw <- as.character(sextant_boundaries[[SEXT_NAME_COL]])
  map_vec <- rep(NA_character_, length(nb_names))
  for (i in seq_along(nb_names)) {
    hits <- idx_list[[i]]
    if (length(hits)) map_vec[i] <- canon_sextant(sext_raw[hits[1]])
  }
  NB_TO_SEXTANT <- setNames(map_vec, nb_names)
}

# ---------------- Generators (plans) ----------------
generate_day_plan <- function(available_places, context = NULL, selected_activities = NULL, selected_modes = NULL, time_available = "quick") {
  if(nrow(available_places) == 0) return(list())
  places_with_neighborhoods <- available_places[!is.na(available_places$neighborhood), ]
  if(nrow(places_with_neighborhoods) == 0) return(list())
  
  neighborhood_counts <- table(places_with_neighborhoods$neighborhood)
  good_neighborhoods <- names(neighborhood_counts[neighborhood_counts >= 1])
  if (length(good_neighborhoods) == 0) return(list())
  
  chosen_neighborhood <- sample(good_neighborhoods, 1)
  neighborhood_places <- places_with_neighborhoods[places_with_neighborhoods$neighborhood == chosen_neighborhood, , drop = FALSE]
  if (nrow(neighborhood_places) == 0) return(list())
  
  avg_distance <- safe_mean(neighborhood_places$distance_mi)
  
  force_drive <- FALSE
  max_allowed_distance <- Inf
  if(!is.null(context)) {
    context_config <- CONTEXT_FILTERS[[context]]
    if(!is.null(context_config)) {
      if(context == "☔ Rainy Weather") {
        force_drive <- TRUE
        max_allowed_distance <- context_config$max_distance %||% 3
      } else if(context == "😴 Low Energy") {
        if(avg_distance > 1) force_drive <- TRUE
        max_allowed_distance <- context_config$max_distance %||% 2
      } else if("max_distance" %in% names(context_config)) {
        max_allowed_distance <- context_config$max_distance
      }
    }
  }
  
  if(force_drive || avg_distance > max_allowed_distance) {
    transit_mode <- "🚗 Driving"
  } else if(avg_distance <= 1) {
    transit_mode <- "🚶 Walking"
  } else if(avg_distance <= 2 && !force_drive) {
    transit_mode <- "🚶 Walking"
  } else if(avg_distance <= 8 && !force_drive && context != "😴 Low Energy") {
    transit_mode <- "🚲 Biking"
  } else {
    transit_mode <- "🚗 Driving"
  }
  
  pick_or_fallback <- function(df, pattern, default_activity, default_desc) {
    spots <- df[grepl(pattern, df$tags, ignore.case = TRUE), , drop = FALSE]
    if (is.data.frame(spots) && nrow(spots) > 0) {
      chosen <- spots[sample(nrow(spots), 1), , drop = FALSE]
      list(
        activity = default_activity,
        venue = chosen$title,
        description = default_desc(chosen$title),
        places = chosen
      )
    } else {
      list(
        activity = default_activity,
        venue = "a spot",
        description = default_desc("a spot"),
        places = df[1, , drop = FALSE]
      )
    }
  }
  
  if(!is.null(selected_modes) && length(selected_modes) > 0) {
    mode <- sample(selected_modes, 1)
    if(mode == "📖 Reading") {
      activity_plan <- pick_or_fallback(
        neighborhood_places,
        "coffee|cafe|bookstore|library|park",
        "reading",
        function(v) paste("go to", v, "and read")
      )
    } else if(mode == "🎨 Drawing/Sketching") {
      activity_plan <- pick_or_fallback(
        neighborhood_places,
        "park|coffee|cafe|garden|scenic|view",
        "sketching",
        function(v) paste("go to", v, "and sketch")
      )
    } else if(mode == "🛍️ Shopping Spree") {
      shopping_spots <- neighborhood_places[grepl("thrift|vintage|bookstore|record|market|shop", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      if (nrow(shopping_spots) > 0) {
        activity_plan <- list(
          activity = "shopping",
          venue = if(nrow(shopping_spots) > 1) "multiple shops" else shopping_spots$title[1],
          description = paste("browse", if(nrow(shopping_spots) > 1) "the shops" else shopping_spots$title[1]),
          places = shopping_spots
        )
      } else {
        activity_plan <- list(
          activity = "exploring",
          venue = "the area",
          description = "wander and explore",
          places = neighborhood_places[1, , drop = FALSE]
        )
      }
    } else if(mode == "📸 Photography") {
      activity_plan <- pick_or_fallback(
        neighborhood_places,
        "park|bridge|view|historic|architecture|mural|art",
        "photography",
        function(v) paste("go to", v, "and take photos")
      )
    } else {
      activity_plan <- list(
        activity = "exploring",
        venue = "the area",
        description = "explore what's available",
        places = neighborhood_places[1, , drop = FALSE]
      )
    }
  } else {
    if(any(grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE))) {
      cafe_spots <- neighborhood_places[grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE), , drop = FALSE]
      chosen_cafe <- cafe_spots[sample(nrow(cafe_spots), 1), , drop = FALSE]
      activity_plan <- list(
        activity = "coffee and reading",
        venue = chosen_cafe$title,
        description = paste("get coffee at", chosen_cafe$title, "and read"),
        places = chosen_cafe
      )
    } else if(any(grepl("bookstore", neighborhood_places$tags, ignore.case = TRUE))) {
      bookstore <- neighborhood_places[grepl("bookstore", neighborhood_places$tags, ignore.case = TRUE), ][1, , drop = FALSE]
      activity_plan <- list(
        activity = "book browsing",
        venue = bookstore$title,
        description = paste("browse books at", bookstore$title),
        places = bookstore
      )
    } else if(any(grepl("park|nature", neighborhood_places$tags, ignore.case = TRUE))) {
      park <- neighborhood_places[grepl("park|nature", neighborhood_places$tags, ignore.case = TRUE), ][1, , drop = FALSE]
      activity_plan <- list(
        activity = "nature walk",
        venue = park$title,
        description = paste("take a walk through", park$title),
        places = park
      )
    } else if(any(grepl("thrift|vintage", neighborhood_places$tags, ignore.case = TRUE))) {
      thrift <- neighborhood_places[grepl("thrift|vintage", neighborhood_places$tags, ignore.case = TRUE), ][1, , drop = FALSE]
      activity_plan <- list(
        activity = "thrifting",
        venue = thrift$title,
        description = paste("browse vintage finds at", thrift$title),
        places = thrift
      )
    } else {
      chosen_place <- neighborhood_places[sample(nrow(neighborhood_places), 1), , drop = FALSE]
      activity_plan <- list(
        activity = "exploring",
        venue = chosen_place$title,
        description = paste("visit", chosen_place$title),
        places = chosen_place
      )
    }
  }
  
  estimated_time <- if(activity_plan$activity %in% c("reading", "coffee and reading")) {
    "2-3 hours"
  } else if(activity_plan$activity %in% c("shopping","thrifting")) {
    "1-3 hours"
  } else if(activity_plan$activity %in% c("photography","nature walk")) {
    "1-2 hours"
  } else {
    "1-2 hours"
  }
  
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
  if(nrow(available_places) == 0) return(list())
  adventure_types <- list(
    "Coffee + Photography" = list(
      activities=c("coffee","photography"),
      venues=list(c("coffee","cafe"), c("park","bridge","view","mural","art")),
      description_templates=c("grab coffee at {venue1}, then take film photos of the area around {venue2}",
                              "start with coffee at {venue1} and wander to {venue2} to take film photos of the area")
    ),
    "Bike + Shopping" = list(
      activities=c("biking","shopping"),
      venues=list(c("bike","trail","path"), c("thrift","vintage","record","bookstore")),
      description_templates=c("bike to {venue1}, then browse {venue2}","cycle around {venue1} area and check out {venue2}")
    ),
    "Draw + Coffee" = list(
      activities=c("drawing","coffee"),
      venues=list(c("park","garden","scenic","view"), c("coffee","cafe")),
      description_templates=c("sketch at {venue1}, then get coffee at {venue2}","find a spot to draw at {venue1} and refuel at {venue2}")
    ),
    "Thrift + Food" = list(
      activities=c("shopping","food"),
      venues=list(c("thrift","vintage","shop"), c("food","restaurant","bar","bakery")),
      description_templates=c("hunt for treasures at {venue1}, then grab a bite at {venue2}","browse {venue1} and treat yourself at {venue2}")
    ),
    "Music + Drinks" = list(
      activities=c("music","drinks"),
      venues=list(c("record","music","vinyl"), c("bar","brewery","pub")),
      description_templates=c("dig through records at {venue1}, then drinks at {venue2}","browse music at {venue1} and unwind at {venue2}")
    ),
    "Nature + Reading" = list(
      activities=c("nature","reading"),
      venues=list(c("park","garden","trail","nature"), c("bookstore","library","cafe")),
      description_templates=c("take a nature walk at {venue1}, then read at {venue2}","explore {venue1} and settle in with a book at {venue2}")
    )
  )
  
  suitable <- adventure_types
  if(!is.null(context)) {
    if(context == "☔ Rainy Weather") suitable <- suitable[!names(suitable) %in% c("Bike + Shopping","Nature + Reading")]
    else if(context == "😴 Low Energy") suitable <- suitable[!names(suitable) %in% c("Bike + Shopping")]
  }
  if(length(suitable) == 0) suitable <- adventure_types
  
  adventure_name <- sample(names(suitable), 1)
  adventure <- suitable[[adventure_name]]
  
  venues_found <- list()
  for(i in seq_along(adventure$venues)) {
    venue_tags <- adventure$venues[[i]]
    matching_places <- available_places[grepl(paste(venue_tags, collapse = "|"), available_places$tags, ignore.case = TRUE), , drop = FALSE]
    if (is.data.frame(matching_places) && nrow(matching_places) > 0) {
      venues_found[[i]] <- matching_places[sample(nrow(matching_places), 1), , drop = FALSE]
    }
  }
  
  if(length(venues_found) >= 2) {
    venue1 <- venues_found[[1]]; venue2 <- venues_found[[2]]
    template <- sample(adventure$description_templates, 1)
    description <- gsub("\\{venue1\\}", venue1$title, template)
    description <- gsub("\\{venue2\\}", venue2$title, description)
    neighborhood <- if(!is.na(venue1$neighborhood)) venue1$neighborhood else "Portland"
    
    avg_distance <- safe_mean(c(venue1$distance_mi, venue2$distance_mi))
    if(!is.null(context)) {
      if(context == "☔ Rainy Weather") transit_mode <- "🚗 Driving"
      else if(context == "😴 Low Energy" && avg_distance > 1) transit_mode <- "🚗 Driving"
      else transit_mode <- if(avg_distance <= 2) "🚶 Walking" else if(avg_distance <= 8) "🚲 Biking" else "🚗 Driving"
    } else {
      transit_mode <- if(avg_distance <= 2) "🚶 Walking" else if(avg_distance <= 8) "🚲 Biking" else "🚗 Driving"
    }
    
    plan_places <- dplyr::bind_rows(venue1, venue2)
    plan <- list(
      type = "adventure",
      title = paste("🎲", adventure_name, "Adventure"),
      description = description,
      neighborhood = neighborhood,
      transit = transit_mode,
      activity = adventure_name,
      places = plan_places,
      estimated_time = if(time_available == "quick") "2-3 hours" else "3-5 hours"
    )
    return(list(plan))
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
      .header { background:var(--card); color:var(--text); padding:28px; margin:-15px -15px 24px -15px; box-shadow: 0 2px 0 0 var(--border), 0 8px 24px rgba(0,0,0,0.04); border-radius:0 0 18px 18px;}
      .header h1 { font-weight:700; margin:0 0 6px 0; font-size:2rem; letter-spacing:-0.01em;} .header p { color:var(--muted); margin:0;}
      .control-panel { background:var(--card); border-radius:16px; padding:22px; border:1px solid var(--border); box-shadow:0 6px 18px rgba(0,0,0,0.05);} .control-panel h4,.control-panel h5 { color:var(--text); font-weight:600; margin-bottom:12px; font-size:1.02em; letter-spacing:-0.01em;}
      .activity-btn { margin:4px; padding:10px 14px; border-radius:14px; border:1.5px solid var(--border); background:#fff; color:#334155; cursor:pointer; font-size:13px; font-weight:500; transition:all .18s ease; display:inline-block;} .activity-btn:hover { border-color:var(--accent); transform:translateY(-1px); box-shadow:0 3px 10px rgba(14,165,168,.15); background:#fafafa;} .activity-btn.active { background:var(--accent); color:#fff; border-color:var(--accent); box-shadow:0 4px 12px rgba(14,165,168,.35);}
      .transport-btn { margin:4px; padding:11px 16px; border-radius:14px; border:1.5px solid var(--border); background:#fff; cursor:pointer; font-weight:600; transition:all .18s ease; color:#334155;} .transport-btn:hover { border-color:var(--accent); transform:translateY(-1px); box-shadow:0 3px 10px rgba(14,165,168,.15);} .transport-btn.active { background:var(--accent-50); border-color:var(--accent); color:var(--accent-600); box-shadow:0 3px 12px rgba(14,165,168,.18);}
      .suggestion-box { background:#fff; border-radius:14px; padding:20px; border-left:4px solid var(--accent); margin:18px 0; box-shadow:0 8px 20px rgba(0,0,0,.06); border:1px solid var(--border);} .suggestion-box h3,.suggestion-box h4 { color:var(--text); margin-top:0; font-weight:700;} .suggestion-box p { color:#374151;}
      .btn-primary { background:var(--accent) !important; border:none !important; border-radius:12px !important; padding:11px 20px !important; font-weight:600 !important; transition:all .18s ease !important;} .btn-primary:hover { transform:translateY(-2px) !important; box-shadow:0 8px 22px rgba(14,165,168,.35) !important;} .btn-info { background:#0284c7 !important; border:none !important; border-radius:12px !important; padding:11px 20px !important; font-weight:600 !important;} .btn-info:hover { transform:translateY(-2px) !important; box-shadow:0 8px 22px rgba(2,132,199,.3) !important;} .btn-success,.btn-warning,.btn-outline-secondary { border-radius:12px !important; font-weight:600 !important; transition:all .18s ease !important;} .btn-success:hover,.btn-warning:hover,.btn-outline-secondary:hover { transform:translateY(-1px) !important;}
      .form-control,.form-select { border-radius:12px !important; border:1.5px solid var(--border) !important; background:#fff !important; color:var(--text) !important; padding:10px 14px !important; transition: box-shadow .18s ease, border-color .18s ease !important;}
      .dataTables_wrapper { background:#fff; border-radius:14px; padding:16px; box-shadow:0 8px 20px rgba(0,0,0,.06); border:1px solid var(--border);}
      .leaflet-container { border-radius:14px; box-shadow:0 8px 20px rgba(0,0,0,.06);}

      /* Tighten label/space under Sextant and Neighborhood */
      #sextant_selector label, #neighborhood_selector label { display: none !important; }
      #sextant_selector .form-group, #neighborhood_selector .form-group { margin-bottom: 6px !important; }
      #sextant_selector .selectize-control, #neighborhood_selector .selectize-control { margin-top: 0 !important; margin-bottom: 0 !important; }
    "))
  ),
  
  div(class = "header",
      h1("Portland Day-Off Planner"),
      p(paste("Home base:", DEFAULT_ADDRESS, "• Loaded", nrow(places), "places"))
  ),
  
  fluidRow(
    column(4,
           div(class = "control-panel",
               h5("📍 Your Address"),
               div(style = "display: flex; gap: 10px; margin-bottom: 20px;",
                   textInput("home_address", "", placeholder = "Enter your address (e.g., 123 Main St, Portland, OR)", value = "", width = "100%"),
                   actionButton("geocode_address", "Set Location", class = "btn-outline-primary", style = "min-width: 120px;")
               ),
               div(id = "address_status", style = "margin-bottom: 15px; font-size: 12px; color: #666;"),
               
               div(style = "text-align: center; margin-bottom: 20px;",
                   actionButton("random_inspiration", "Random Suggestion", class = "btn-info", style = "width: 100%;")
               ),
               
               h5("🌦️ Context (optional)"),
               selectizeInput("context_filter", "Weather/Mood:", choices = names(CONTEXT_FILTERS),
                              selected = NULL, multiple = TRUE,
                              options = list(placeholder = 'Any context'), width = "100%"),
               
               h5("⏰ Time Available"),
               selectInput("time_filter", "How much time?",
                           choices = list("Quick (1-2 hours)" = "quick",
                                          "Half day (3-4 hours)" = "half_day",
                                          "Full day (5+ hours)" = "full_day"),
                           selected = "quick", width = "100%"),
               
               h5("🧭 Sextant"),
               uiOutput("sextant_selector"),
               
               h5("🏘️ Neighborhood"),
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
               div(style = "text-align: center; margin-bottom: 20px;",
                   actionButton("suggest_place", "Get Suggestions", class = "btn-primary", style = "width: 100%;")
               ),
               div(style = "text-align: center;",
                   actionButton("mark_visited", "✅ Mark Visited", class = "btn-success"),
                   actionButton("unmark", "↩️ Undo", class = "btn-warning"),
                   br(), br(),
                   actionButton("reset_visited", "♻️ Reset All", class = "btn-outline-secondary")
               ),
               hr(),
               h5("✅ Places Visited"),
               verbatimTextOutput("visited_count"),
               uiOutput("visited_preview"),
               hr(),
               div(style = "font-size: 11px; color: #666; text-align: center;",
                   "To add new places: Update CSV files, then run process_data.R")
           )
    ),
    column(8,
           div(style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
               div(style = "display: flex; justify-content: space-between; align-items: center;",
                   div(uiOutput("weather_display")),
                   div(style = "text-align: right;",
                       tags$strong("Ready to explore?"),
                       br(),
                       tags$small("Use 'Get Suggestions' for recommendations, or 'Random Suggestion' for something different!")
                   )
               )
           ),
           leafletOutput("map", height = 520),
           br(),
           uiOutput("suggestion_display"),
           br(),
           h5("📋 Filtered Places"),
           DT::dataTableOutput("places_table")
    )
  ),
  
  tags$script(HTML("
    $(document).on('click', '.activity-btn', function() {
      $(this).toggleClass('active');
      var a1 = $('#activity_buttons .activity-btn.active').map(function(){return $(this).text();}).get();
      Shiny.setInputValue('selected_activities', a1);
      var a2 = $('#activity_mode_buttons .activity-btn.active').map(function(){return $(this).text();}).get();
      Shiny.setInputValue('selected_activity_modes', a2);
    });
    $(document).on('click', '.transport-btn', function() {
      if ($(this).hasClass('active')) {
        $(this).removeClass('active');
        Shiny.setInputValue('selected_transport', '');
      } else {
        $('.transport-btn').removeClass('active');
        $(this).addClass('active');
        Shiny.setInputValue('selected_transport', $(this).text());
      }
    });
    $(document).on('change', '#context_filter', function() {
      var contexts = $(this).val(); $('.activity-btn').show();
      if (contexts && contexts.includes('☔ Rainy Weather')) {
        $('#activity_mode_buttons .activity-btn').each(function(){
          var t = $(this).text();
          if (t.includes('Biking') || t.includes('Hiking') || t.includes('Long Walk') || t.includes('Taking Film Photos')) { $(this).hide().removeClass('active'); }
        });
        var active_modes = $('#activity_mode_buttons .activity-btn.active:visible').map(function(){return $(this).text();}).get();
        Shiny.setInputValue('selected_activity_modes', active_modes);
      }
    });
  "))
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  values <- reactiveValues(
    completed = load_completed(),
    suggested = NULL,
    inspiration_text = NULL,
    home_lat = DEFAULT_LAT,
    home_lng = DEFAULT_LNG,
    home_address = DEFAULT_ADDRESS
  )
  
  # Geocoding
  observeEvent(input$geocode_address, {
    if(is.null(input$home_address) || input$home_address == "") {
      output$address_status <- renderText("Please enter an address."); return()
    }
    output$address_status <- renderText("Geocoding address...")
    result <- geocode_address(input$home_address)
    if(result$success) {
      values$home_lat <- result$lat; values$home_lng <- result$lng; values$home_address <- result$formatted_address
      output$address_status <- renderText(paste("Location set:", result$formatted_address))
      showNotification("Address successfully geocoded! Distances updated.", type = "message")
    } else {
      output$address_status <- renderText(paste("Error:", result$error))
      showNotification(paste("Geocoding failed:", result$error), type = "error")
    }
  })
  
  # Distances
  places_with_distances <- reactive({
    df <- places
    if(!is.null(values$home_lat) && !is.null(values$home_lng)) {
      df$distance_mi <- mapply(function(lat, lng) calc_distance_miles(values$home_lat, values$home_lng, lat, lng),
                               df$lat, df$lng)
    }
    df
  })
  
  # ---- Selectors (UI) ----
  output$sextant_selector <- renderUI({
    selectizeInput(
      "sextant_filter",
      label = NULL,
      choices = SEXTANT_LEVELS,   # always the six
      selected = NULL,
      multiple = TRUE,
      options = list(placeholder = 'Choose one or more Sextants')
    )
  })
  
  output$neighborhood_selector <- renderUI({
    # Disabled until a Sextant is chosen
    if (is.null(input$sextant_filter) || length(input$sextant_filter) == 0) {
      return(selectizeInput(
        "neighborhood_filter",
        label = NULL,
        choices = character(0),
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = 'Select a Sextant first',
          onInitialize = I('function(){ this.disable(); }')
        )
      ))
    }
    
    # Available neighborhoods are those whose mapped sextant is in the selected list
    available_neighborhoods <- character(0)
    if (!is.null(NB_TO_SEXTANT)) {
      secs <- intersect(SEXTANT_LEVELS, unique(input$sextant_filter))
      if (length(secs)) {
        available_neighborhoods <- sort(names(NB_TO_SEXTANT)[NB_TO_SEXTANT %in% secs])
      }
    } else if (!is.null(neighborhood_boundaries) && !is.null(sextant_boundaries) &&
               !is.null(NEI_NAME_COL) && !is.null(SEXT_NAME_COL)) {
      # Fallback: spatial filter
      secs_canon <- intersect(SEXTANT_LEVELS, input$sextant_filter)
      keep_rows <- which(vapply(sextant_boundaries[[SEXT_NAME_COL]], canon_sextant, character(1)) %in% secs_canon)
      sel_secs <- sextant_boundaries[keep_rows, , drop = FALSE]
      if (nrow(sel_secs)) {
        idx <- sf::st_intersects(neighborhood_boundaries, sel_secs, sparse = FALSE)
        inter_nb <- neighborhood_boundaries[rowSums(idx) > 0, ]
        if (nrow(inter_nb) > 0) available_neighborhoods <- sort(unique(as.character(inter_nb[[NEI_NAME_COL]])))
      }
    }
    
    current_sel <- input$neighborhood_filter
    if (is.null(current_sel)) current_sel <- character(0)
    current_sel <- intersect(current_sel, available_neighborhoods)
    
    selectizeInput(
      "neighborhood_filter",
      label = NULL,
      choices = available_neighborhoods,
      selected = current_sel,
      multiple = TRUE,
      options = list(
        placeholder = if (length(available_neighborhoods)) 'Choose Neighborhoods (optional)'
        else 'No Neighborhoods in the selected Sextant(s)'
      )
    )
  })
  
  # When Sextants change, clear Neighborhood picks
  observeEvent(input$sextant_filter, {
    updateSelectizeInput(session, "neighborhood_filter", selected = character(0))
  }, ignoreNULL = FALSE)
  
  # ---- Filtering ----
  filtered_places <- reactive({
    df <- places_with_distances()
    
    # Filter by Sextant via Neighborhood->Sextant mapping (preferred)
    if (!is.null(input$sextant_filter) && length(input$sextant_filter) > 0) {
      secs <- intersect(SEXTANT_LEVELS, unique(input$sextant_filter))
      if (length(secs)) {
        if (!is.null(NB_TO_SEXTANT)) {
          df <- df[!is.na(df$neighborhood) & NB_TO_SEXTANT[as.character(df$neighborhood)] %in% secs, , drop = FALSE]
        } else if ("section" %in% names(df)) {
          canon_sections <- vapply(df$section, canon_sextant, character(1))
          df <- df[!is.na(canon_sections) & canon_sections %in% secs, , drop = FALSE]
        }
      }
    }
    
    # Neighborhoods (sub-neighborhoods)
    if(!is.null(input$neighborhood_filter) && length(input$neighborhood_filter) > 0) {
      df <- df[!is.na(df$neighborhood) & df$neighborhood %in% input$neighborhood_filter, , drop = FALSE]
    }
    
    # Venue type categories
    if(!is.null(input$selected_activities) && length(input$selected_activities) > 0) {
      activity_match <- rep(FALSE, nrow(df))
      for(activity in input$selected_activities) if(activity %in% names(ACTIVITY_CATEGORIES)) {
        matches <- sapply(df$tags, function(tags) matches_activity(tags, ACTIVITY_CATEGORIES[[activity]]))
        activity_match <- activity_match | matches
      }
      df <- df[activity_match, , drop = FALSE]
    }
    
    # Activity modes
    if(!is.null(input$selected_activity_modes) && length(input$selected_activity_modes) > 0) {
      mode_match <- rep(FALSE, nrow(df))
      for(mode in input$selected_activity_modes) if(mode %in% names(ACTIVITY_MODES)) {
        matches <- mapply(function(title, tags, note) matches_activity_mode(title, tags, note, ACTIVITY_MODES[[mode]]),
                          df$title, df$tags, df$note)
        mode_match <- mode_match | matches
      }
      df <- df[mode_match, , drop = FALSE]
    }
    
    # Context
    if(!is.null(input$context_filter) && length(input$context_filter) > 0) {
      for(context_name in input$context_filter) {
        context <- CONTEXT_FILTERS[[context_name]]
        if("exclude_activities" %in% names(context)) for(ex in context$exclude_activities) if(ex %in% names(ACTIVITY_MODES)) {
          terms <- ACTIVITY_MODES[[ex]]
          exm <- mapply(function(title, tags, note) matches_activity_mode(title, tags, note, terms), df$title, df$tags, df$note)
          df <- df[!exm, , drop = FALSE]
        }
        if("exclude_venues" %in% names(context)) for(ex in context$exclude_venues) if(ex %in% names(ACTIVITY_CATEGORIES)) {
          terms <- ACTIVITY_CATEGORIES[[ex]]
          exm <- sapply(df$tags, function(tags) matches_activity(tags, terms))
          df <- df[!exm, , drop = FALSE]
        }
        if("max_distance" %in% names(context)) df <- df[!is.na(df$distance_mi) & df$distance_mi <= context$max_distance, , drop = FALSE]
      }
    }
    
    # Transport distance cap
    if(!is.null(input$selected_transport) && nzchar(input$selected_transport)) {
      # input text equals the button label; find matching key
      key <- names(TRANSPORT_MODES)[match(input$selected_transport, names(TRANSPORT_MODES))]
      if (!is.na(key)) {
        max_dist <- TRANSPORT_MODES[[key]]
        if (max_dist < 999) df <- df[!is.na(df$distance_mi) & df$distance_mi <= max_dist, , drop = FALSE]
      }
    }
    
    # Keyword search
    if(!is.null(input$keyword_filter) && input$keyword_filter != "") {
      keywords <- stringr::str_split(input$keyword_filter, ",")[[1]] |> stringr::str_trim()
      for(kw in keywords[keywords != ""]) {
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
  
  # ---- Suggestions ----
  observeEvent(input$suggest_place, {
    candidates <- available_places()
    if(nrow(candidates) == 0) {
      showNotification("No unvisited places match your criteria!", type = "warning")
      values$suggested <- NULL
    } else {
      context_for_plan <- if(length(input$context_filter) > 0) input$context_filter[1] else NULL
      plans <- generate_day_plan(candidates, context_for_plan, input$selected_activities, input$selected_activity_modes, input$time_filter)
      if(length(plans) > 0 && !is.null(plans[[1]])) {
        plan <- plans[[1]]
        if (is.data.frame(plan$places) && nrow(plan$places) > 0) values$suggested <- plan$places[1, , drop = FALSE]
        else values$suggested <- candidates[sample(nrow(candidates), 1), , drop = FALSE]
        values$inspiration_text <- list(title = plan$title, description = plan$description, type = plan$type, estimated_time = plan$estimated_time)
      } else {
        values$suggested <- candidates[sample(nrow(candidates), 1), , drop = FALSE]
        values$inspiration_text <- NULL
      }
    }
  })
  
  observeEvent(input$random_inspiration, {
    all_available <- places[!(places$id %in% values$completed), , drop = FALSE]
    if(nrow(all_available) == 0) {
      showNotification("You've visited everywhere! Time for new places.", type = "warning")
      values$suggested <- NULL; return()
    }
    context_for_adventure <- if(length(input$context_filter) > 0) input$context_filter[1] else NULL
    adventures <- generate_surprise_adventure(all_available, input$time_filter, context_for_adventure)
    if(length(adventures) > 0 && !is.null(adventures[[1]])) {
      adventure <- adventures[[1]]
      if (is.data.frame(adventure$places) && nrow(adventure$places) > 0) values$suggested <- adventure$places[1, , drop = FALSE]
      else values$suggested <- all_available[sample(nrow(all_available), 1), , drop = FALSE]
      values$inspiration_text <- list(
        title = adventure$title, description = adventure$description, type = adventure$type,
        estimated_time = adventure$estimated_time, transit = adventure$transit, neighborhood = adventure$neighborhood
      )
    } else {
      values$suggested <- all_available[sample(nrow(all_available), 1), , drop = FALSE]
      values$inspiration_text <- list(title="🎲 Random Adventure", description="Go explore this place and see what happens!", type="random", estimated_time="1-3 hours")
    }
  })
  
  # ---- Visit toggles ----
  observeEvent(input$mark_visited, {
    if(is.null(values$suggested)) showNotification("Please suggest a place first!", type = "message")
    else { values$completed <- unique(c(values$completed, values$suggested$id)); save_completed(values$completed)
    showNotification(paste("✅", values$suggested$title, "marked as visited!"), type = "success") }
  })
  observeEvent(input$unmark, {
    if(is.null(values$suggested)) showNotification("Please suggest a place first!", type = "message")
    else { values$completed <- setdiff(values$completed, values$suggested$id); save_completed(values$completed)
    showNotification(paste("↩️", values$suggested$title, "unmarked!"), type = "success") }
  })
  observeEvent(input$reset_visited, { values$completed <- character(0); save_completed(values$completed); showNotification("♻️ All visited places reset!", type = "info") })
  
  # ---- Map clicks: Sextant toggles; Neighborhood toggles only after Sextant picked ----
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (is.null(click$id)) return(NULL)
    
    id <- click$id
    grp <- click$group %||% ""
    is_sextant <- (grp == "sextants") || startsWith(id, "sextant::")
    is_neigh   <- (grp == "neighborhoods") || startsWith(id, "neigh::")
    
    if (is_sextant) {
      sx_raw <- sub("^sextant::", "", id)
      sx <- sx_raw
      cur <- isolate(input$sextant_filter); if (is.null(cur)) cur <- character(0)
      cur <- if (sx %in% cur) setdiff(cur, sx) else unique(c(cur, sx))
      updateSelectizeInput(session, "sextant_filter", selected = cur, server = TRUE)
      showNotification(paste("🧭 Sextant:", if(length(cur)) paste(cur, collapse = ", ") else "Any"), type = "message")
    }
    
    if (is_neigh) {
      if (is.null(input$sextant_filter) || length(input$sextant_filter) == 0) return(NULL)  # ignore until a Sextant is chosen
      nb <- sub("^neigh::", "", id)
      cur_nb <- isolate(input$neighborhood_filter); if (is.null(cur_nb)) cur_nb <- character(0)
      cur_nb <- if (nb %in% cur_nb) setdiff(cur_nb, nb) else unique(c(cur_nb, nb))
      updateSelectizeInput(session, "neighborhood_filter", selected = cur_nb, server = TRUE)
      showNotification(paste("🏘️ Neighborhood:", if(length(cur_nb)) paste(cur_nb, collapse = ", ") else "Any"), type = "message")
    }
  })
  
  # ---- Base map ----
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = values$home_lng, lat = values$home_lat, zoom = 11)
  })
  
  # ---- Map layers ----
  observe({
    filtered <- filtered_places()
    visited <- places[places$id %in% values$completed, , drop = FALSE]
    suggested <- values$suggested
    
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    
    # Sextant polygons (always visible)
    if (!is.null(sextant_boundaries) && !is.null(SEXT_NAME_COL) && SEXT_NAME_COL %in% names(sextant_boundaries)) {
      sx_all <- sextant_boundaries
      sx_names_raw <- as.character(sx_all[[SEXT_NAME_COL]])
      sx_names <- vapply(sx_names_raw, canon_sextant, character(1))
      selected_sx <- isolate(input$sextant_filter); selected_sx <- if (is.null(selected_sx)) character(0) else selected_sx
      
      section_colors <- c("Southwest"="#ff6b6b","Northwest"="#4ecdc4","Southeast"="#45b7d1",
                          "Northeast"="#96ceb4","North"="#feca57","South"="#fd79a8","Other"="#a29bfe")
      
      for (i in seq_along(sx_names)) {
        sx <- sx_names[i]
        is_selected <- sx %in% selected_sx
        base_color <- section_colors[[sx]] %||% section_colors[["Other"]]
        proxy <- proxy %>% addPolygons(
          data = sx_all[i,],
          fillColor = base_color, fillOpacity = if(is_selected) 0.08 else 0.03,
          color = base_color, weight = if(is_selected) 2.5 else 1.5, opacity = if(is_selected) 0.9 else 0.7,
          group = "sextants", layerId = paste0("sextant::", sx),
          options = pathOptions(interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
          highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.25, bringToFront = FALSE),
          popup = paste0("<b>", sx, "</b><br>", if(is_selected) "Click to remove" else "Click to add")
        )
      }
    }
    
    # Neighborhood polygons (only AFTER a Sextant is selected; only those inside selection)
    if (!is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL) &&
        !is.null(input$sextant_filter) && length(input$sextant_filter) > 0) {
      
      nb <- neighborhood_boundaries
      nb_names <- as.character(nb[[NEI_NAME_COL]])
      selected_nb <- isolate(input$neighborhood_filter); selected_nb <- if (is.null(selected_nb)) character(0) else selected_nb
      
      rows_to_draw <- integer(0)
      if (!is.null(NB_TO_SEXTANT)) {
        rows_to_draw <- which(NB_TO_SEXTANT[nb_names] %in% intersect(SEXTANT_LEVELS, input$sextant_filter))
      } else if (!is.null(sextant_boundaries) && !is.null(SEXT_NAME_COL)) {
        secs_canon <- intersect(SEXTANT_LEVELS, input$sextant_filter)
        keep_rows <- which(vapply(sextant_boundaries[[SEXT_NAME_COL]], canon_sextant, character(1)) %in% secs_canon)
        sel_secs <- sextant_boundaries[keep_rows, , drop = FALSE]
        if (nrow(sel_secs)) {
          idx <- sf::st_intersects(nb, sel_secs, sparse = FALSE)
          rows_to_draw <- which(rowSums(idx) > 0)
        }
      }
      
      if (length(rows_to_draw)) {
        for (i in rows_to_draw) {
          nb_name <- nb_names[i]
          is_selected <- nb_name %in% selected_nb
          proxy <- proxy %>% addPolygons(
            data = nb[i,],
            fillColor = if(is_selected) "#10b981" else "#ccfbf1",
            fillOpacity = if(is_selected) 0.06 else 0.01,
            color = if(is_selected) "#10b981" else "#0ea5a8",
            weight = if(is_selected) 2 else 1.2, opacity = if(is_selected) 1 else 0.6,
            group = "neighborhoods", layerId = paste0("neigh::", nb_name),
            options = pathOptions(interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
            popup = paste0("<b>", nb_name, "</b><br>", if(is_selected) "Click to remove" else "Click to add"),
            highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.25, bringToFront = TRUE)
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
    if(nrow(filtered) > 0) {
      dist_txt <- ifelse(is.na(filtered$distance_mi), "—", round(filtered$distance_mi, 1))
      proxy <- proxy %>% addCircleMarkers(
        lng = filtered$lng, lat = filtered$lat, radius = 6, color = "#0ea5a8", fillColor = "#99f6e4",
        opacity = 0.9, fillOpacity = 0.6,
        popup = paste0("<b>", filtered$title, "</b><br>", filtered$tags, "<br>", dist_txt, " miles from home"),
        options = markerOptions(interactive = TRUE, zIndexOffset = 1000)
      )
    }
    
    # Visited places
    if(nrow(visited) > 0) proxy <- proxy %>% addCircleMarkers(
      lng = visited$lng, lat = visited$lat, radius = 5, color = "#9ca3af", fillColor = "#9ca3af",
      opacity = 0.9, fillOpacity = 0.7, options = markerOptions(interactive = TRUE, zIndexOffset = 1100)
    )
    
    # Suggested place
    if(!is.null(suggested)) proxy <- proxy %>% addCircleMarkers(
      lng = suggested$lng, lat = suggested$lat, radius = 12, color = "#dc2626", fillColor = "#dc2626",
      opacity = 1, fillOpacity = 0.85, options = markerOptions(interactive = TRUE, zIndexOffset = 1200)
    ) %>% setView(lng = suggested$lng, lat = suggested$lat, zoom = 15)
  })
  
  # ---- Weather ----
  current_weather <- reactive({
    weather <- get_weather_forecast()
    if(weather$success) return(weather)
    NULL
  })
  
  observe({
    weather <- current_weather()
    if(!is.null(weather) && weather$is_rainy && (is.null(input$context_filter) || length(input$context_filter) == 0)) {
      updateSelectizeInput(session, "context_filter", selected = "☔ Rainy Weather")
      showNotification("☔ Rainy weather detected - filtering to indoor activities", type = "message")
    }
  })
  
  output$weather_display <- renderUI({
    weather <- current_weather()
    if(!is.null(weather) && weather$success) {
      div(tags$strong(paste("Portland Weather:", weather$condition, "•", weather$temperature)), style = "color: #495057;")
    } else {
      div("Portland Weather", style = "color: #495057;")
    }
  })
  
  # ---- Suggestion panel ----
  output$suggestion_display <- renderUI({
    weather <- current_weather()
    if(is.null(values$suggested)) {
      weather_display <- if(!is.null(weather)) {
        div(style = "margin-bottom: 15px; padding: 10px; background: #f0f9ff; border-radius: 8px;",
            strong("🌤️ Portland Weather: "), weather$condition, " • ", weather$temperature,
            if(weather$is_rainy) span(style = "color: #0ea5a8; font-weight: 600;", " • Consider indoor activities!")
        )
      } else NULL
      div(class = "suggestion-box", weather_display,
          h4("🎯 Ready to explore?"),
          p("Use 'Get Suggestions' for intelligent picks, or 'Random Suggestion' for a surprise!")
      )
    } else {
      place <- values$suggested
      clean_tags_display <- clean_tags(place$tags)
      suggestion_header <- if(!is.null(values$inspiration_text) && is.list(values$inspiration_text)) {
        div(
          h4(values$inspiration_text$title, style = "color: var(--accent); margin-bottom: 8px;"),
          p(style = "color: #666; font-style: italic; margin-bottom: 12px;", values$inspiration_text$description),
          if(!is.null(values$inspiration_text$estimated_time))
            p(style = "color: #666; font-size: 12px;", "⏱️ Estimated time: ", values$inspiration_text$estimated_time),
          h3("✨ ", place$title)
        )
      } else {
        h3("🌟 ", place$title)
      }
      div(class = "suggestion-box",
          suggestion_header,
          if(clean_tags_display != "") p(strong("Features: "), clean_tags_display),
          p(strong("Distance: "), ifelse(is.na(place$distance_mi), "—", place$distance_mi), " miles from home"),
          if(!is.na(place$neighborhood)) p(strong("Neighborhood: "), place$neighborhood),
          if(place$note != "") p(strong("Note: "), place$note),
          if(place$url != "") tags$a("📍 View on Google Maps", href = place$url, target = "_blank")
      )
    }
  })
  
  # ---- Table ----
  output$places_table <- DT::renderDataTable({
    df <- filtered_places(); if(nrow(df) == 0) return(NULL)
    df$clean_tags <- sapply(df$tags, clean_tags)
    cols <- c("title","clean_tags","feature","neighborhood","distance_mi")
    keep <- intersect(cols, names(df))
    display_df <- df[, keep, drop = FALSE] %>% dplyr::arrange(distance_mi)
    DT::datatable(
      display_df,
      options = list(pageLength = 8, scrollX = TRUE, dom = 'tip'),
      colnames = c("title"="Place","clean_tags"="Tags","feature"="Feature",
                   "neighborhood"="Neighborhood","distance_mi"="Distance (mi)")
    )
  })
  
  output$visited_count <- renderText({ paste("Visited:", length(values$completed), "places") })
  output$visited_preview <- renderUI({
    if(length(values$completed) == 0) div("None yet - start exploring!") else {
      visited_places <- places[places$id %in% values$completed, , drop = FALSE]; recent <- head(visited_places$title, 5)
      div(
        lapply(recent, function(name) tags$span(style = "background: #dcfce7; padding: 2px 6px; margin: 2px; border-radius: 6px; font-size: 11px; display: inline-block;", name)),
        if(length(values$completed) > 5) p(paste("...", length(values$completed) - 5, "more"))
      )
    }
  })
}

cat("✅ Launching Portland Day Planner...\n"); shinyApp(ui, server)
