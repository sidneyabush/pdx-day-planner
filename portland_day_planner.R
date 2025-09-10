# portland_day_planner.R - Fast-Loading Portland Day-Off Planner (Modern UI, no purple, + Sections w/ multi-select)
# 
# SIMPLE WORKFLOW:
# 1. First time: Run source("process_data.R") to process CSV files
# 2. Then: Run source("portland_day_planner.R") for instant app launch
# 3. When you add new places: Re-run step 1, then step 2

suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(readr)
  library(stringr)
  library(DT)
})

# ---------------- CONFIGURATION ----------------
HOME_ADDRESS <- "3976 N Gantenbein Avenue, Portland, OR 97227"
HOME_LAT <- 45.5623
HOME_LNG <- -122.6754

# Weather API function (using a free service)
get_weather_forecast <- function() {
  tryCatch({
    url <- paste0("http://wttr.in/Portland,OR?format=%C+%t+%h+%w+%S+%s")
    response <- readLines(url, warn = FALSE)
    parts <- strsplit(response[1], " ")[[1]]
    if (length(parts) >= 4) {
      condition <- parts[1]; temp <- parts[2]; humidity <- parts[3]
      wind <- paste(parts[4:length(parts)], collapse = " ")
      
      # Determine if it's rainy
      is_rainy <- grepl("rain|drizzle|shower|storm", tolower(condition)) ||
                 grepl("☔|🌧️|⛈️|🌦️", condition)
      
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
  }, error = function(e) list(success = FALSE, error = e$message))
}

# Activity categories that match emoji tags (FIXED: removed bike shops from biking)
ACTIVITY_CATEGORIES <- list(
  "☕️ Coffee & Cafes" = c("coffee", "cafe", "espresso", "latte"),
  "🍰 Sweet Treats" = c("sweet treats", "bagels", "dessert", "bakery", "sweet", "donut", "pastry"),
  "🍃 Food & Restaurants" = c("vegan", "breakfast", "food carts", "restaurant", "food", "lunch", "dinner", "brunch"),
  "🍸 Drinks & Bars" = c("beer", "cocktails", "bar", "brewery", "drinks", "wine", "spirits"),
  "📚 Bookstores" = c("bookstore", "books", "reading", "literature"),
  "💽 Music & Records" = c("record stores", "music", "records", "vinyl", "cd"),
  "🏷️ Thrift & Vintage" = c("thrift", "vintage", "antique", "secondhand", "consignment"),
  "📔 Stationery & Art" = c("stationery", "art supplies", "paper", "pens", "notebooks"),
  "🛴 Fun Shopping" = c("fun stores", "toys", "games", "gifts", "novelty"),
  "🌲 Parks & Nature" = c("park", "garden", "nature", "trail", "hike", "outdoor", "forest"),
  "🥕 Markets" = c("farmers markets", "market", "farmers", "produce", "fresh food", "grocery"),
  "🎭 Entertainment" = c("movies", "theater", "entertainment", "cinema", "show", "performance"),
  "🖋️ Creative" = c("tattoo", "art studio", "art")
)

# Transportation modes  
TRANSPORT_MODES <- list("🚶 Walking"=2, "🚲 Biking"=10, "🚌 Public Transit"=15, "🚗 Driving"=30, "🌍 Any Distance"=999)

# Activity modes - what you want to DO during your trip
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

# Context filters for weather/mood
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

# ---------------- HELPER FUNCTIONS ----------------

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

load_completed <- function() {
  f <- "data/completed_places.rds"; if(file.exists(f)) readRDS(f) else character(0)
}
save_completed <- function(ids) { dir.create("data", showWarnings = FALSE); saveRDS(ids, "data/completed_places.rds") }

# Clean tags by removing symbols and emoji, keeping only readable text
clean_tags <- function(tags_text) {
  if(is.na(tags_text) || tags_text == "") return("")
  
  # Remove emoji and symbols, keep only letters, numbers, spaces, and basic punctuation
  cleaned <- gsub("[^A-Za-z0-9 .,!?()-]", " ", tags_text)
  
  # Clean up multiple spaces and trim
  cleaned <- str_squish(cleaned)
  
  # Remove single letters and very short words that are likely artifacts
  words <- strsplit(cleaned, " ")[[1]]
  words <- words[nchar(words) > 2 | words %in% c("&", "or", "of")]
  
  return(paste(words, collapse = " "))
}

# Generate smart day plan suggestions following formula: neighborhood + transit + activity
generate_day_plan <- function(available_places, context = NULL, selected_activities = NULL, selected_modes = NULL, time_available = "quick") {
  if(nrow(available_places) == 0) return(list())
  
  # ALWAYS pick a specific neighborhood (never just a section)
  places_with_neighborhoods <- available_places[!is.na(available_places$neighborhood), ]
  if(nrow(places_with_neighborhoods) == 0) return(list())
  
  # Select a specific neighborhood
  neighborhood_counts <- table(places_with_neighborhoods$neighborhood)
  # Prefer neighborhoods with multiple places
  good_neighborhoods <- names(neighborhood_counts[neighborhood_counts >= 1])
  chosen_neighborhood <- sample(good_neighborhoods, 1)
  neighborhood_places <- places_with_neighborhoods[places_with_neighborhoods$neighborhood == chosen_neighborhood, ]
  
  # Choose transportation based on distance
  avg_distance <- mean(neighborhood_places$distance_mi, na.rm = TRUE)
  transit_mode <- if(avg_distance <= 2) {
    "🚶 Walk"
  } else if(avg_distance <= 8) {
    "🚲 Bike" 
  } else {
    "🚗 Drive"
  }
  
  # Choose activity based on available venues or selected modes
  available_venue_types <- unique(unlist(strsplit(tolower(paste(neighborhood_places$tags, collapse = " ")), " ")))
  
  activity_plan <- if(!is.null(selected_modes) && length(selected_modes) > 0) {
    # Use selected activity mode
    mode <- sample(selected_modes, 1)
    if(mode == "📖 Reading") {
      reading_spots <- neighborhood_places[grepl("coffee|cafe|bookstore|library|park", neighborhood_places$tags, ignore.case = TRUE), ]
      if(nrow(reading_spots) > 0) {
        chosen_spot <- reading_spots[sample(nrow(reading_spots), 1), ]
        list(
          activity = "reading",
          venue = chosen_spot$title,
          description = paste("go to", chosen_spot$title, "and read"),
          places = chosen_spot
        )
      } else {
        list(activity = "reading", venue = "a quiet spot", description = "find a quiet spot and read", places = neighborhood_places[1, ])
      }
    } else if(mode == "🎨 Drawing/Sketching") {
      scenic_spots <- neighborhood_places[grepl("park|coffee|cafe|garden|scenic|view", neighborhood_places$tags, ignore.case = TRUE), ]
      if(nrow(scenic_spots) > 0) {
        chosen_spot <- scenic_spots[sample(nrow(scenic_spots), 1), ]
        list(
          activity = "sketching",
          venue = chosen_spot$title,
          description = paste("go to", chosen_spot$title, "and sketch"),
          places = chosen_spot
        )
      } else {
        list(activity = "sketching", venue = "a scenic spot", description = "find a scenic spot and sketch", places = neighborhood_places[1, ])
      }
    } else if(mode == "🛍️ Shopping Spree") {
      shopping_spots <- neighborhood_places[grepl("thrift|vintage|bookstore|record|market|shop", neighborhood_places$tags, ignore.case = TRUE), ]
      if(nrow(shopping_spots) > 0) {
        list(
          activity = "shopping",
          venue = if(nrow(shopping_spots) > 1) "multiple shops" else shopping_spots$title[1],
          description = paste("browse", if(nrow(shopping_spots) > 1) "the shops" else shopping_spots$title[1]),
          places = shopping_spots
        )
      } else {
        list(activity = "exploring", venue = "the area", description = "wander and explore", places = neighborhood_places[1, ])
      }
    } else if(mode == "📸 Photography") {
      photo_spots <- neighborhood_places[grepl("park|bridge|view|historic|architecture|mural|art", neighborhood_places$tags, ignore.case = TRUE), ]
      if(nrow(photo_spots) > 0) {
        chosen_spot <- photo_spots[sample(nrow(photo_spots), 1), ]
        list(
          activity = "photography",
          venue = chosen_spot$title,
          description = paste("go to", chosen_spot$title, "and take photos"),
          places = chosen_spot
        )
      } else {
        list(activity = "photography", venue = "around the neighborhood", description = "walk around and take photos", places = neighborhood_places[1, ])
      }
    } else {
      # Default activity based on venue types
      list(activity = "exploring", venue = "the area", description = "explore what's available", places = neighborhood_places[1, ])
    }
  } else {
    # Auto-choose activity based on available venues
    if(any(grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE))) {
      cafe_spots <- neighborhood_places[grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE), ]
      chosen_cafe <- cafe_spots[sample(nrow(cafe_spots), 1), ]
      list(
        activity = "coffee and reading",
        venue = chosen_cafe$title,
        description = paste("get coffee at", chosen_cafe$title, "and read"),
        places = chosen_cafe
      )
    } else if(any(grepl("bookstore", neighborhood_places$tags, ignore.case = TRUE))) {
      bookstore <- neighborhood_places[grepl("bookstore", neighborhood_places$tags, ignore.case = TRUE), ][1, ]
      list(
        activity = "book browsing", 
        venue = bookstore$title,
        description = paste("browse books at", bookstore$title),
        places = bookstore
      )
    } else if(any(grepl("park|nature", neighborhood_places$tags, ignore.case = TRUE))) {
      park <- neighborhood_places[grepl("park|nature", neighborhood_places$tags, ignore.case = TRUE), ][1, ]
      list(
        activity = "nature walk",
        venue = park$title,
        description = paste("take a walk through", park$title),
        places = park
      )
    } else if(any(grepl("thrift|vintage", neighborhood_places$tags, ignore.case = TRUE))) {
      thrift <- neighborhood_places[grepl("thrift|vintage", neighborhood_places$tags, ignore.case = TRUE), ][1, ]
      list(
        activity = "thrifting",
        venue = thrift$title,
        description = paste("browse vintage finds at", thrift$title),
        places = thrift
      )
    } else {
      # Default: just visit a place in the neighborhood
      chosen_place <- neighborhood_places[sample(nrow(neighborhood_places), 1), ]
      list(
        activity = "exploring",
        venue = chosen_place$title,
        description = paste("visit", chosen_place$title),
        places = chosen_place
      )
    }
  }
  
  # Calculate estimated time based on activity and distance
  estimated_time <- if(activity_plan$activity %in% c("reading", "coffee and reading")) {
    "2-3 hours"
  } else if(activity_plan$activity %in% c("shopping", "thrifting")) {
    "1-3 hours"
  } else if(activity_plan$activity %in% c("photography", "nature walk")) {
    "1-2 hours"
  } else {
    "1-2 hours"
  }
  
  # Build multi-activity plan based on time available
  if(time_available == "half_day" || time_available == "full_day") {
    # Generate additional activities for longer plans
    additional_plans <- list()
    
    if(time_available == "half_day") {
      # 3-4 hours: add coffee/cafe activity
      coffee_spots <- neighborhood_places[grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE), ]
      if(nrow(coffee_spots) > 0) {
        chosen_coffee <- coffee_spots[sample(nrow(coffee_spots), 1), ]
        additional_plans <- append(additional_plans, list(list(
          type = "structured",
          title = paste("☕️", "Coffee break"),
          description = paste("grab coffee at", chosen_coffee$title),
          neighborhood = chosen_neighborhood,
          transit = "🚶 Walk",
          activity = "coffee",
          places = chosen_coffee,
          estimated_time = "30-45 minutes"
        )))
      }
    }
    
    if(time_available == "full_day") {
      # 5+ hours: add museum/hike and coffee
      museum_spots <- neighborhood_places[grepl("museum|gallery|art", neighborhood_places$tags, ignore.case = TRUE), ]
      hike_spots <- neighborhood_places[grepl("hike|trail|park", neighborhood_places$tags, ignore.case = TRUE), ]
      
      if(nrow(museum_spots) > 0) {
        chosen_museum <- museum_spots[sample(nrow(museum_spots), 1), ]
        additional_plans <- append(additional_plans, list(list(
          type = "structured", 
          title = paste("🏛️", "Museum visit"),
          description = paste("explore", chosen_museum$title),
          neighborhood = chosen_neighborhood,
          transit = transit_mode,
          activity = "museum",
          places = chosen_museum,
          estimated_time = "1-2 hours"
        )))
      } else if(nrow(hike_spots) > 0) {
        chosen_hike <- hike_spots[sample(nrow(hike_spots), 1), ]
        additional_plans <- append(additional_plans, list(list(
          type = "structured",
          title = paste("🥾", "Nature time"), 
          description = paste("explore", chosen_hike$title),
          neighborhood = chosen_neighborhood,
          transit = transit_mode,
          activity = "hiking",
          places = chosen_hike,
          estimated_time = "2-3 hours"
        )))
      }
      
      # Also add coffee for full day
      coffee_spots <- neighborhood_places[grepl("coffee|cafe", neighborhood_places$tags, ignore.case = TRUE), ]
      if(nrow(coffee_spots) > 0) {
        chosen_coffee <- coffee_spots[sample(nrow(coffee_spots), 1), ]
        additional_plans <- append(additional_plans, list(list(
          type = "structured",
          title = paste("☕️", "Coffee break"),
          description = paste("refuel at", chosen_coffee$title),
          neighborhood = chosen_neighborhood, 
          transit = "🚶 Walk",
          activity = "coffee",
          places = chosen_coffee,
          estimated_time = "30 minutes"
        )))
      }
    }
    
    # Combine main plan with additional activities
    plan_title <- paste("🏡", chosen_neighborhood, if(time_available == "half_day") "(Half Day)" else "(Full Day)")
    main_plan <- list(
      type = "structured",
      title = plan_title,
      description = paste(transit_mode, "to", chosen_neighborhood, "and", activity_plan$description),
      neighborhood = chosen_neighborhood,
      transit = transit_mode,
      activity = activity_plan$activity,
      places = activity_plan$places,
      estimated_time = estimated_time
    )
    
    plans <- c(list(main_plan), additional_plans)
  } else {
    # Quick plan (1-2 hours) - single activity
    plan_title <- paste("🏡", chosen_neighborhood)
    plan_description <- paste(transit_mode, "to", chosen_neighborhood, "and", activity_plan$description)
    
    plans <- list(list(
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
  
  return(plans)
}

# ---------------- LOAD PROCESSED DATA ----------------
processed_file <- "data/portland_places_processed.rds"
if(!file.exists(processed_file)) { cat("❌ No processed data. Run process_data.R first.
"); stop("Missing processed data file") }
cat("🚀 Loading Portland Day Planner...
"); places <- readRDS(processed_file)
if(!is.data.frame(places) || nrow(places) == 0) { cat("❌ Invalid processed data.
"); stop("Invalid processed data") }

# Remove "Unnamed place" entries
if("title" %in% names(places)) {
  original_count <- nrow(places)
  places <- places[!grepl("^Unnamed place", places$title, ignore.case = TRUE), ]
  removed_count <- original_count - nrow(places)
  if(removed_count > 0) cat("🗑️ Removed", removed_count, "unnamed places
")
}

cat("✅ Loaded", nrow(places), "places
"); if("processed_date" %in% names(places)) cat("📅 Data processed on:", as.character(places$processed_date[1]), "
")

# ---------------- MAP DATA: neighborhoods + sections ----------------
load_map_file <- function(paths) {
  for (p in paths) if (file.exists(p)) return(p)
  return(NULL)
}

safe_read_sf <- function(path) {
  if (is.null(path)) return(NULL)
  tryCatch({
    suppressPackageStartupMessages(library(sf))
    sf::st_read(path, quiet = TRUE)
  }, error = function(e) { cat("⚠️ Could not load", path, ":", e$message, "
"); NULL })
}

pick_name_col <- function(sfobj, candidates) {
  cands <- candidates[candidates %in% names(sfobj)]
  if (length(cands)) cands[[1]] else NULL
}

# Neighborhoods
neighborhood_path <- load_map_file(c(
  "archive/Neighborhood_Boundaries.geojson",
  "archive/neighborhoods.geojson",
  "data/Neighborhood_Boundaries.geojson"
))
neighborhood_boundaries <- safe_read_sf(neighborhood_path)
NEI_NAME_COL <- if (!is.null(neighborhood_boundaries)) pick_name_col(neighborhood_boundaries, c("MAPLABEL","NAME","Label","Neighborhood","NEIGHBORHD","neigh","label")) else NULL

# Sextants (Portland Administrative Sections)
sextants_path <- load_map_file(c(
  "archive/Portland_Administrative_Sextants.geojson",
  "data/Portland_Administrative_Sextants.geojson"
))
sections_boundaries <- safe_read_sf(sextants_path)
SEC_NAME_COL <- if (!is.null(sections_boundaries)) pick_name_col(sections_boundaries, c("Sextant","SEXTANT","PREFIX","NAME")) else NULL

# ---------------- SHINY APP ----------------
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
      .form-control,.form-select { border-radius:12px !important; border:1.5px solid var(--border) !important; background:#fff !important; color:var(--text) !important; padding:10px 14px !important; transition: box-shadow .18s ease, border-color .18s ease !important;} .form-control:focus,.form-select:focus { border-color:var(--accent) !important; box-shadow:0 0 0 3px rgba(14,165,168,.12) !important;}
      .dataTables_wrapper { background:#fff; border-radius:14px; padding:16px; box-shadow:0 8px 20px rgba(0,0,0,.06); border:1px solid var(--border);} .dataTables_wrapper .dataTables_filter input { background:#fff !important; border:1px solid var(--border) !important; color:var(--text) !important; border-radius:10px !important;}
      .leaflet-container { border-radius:14px; box-shadow:0 8px 20px rgba(0,0,0,.06);} .leaflet-control-layers { border-radius:10px; }
    "))
  ),
  
  div(class = "header",
      h1("Portland Day‑Off Planner"),
      p(paste("Home base:", HOME_ADDRESS, "• Loaded", nrow(places), "places"))
  ),
  
  fluidRow(
    column(4,
           div(class = "control-panel",
               div(style = "text-align: center; margin-bottom: 20px;",
                   actionButton("random_inspiration", "🎲 Surprise Me!", class = "btn-info", style = "width: 100%;")
               ),
               h5("🌦️ Context (optional)"),
               selectizeInput("context_filter", "Weather/Mood:", choices = names(CONTEXT_FILTERS), selected = NULL, multiple = TRUE, options = list(placeholder = 'Any context'), width = "100%"),
               h5("⏰ Time Available"),
               selectInput("time_filter", "How much time?", 
                          choices = list(
                            "Quick (1-2 hours)" = "quick",
                            "Half day (3-4 hours)" = "half_day", 
                            "Full day (5+ hours)" = "full_day"
                          ), 
                          selected = "quick", width = "100%"),
               h5("🌈 Neighborhoods"),
               uiOutput("section_selector"),
               h5("🏘️ Sub-Neighborhoods"),
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
                   actionButton("suggest_place", "🎯 Smart Day Plan", class = "btn-primary", style = "width: 100%;")
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
               div(style = "font-size: 11px; color: #666; text-align: center;","To add new places: Update CSV files, then run process_data.R")
           )
    ),
    column(8,
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

server <- function(input, output, session) {
  values <- reactiveValues(completed = load_completed(), suggested = NULL, inspiration_text = NULL)
  
  # --- Selectors ---
  output$section_selector <- renderUI({
    if(!is.null(sections_boundaries) && !is.null(SEC_NAME_COL) && SEC_NAME_COL %in% names(sections_boundaries)) {
      tryCatch({
        secs <- sections_boundaries[[SEC_NAME_COL]] |> as.character() |> unique() |> sort()
        selectizeInput("section_filter", "", choices = secs, selected = NULL, multiple = TRUE, options = list(placeholder = 'Any neighborhoods'))
      }, error = function(e) {
        p(paste("Error loading sextants:", e$message), style = "font-size: 12px; color: #red;")
      })
    } else {
      p("Sextants layer not found. Need Portland_Administrative_Sextants.geojson", style = "font-size: 12px; color: #666;")
    }
  })
  output$neighborhood_selector <- renderUI({
    if(!is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL) && NEI_NAME_COL %in% names(neighborhood_boundaries)) {
      tryCatch({
        neighborhoods <- neighborhood_boundaries[[NEI_NAME_COL]] |> as.character() |> unique() |> sort()
        selectizeInput("neighborhood_filter", "", choices = neighborhoods, selected = NULL, multiple = TRUE, options = list(placeholder = 'Any sub-neighborhoods'))
      }, error = function(e) {
        p(paste("Error loading neighborhoods:", e$message), style = "font-size: 12px; color: #red;")
      })
    } else {
      p("Neighborhoods will appear after processing", style = "font-size: 12px; color: #666;")
    }
  })
  
  
  available_places <- reactive({
    df <- filtered_places()
    df[!(df$id %in% values$completed), ]
  })
  
  # --- Filtering logic ---
  filtered_places <- reactive({
    df <- places
    
    # SECTION filter (NEW) — requires a 'section' column in processed data
    if(!is.null(input$section_filter) && length(input$section_filter) > 0 && ("section" %in% names(df))) {
      df <- df[!is.na(df$section) & df$section %in% input$section_filter, ]
    }
    
    # Venue type filtering
    if(!is.null(input$selected_activities) && length(input$selected_activities) > 0) {
      activity_match <- rep(FALSE, nrow(df))
      for(activity in input$selected_activities) if(activity %in% names(ACTIVITY_CATEGORIES)) {
        matches <- sapply(df$tags, function(tags) matches_activity(tags, ACTIVITY_CATEGORIES[[activity]]))
        activity_match <- activity_match | matches
      }
      df <- df[activity_match, ]
    }
    
    # Activity mode filtering
    if(!is.null(input$selected_activity_modes) && length(input$selected_activity_modes) > 0) {
      mode_match <- rep(FALSE, nrow(df))
      for(mode in input$selected_activity_modes) if(mode %in% names(ACTIVITY_MODES)) {
        matches <- mapply(function(title, tags, note) matches_activity_mode(title, tags, note, ACTIVITY_MODES[[mode]]), df$title, df$tags, df$note)
        mode_match <- mode_match | matches
      }
      df <- df[mode_match, ]
    }
    
    # Context filtering
    if(!is.null(input$context_filter) && length(input$context_filter) > 0) {
      for(context_name in input$context_filter) {
        context <- CONTEXT_FILTERS[[context_name]]
        if("exclude_activities" %in% names(context)) for(ex in context$exclude_activities) if(ex %in% names(ACTIVITY_MODES)) {
          terms <- ACTIVITY_MODES[[ex]]
          exm <- mapply(function(title, tags, note) matches_activity_mode(title, tags, note, terms), df$title, df$tags, df$note)
          df <- df[!exm, ]
        }
        if("exclude_venues" %in% names(context)) for(ex in context$exclude_venues) if(ex %in% names(ACTIVITY_CATEGORIES)) {
          terms <- ACTIVITY_CATEGORIES[[ex]]
          exm <- sapply(df$tags, function(tags) matches_activity(tags, terms))
          df <- df[!exm, ]
        }
        if("max_distance" %in% names(context)) df <- df[!is.na(df$distance_mi) & df$distance_mi <= context$max_distance, ]
      }
    }
    
    # Transportation
    if(!is.null(input$selected_transport)) {
      transport_text <- input$selected_transport
      for(mode in names(TRANSPORT_MODES)) if(stringr::str_detect(transport_text, fixed(stringr::str_sub(mode, 1, 10)))) {
        max_dist <- TRANSPORT_MODES[[mode]]
        if(max_dist < 999) df <- df[!is.na(df$distance_mi) & df$distance_mi <= max_dist, ]
        break
      }
    }
    
    # Neighborhood (multi-select)
    if(!is.null(input$neighborhood_filter) && length(input$neighborhood_filter) > 0) {
      df <- df[!is.na(df$neighborhood) & df$neighborhood %in% input$neighborhood_filter, ]
    }
    
    # Keyword
    if(!is.null(input$keyword_filter) && input$keyword_filter != "") {
      keywords <- stringr::str_split(input$keyword_filter, ",")[[1]] |> stringr::str_trim()
      for(kw in keywords[keywords != ""]) {
        kw_match <- stringr::str_detect(tolower(paste(df$title, df$tags, df$note)), fixed(tolower(kw)))
        df <- df[kw_match, ]
      }
    }
    df
  })
  
  # --- Suggestion actions ---
  observeEvent(input$suggest_place, {
    candidates <- available_places()
    if(nrow(candidates) == 0) { 
      showNotification("No unvisited places match your criteria!", type = "warning"); 
      values$suggested <- NULL 
    } else { 
      # Generate smart day plans
      # Pass first context filter for now (day plan function needs updating for multiple contexts)
      context_for_plan <- if(length(input$context_filter) > 0) input$context_filter[1] else NULL
      plans <- generate_day_plan(candidates, context_for_plan, input$selected_activities, input$selected_activity_modes)
      
      if(length(plans) > 0 && !is.null(plans[[1]])) {
        plan <- plans[[1]]
        # Pick the first place from the plan as the "suggested" place
        values$suggested <- plan$places[1, ]
        values$inspiration_text <- list(
          title = plan$title,
          description = plan$description,
          type = plan$type,
          estimated_time = plan$estimated_time
        )
      } else {
        # Fallback to simple random selection
        values$suggested <- candidates[sample(nrow(candidates), 1), ]
        values$inspiration_text <- NULL
      }
    }
  })
  
  observeEvent(input$random_inspiration, {
    all_available <- places[!(places$id %in% values$completed), ]
    if(nrow(all_available) == 0) { 
      showNotification("You've visited everywhere! Time for new places.", type = "warning"); 
      values$suggested <- NULL; 
      return() 
    }
    
    # Generate a random day plan
    plans <- generate_day_plan(all_available, NULL, NULL, NULL)
    
    if(length(plans) > 0 && !is.null(plans[[1]])) {
      plan <- plans[[1]]
      values$suggested <- plan$places[1, ]
      values$inspiration_text <- list(
        title = paste("🎲 Random Inspiration:", plan$title),
        description = plan$description,
        type = plan$type,
        estimated_time = plan$estimated_time
      )
    } else {
      # Fallback
      values$suggested <- all_available[sample(nrow(all_available), 1), ]
      values$inspiration_text <- list(
        title = "🎲 Random Adventure",
        description = "Go explore this place and see what happens!",
        type = "random",
        estimated_time = "1-3 hours"
      )
    }
  })
  
  # --- Visit toggles ---
  observeEvent(input$mark_visited, { if(is.null(values$suggested)) showNotification("Please suggest a place first!", type = "message") else { values$completed <- unique(c(values$completed, values$suggested$id)); save_completed(values$completed); showNotification(paste("✅", values$suggested$title, "marked as visited!"), type = "success") } })
  observeEvent(input$unmark, { if(is.null(values$suggested)) showNotification("Please suggest a place first!", type = "message") else { values$completed <- setdiff(values$completed, values$suggested$id); save_completed(values$completed); showNotification(paste("↩️", values$suggested$title, "unmarked!"), type = "success") } })
  observeEvent(input$reset_visited, { values$completed <- character(0); save_completed(values$completed); showNotification("♻️ All visited places reset!", type = "info") })
  
  # --- Map click routing: neighborhoods vs sections ---
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (is.null(click$id)) return(NULL)
    grp <- click$group %||% ""
    
    if (grp == "sections") {
      # toggle section selection
      sec_name <- sub("^section::", "", click$id)
      cur <- isolate(input$section_filter)
      if (is.null(cur)) cur <- character(0)
      if (sec_name %in% cur) cur <- setdiff(cur, sec_name) else cur <- c(cur, sec_name)
      updateSelectizeInput(session, "section_filter", selected = cur, server = TRUE)
      showNotification(paste("📍 Sections:", if(length(cur)) paste(cur, collapse = ", ") else "Any"), type = "message")
    } else if (grp == "neighborhoods") {
      nb <- sub("^neigh::", "", click$id)
      cur <- isolate(input$neighborhood_filter)
      if (is.null(cur)) cur <- character(0)
      if (nb %in% cur) cur <- setdiff(cur, nb) else cur <- c(cur, nb)
      updateSelectizeInput(session, "neighborhood_filter", selected = cur, server = TRUE)
      showNotification(paste("🗺️ Neighborhoods:", if(length(cur)) paste(cur, collapse = ", ") else "Any"), type = "message")
    }
  })
  
  # --- Base map ---
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = HOME_LNG, lat = HOME_LAT, zoom = 11)
  })
  
  # --- Map layers (polygons + markers) ---
  observe({
    filtered <- filtered_places(); visited <- places[places$id %in% values$completed, ]; suggested <- values$suggested
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    
    # Sections layer — show with different translucent colors for each sextant
    if (!is.null(sections_boundaries) && !is.null(SEC_NAME_COL) && SEC_NAME_COL %in% names(sections_boundaries)) {
      secs_all <- sections_boundaries
      sec_names <- as.character(secs_all[[SEC_NAME_COL]])
      selected <- isolate(input$section_filter)
      selected <- if (is.null(selected)) character(0) else selected
      
      # Define colors for different sections
      section_colors <- c(
        "Southwest" = "#ff6b6b", "Northwest" = "#4ecdc4", "Southeast" = "#45b7d1", 
        "Northeast" = "#96ceb4", "North" = "#feca57", "South" = "#fd79a8", "Other" = "#a29bfe"
      )
      
      # Add each section with its own color
      for (i in seq_along(sec_names)) {
        sec_name <- sec_names[i]
        is_selected <- sec_name %in% selected
        
        # Choose color based on section name
        base_color <- section_colors[[sec_name]] %||% section_colors[["Other"]]
        
        proxy <- proxy %>% addPolygons(
          data = secs_all[i,], 
          fillColor = base_color, 
          fillOpacity = if(is_selected) 0.15 else 0.06,  # Reduced for better marker visibility
          color = base_color, 
          weight = if(is_selected) 2.5 else 1.5, 
          opacity = if(is_selected) 0.9 else 0.7, 
          group = "sections",
          layerId = paste0("section::", sec_name),
          options = pathOptions(interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
          highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.25, bringToFront = FALSE),
          popup = paste0("<b>", sec_name, "</b><br>", 
                        if(is_selected) "Click to remove from filter" else "Click to add to filter")
        )
      }
    }
    
    # Neighborhoods layer - show selected vs unselected
    if (!is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL) && NEI_NAME_COL %in% names(neighborhood_boundaries)) {
      nb <- neighborhood_boundaries
      nb_names <- as.character(nb[[NEI_NAME_COL]])
      selected_nb <- isolate(input$neighborhood_filter)
      selected_nb <- if (is.null(selected_nb)) character(0) else selected_nb
      
      # Add each neighborhood with appropriate styling
      for (i in seq_along(nb_names)) {
        nb_name <- nb_names[i]
        is_selected <- nb_name %in% selected_nb
        
        proxy <- proxy %>% addPolygons(
          data = nb[i,], 
          fillColor = if(is_selected) "#10b981" else "#ccfbf1", 
          fillOpacity = if(is_selected) 0.12 else 0.02,  # Further reduced to allow marker clicks
          color = if(is_selected) "#10b981" else "#0ea5a8", 
          weight = if(is_selected) 2 else 1.2,
          opacity = if(is_selected) 1 else 0.6, 
          group = "neighborhoods", 
          layerId = paste0("neigh::", nb_name),
          options = pathOptions(interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
          popup = paste0("<b>", nb_name, "</b><br>", 
                        if(is_selected) "Click to remove from filter" else "Click to add to filter"),
          highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.25, bringToFront = TRUE),
          options = pathOptions(interactive = TRUE, clickable = TRUE)  # Ensure proper click handling
        )
      }
    }
    
    # Home marker
    proxy <- proxy %>% addCircleMarkers(lng = HOME_LNG, lat = HOME_LAT, label = "🏠 Home", popup = HOME_ADDRESS, radius = 8, color = "#16a34a", fillColor = "#16a34a", opacity = 1, fillOpacity = 0.9)
    
    # Filtered places - ensure they appear above polygons with higher z-index
    if(nrow(filtered) > 0) proxy <- proxy %>% addCircleMarkers(
      lng = filtered$lng, lat = filtered$lat, radius = 6, color = "#0ea5a8", fillColor = "#99f6e4", opacity = 0.9, fillOpacity = 0.6,
      popup = paste0("<b>", filtered$title, "</b><br>", filtered$tags, "<br>", round(filtered$distance_mi, 1), " miles from home"),
      options = markerOptions(interactive = TRUE, zIndexOffset = 1000)  # Higher z-index to appear above polygons
    )
    
    # Visited places
    if(nrow(visited) > 0) proxy <- proxy %>% addCircleMarkers(
      lng = visited$lng, lat = visited$lat, radius = 5, color = "#9ca3af", fillColor = "#9ca3af", opacity = 0.9, fillOpacity = 0.7,
      options = markerOptions(interactive = TRUE, zIndexOffset = 1100)
    )
    
    # Suggested place
    if(!is.null(suggested)) proxy <- proxy %>% addCircleMarkers(
      lng = suggested$lng, lat = suggested$lat, radius = 12, color = "#dc2626", fillColor = "#dc2626", opacity = 1, fillOpacity = 0.85,
      options = markerOptions(interactive = TRUE, zIndexOffset = 1200)
    ) %>% setView(lng = suggested$lng, lat = suggested$lat, zoom = 15)
  })
  
  # Get current weather
  current_weather <- reactive({
    weather <- get_weather_forecast()
    if(weather$success) {
      return(weather)
    }
    return(NULL)
  })
  
  # Auto-set rainy context based on weather
  observe({
    weather <- current_weather()
    if(!is.null(weather) && weather$is_rainy && (is.null(input$context_filter) || length(input$context_filter) == 0)) {
      updateSelectizeInput(session, "context_filter", selected = "☔ Rainy Weather")
      showNotification("☔ Rainy weather detected - filtering to indoor activities", type = "message")
    }
  })

  # --- Suggestion display ---
  output$suggestion_display <- renderUI({
    weather <- current_weather()
    
    if(is.null(values$suggested)) {
      weather_display <- if(!is.null(weather)) {
        div(style = "margin-bottom: 15px; padding: 10px; background: #f0f9ff; border-radius: 8px;",
            strong("🌤️ Portland Weather: "), weather$condition, " • ", weather$temperature,
            if(weather$is_rainy) span(style = "color: #0ea5a8; font-weight: 600;", " • Consider indoor activities!")
        )
      } else NULL
      
      div(class = "suggestion-box", 
          weather_display,
          h4("🎯 Ready to explore?"), 
          p("Use 'Smart Day Plan' for intelligent suggestions, or 'Surprise Me!' for spontaneous adventure ideas!")
      )
    } else {
      place <- values$suggested
      
      # Clean the tags for display
      clean_tags_display <- clean_tags(place$tags)
      
      suggestion_header <- if(!is.null(values$inspiration_text) && is.list(values$inspiration_text)) {
        div(
          h4(values$inspiration_text$title, style = "color: var(--accent); margin-bottom: 8px;"),
          p(style = "color: #666; font-style: italic; margin-bottom: 12px;", values$inspiration_text$description),
          if(!is.null(values$inspiration_text$estimated_time)) 
            p(style = "color: #666; font-size: 12px;", "⏱️ Estimated time: ", values$inspiration_text$estimated_time),
          h3("✨ ", place$title)
        )
      } else if(!is.null(values$inspiration_text)) {
        div(h4(values$inspiration_text, style = "color: var(--accent); margin-bottom: 12px;"), h3("✨ ", place$title))
      } else {
        h3("🌟 ", place$title)
      }
      
      div(class = "suggestion-box",
          suggestion_header,
          if(clean_tags_display != "") p(strong("Features: "), clean_tags_display),
          p(strong("Distance: "), place$distance_mi, " miles from home"),
          if(!is.na(place$neighborhood)) p(strong("Area: "), place$neighborhood),
          if(place$note != "") p(strong("Note: "), place$note),
          if(place$url != "") tags$a("📍 View on Google Maps", href = place$url, target = "_blank")
      )
    }
  })
  
  # --- Table & visited ---
  output$places_table <- DT::renderDataTable({
    df <- filtered_places(); if(nrow(df) == 0) return(NULL)
    
    # Clean tags for table display
    df$clean_tags <- sapply(df$tags, clean_tags)
    
    display_df <- df %>% 
      select(title, clean_tags, neighborhood, distance_mi, source_list) %>% 
      arrange(distance_mi)
    
    DT::datatable(display_df, options = list(pageLength = 8, scrollX = TRUE, dom = 'tip'), 
                  colnames = c("Place","Features","Neighborhood","Distance (mi)","List"))
  })
  output$visited_count <- renderText({ paste("Visited:", length(values$completed), "places") })
  output$visited_preview <- renderUI({
    if(length(values$completed) == 0) div("None yet - start exploring!") else {
      visited_places <- places[places$id %in% values$completed, ]; recent <- head(visited_places$title, 5)
      div(lapply(recent, function(name) tags$span(style = "background: #dcfce7; padding: 2px 6px; margin: 2px; border-radius: 6px; font-size: 11px; display: inline-block;", name)), if(length(values$completed) > 5) p(paste("...", length(values$completed) - 5, "more")))
    }
  })
}

`%||%` <- function(a,b) if(!is.null(a)) a else b

cat("✅ Launching Portland Day Planner...
"); shinyApp(ui, server)
