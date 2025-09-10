# portland_day_planner.R - Fast-Loading Portland Day-Off Planner (Modern UI, no purple, + Sections w/ multi-select)
# 
# SIMPLE WORKFLOW:
# 1. First time: Run source("process_data.R") to process your CSV files
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
    url <- paste0("http://wttr.in/Portland,OR?format=%C+%t+%h+%w")
    response <- readLines(url, warn = FALSE)
    parts <- strsplit(response[1], " ")[[1]]
    if (length(parts) >= 4) {
      condition <- parts[1]; temp <- parts[2]; humidity <- parts[3]
      wind <- paste(parts[4:length(parts)], collapse = " ")
      return(list(condition=condition, temperature=temp, humidity=humidity, wind=wind, success=TRUE))
    }
    list(success = FALSE)
  }, error = function(e) list(success = FALSE, error = e$message))
}

# Activity categories that match your emoji tags
ACTIVITY_CATEGORIES <- list(
  "☕️ Coffee & Cafes" = c("☕️ Coffee", "coffee", "cafe"),
  "🍰 Sweet Treats" = c("🍰 Sweet treats", "🥯 Bagels", "dessert", "bakery", "sweet"),
  "🍃 Food & Restaurants" = c("🍃 Vegan", "🥞 Breakfast", "🚚 Food carts", "restaurant", "food", "lunch", "dinner"),
  "🍸 Drinks & Bars" = c("🍸 Beer & Cocktails", "bar", "brewery", "cocktails", "drinks"),
  "📚 Bookstores" = c("📚 Bookstores", "bookstore", "books"),
  "💽 Music & Records" = c("💽 Record stores", "music", "records", "vinyl"),
  "🏷️ Thrift & Vintage" = c("🏷️ Thrift & vintage", "vintage", "thrift", "antique"),
  "📔 Stationery & Art" = c("📔 Stationery stores", "stationery", "art supplies", "paper"),
  "🛴 Fun Shopping" = c("🛴 Fun stores", "toys", "games", "gifts"),
  "🚲 Outdoor & Sports" = c("🚲 Bike shops", "outdoor", "sports", "hikes", "nature"),
  "🥕 Markets" = c("🥕 Farmer's markets", "market", "farmers", "produce"),
  "🎭 Entertainment" = c("🎬 Movies", "🎭 Theater", "entertainment", "cinema", "theater"),
  "🖋️ Services & Creative" = c("🖋️ Tattoo shops", "tattoo", "salon", "spa")
)

# Transportation modes  
TRANSPORT_MODES <- list("🚶 Walking"=2, "🚲 Biking"=10, "🚗 Driving"=30, "🌍 Any Distance"=999)

# Activity modes - what you want to DO during your trip
ACTIVITY_MODES <- list(
  "📖 Reading" = c("coffee","cafe","bookstore","quiet","library","park"),
  "🎨 Drawing" = c("coffee","cafe","park","outdoor","scenic","garden","museum"),  
  "🚲 Biking" = c("bike","trail","park","outdoor","path","route"),
  "🥾 Hiking" = c("trail","hike","outdoor","nature","park","forest"),
  "📸 Taking Film Photos" = c("scenic","architecture","vintage","historic","art","bridge","view"),
  "🚶 Long Walk" = c("park","trail","bridge","scenic","outdoor","nature","neighborhood")
)

# Context filters for weather/mood
CONTEXT_FILTERS <- list(
  "☔ Rainy Weather" = list(
    exclude_activities = c("🚲 Biking","🥾 Hiking","🚶 Long Walk","📸 Taking Film Photos"),
    exclude_venues = c("🚲 Outdoor & Sports"),
    hide_activity_buttons = c("🚲 Biking","🥾 Hiking","🚶 Long Walk","📸 Taking Film Photos"),
    prefer_close = TRUE, max_distance = 3
  ),
  "😴 Low Energy" = list(
    exclude_activities = c("🚲 Biking","🥾 Hiking"),
    prefer_activities = c("📖 Reading","🎨 Drawing"),
    prefer_venues = c("☕️ Coffee & Cafes","📚 Bookstores"),
    prefer_close = TRUE, max_distance = 2
  ),
  "🏠 Stay Home Mode" = list(
    home_activities = c("📖 Reading","🎨 Drawing/Painting","🧩 Puzzling","🎬 Movies","🍳 Cooking","🧘 Meditation")
  ),
  "🌞 Perfect Weather" = list(
    prefer_activities = c("🚲 Biking","🥾 Hiking","📸 Taking Film Photos"),
    prefer_venues = c("🚲 Outdoor & Sports"),
    boost_outdoor = TRUE
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

# ---------------- LOAD PROCESSED DATA ----------------
processed_file <- "data/portland_places_processed.rds"
if(!file.exists(processed_file)) { cat("❌ No processed data. Run process_data.R first.
"); stop("Missing processed data file") }
cat("🚀 Loading Portland Day Planner...
"); places <- readRDS(processed_file)
if(!is.data.frame(places) || nrow(places) == 0) { cat("❌ Invalid processed data.
"); stop("Invalid processed data") }
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

# Sections (NEW)
sections_path <- load_map_file(c(
  "archive/Portland_Sections.geojson",
  "archive/Sections.geojson",
  "archive/Portland_Sections.shp",
  "data/Portland_Sections.geojson"
))
sections_boundaries <- safe_read_sf(sections_path)
SEC_NAME_COL <- if (!is.null(sections_boundaries)) pick_name_col(sections_boundaries, c("SECTION","Section","SEC_NAME","NAME","MAPLABEL","LABEL","section")) else NULL

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
               h4("🎯 What's your vibe?"),
               div(id = "activity_buttons",
                   lapply(names(ACTIVITY_CATEGORIES), function(cat) {
                     actionButton(paste0("act_", gsub("[^A-Za-z0-9]", "", cat)), cat, class = "activity-btn")
                   })
               ),
               br(),
               h5("🎯 What do you want to DO there?"),
               div(id = "activity_mode_buttons",
                   lapply(names(ACTIVITY_MODES), function(mode) {
                     actionButton(paste0("mode_", gsub("[^A-Za-z0-9]", "", mode)), mode, class = "activity-btn")
                   })
               ),
               br(), br(),
               h5("🚶 How are you getting there?"),
               div(id = "transport_buttons",
                   lapply(names(TRANSPORT_MODES), function(mode) {
                     actionButton(paste0("trans_", gsub("[^A-Za-z0-9]", "", mode)), paste(mode, "≤", TRANSPORT_MODES[[mode]], "mi"), class = "transport-btn")
                   })
               ),
               br(), br(),
               h5("🌦️ Context (optional)"),
               selectInput("context_filter", "Weather/Mood:", choices = c("Any context" = "", names(CONTEXT_FILTERS)), selected = "", width = "100%"),
               h5("🏙️ Section(s) (optional) — NEW"),
               uiOutput("section_selector"),
               h5("🏘️ Neighborhood (optional)"),
               uiOutput("neighborhood_selector"),
               br(),
               textInput("keyword_filter", "Additional keywords:", ""),
               hr(),
               div(style = "text-align: center;",
                   actionButton("suggest_place", "🎯 Find Specific Place", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                   actionButton("random_inspiration", "🎲 Random Inspiration", class = "btn-info", style = "width: 100%;"),
                   br(), br(),
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
      $('.transport-btn').removeClass('active'); $(this).addClass('active');
      Shiny.setInputValue('selected_transport', $(this).text());
    });
    $(document).on('change', '#context_filter', function() {
      var context = $(this).val(); $('.activity-btn').show();
      if (context === '☔ Rainy Weather') {
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
    if(!is.null(sections_boundaries) && !is.null(SEC_NAME_COL)) {
      secs <- sections_boundaries[[SEC_NAME_COL]] |> as.character() |> unique() |> sort()
      selectizeInput("section_filter", "Choose section(s):", choices = secs, selected = NULL, multiple = TRUE, options = list(placeholder = 'Any section'))
    } else {
      p("Sections layer not found. Add Portland_Sections.geojson to archive/", style = "font-size: 12px; color: #666;")
    }
  })
  output$neighborhood_selector <- renderUI({
    if(!is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL)) {
      neighborhoods <- neighborhood_boundaries[[NEI_NAME_COL]] |> as.character() |> unique() |> sort()
      selectInput("neighborhood_filter", "Choose specific neighborhood:", choices = c("Any neighborhood" = "", neighborhoods), selected = "", width = "100%")
    } else {
      p("Neighborhoods will appear after processing", style = "font-size: 12px; color: #666;")
    }
  })
  
  # --- Stay-home suggestion ---
  stay_home_suggestion <- reactive({
    if(!is.null(input$context_filter) && input$context_filter == "🏠 Stay Home Mode") {
      chosen_activity <- sample(CONTEXT_FILTERS[["🏠 Stay Home Mode"]]$home_activities, 1)
      return(list(activity = chosen_activity, is_home = TRUE))
    }
    NULL
  })
  
  available_places <- reactive({
    if(!is.null(input$context_filter) && input$context_filter == "🏠 Stay Home Mode") return(data.frame())
    df <- filtered_places()
    df[!(df$id %in% values$completed), ]
  })
  
  # --- Filtering logic ---
  filtered_places <- reactive({
    if(!is.null(stay_home_suggestion())) return(data.frame())
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
    if(!is.null(input$context_filter) && input$context_filter != "") {
      context <- CONTEXT_FILTERS[[input$context_filter]]
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
    
    # Transportation
    if(!is.null(input$selected_transport)) {
      transport_text <- input$selected_transport
      for(mode in names(TRANSPORT_MODES)) if(stringr::str_detect(transport_text, fixed(stringr::str_sub(mode, 1, 10)))) {
        max_dist <- TRANSPORT_MODES[[mode]]
        if(max_dist < 999) df <- df[!is.na(df$distance_mi) & df$distance_mi <= max_dist, ]
        break
      }
    }
    
    # Neighborhood
    if(!is.null(input$neighborhood_filter) && input$neighborhood_filter != "") df <- df[!is.na(df$neighborhood) & df$neighborhood == input$neighborhood_filter, ]
    
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
    if(nrow(candidates) == 0) { showNotification("No unvisited places match your criteria!", type = "warning"); values$suggested <- NULL }
    else { values$suggested <- candidates[sample(nrow(candidates), 1), ]; values$inspiration_text <- NULL }
  })
  
  observeEvent(input$random_inspiration, {
    all_available <- places[!(places$id %in% values$completed), ]
    if(nrow(all_available) == 0) { showNotification("You've visited everywhere! Time for new places.", type = "warning"); values$suggested <- NULL; return() }
    inspiration_type <- sample(c("neighborhood","activity_mode","venue_type","combo","full_combo"), 1, prob = c(0.25,0.25,0.25,0.15,0.10))
    neighborhoods <- if(!is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL)) unique(neighborhood_boundaries[[NEI_NAME_COL]]) else unique(all_available$neighborhood)
    activity_modes <- names(ACTIVITY_MODES); venue_types <- names(ACTIVITY_CATEGORIES)
    if(inspiration_type == "neighborhood" && length(neighborhoods) > 0) {
      chosen_hood <- sample(neighborhoods, 1)
      hood_places <- all_available[!is.na(all_available$neighborhood) & all_available$neighborhood == chosen_hood, ]
      if(nrow(hood_places) > 0) { values$suggested <- hood_places[sample(nrow(hood_places), 1), ]; values$inspiration_text <- paste("🏘️ Explore", chosen_hood, "today!") }
    } else if(inspiration_type == "activity_mode") {
      chosen_mode <- sample(activity_modes, 1)
      if(chosen_mode == "🚶 Long Walk") {
        walk_places <- all_available[all_available$distance_mi <= 5 & !is.na(all_available$distance_mi), ]
        if(nrow(walk_places) > 0) { values$suggested <- walk_places[sample(nrow(walk_places), 1), ]; values$inspiration_text <- "🚶 Go on a long walk (walk to this place and back home)!" }
      } else {
        mode_terms <- ACTIVITY_MODES[[chosen_mode]]
        mode_places <- all_available[mapply(function(title, tags, note) matches_activity_mode(title, tags, note, mode_terms), all_available$title, all_available$tags, all_available$note), ]
        if(nrow(mode_places) > 0) { values$suggested <- mode_places[sample(nrow(mode_places), 1), ]; values$inspiration_text <- paste("🎯", gsub("[🎨🚲🥾📖📸🚶]", "", chosen_mode), "somewhere today!") }
      }
    } else if(inspiration_type == "venue_type") {
      chosen_venue <- sample(venue_types, 1)
      venue_terms <- ACTIVITY_CATEGORIES[[chosen_venue]]
      venue_places <- all_available[sapply(all_available$tags, function(tags) matches_activity(tags, venue_terms)), ]
      if(nrow(venue_places) > 0) {
        values$suggested <- venue_places[sample(nrow(venue_places), 1), ]
        clean_venue <- gsub("[☕🍰📚💽🏷️📔🛴🚲🥕🎭🖋️🍸🍃🎬]", "", chosen_venue)
        values$inspiration_text <- if(stringr::str_detect(chosen_venue, "Movies")) "🍿 Go see a movie today!" else paste("☕ Find a", clean_venue, "today!")
      }
    } else if(inspiration_type == "combo" && length(neighborhoods) > 0) {
      chosen_hood <- sample(neighborhoods, 1); chosen_activity <- sample(c(activity_modes, venue_types), 1)
      hood_places <- all_available[!is.na(all_available$neighborhood) & all_available$neighborhood == chosen_hood, ]
      if(nrow(hood_places) > 0) { values$suggested <- hood_places[sample(nrow(hood_places), 1), ]; values$inspiration_text <- paste("🌟", gsub("[🎨🚲🥾📖📸☕🍰📚💽🏷️📔🛴🚲🥕🎭🖋️🍸🍃]", "", chosen_activity), "in", chosen_hood, "today!") }
    } else {
      values$suggested <- all_available[sample(nrow(all_available), 1), ]; values$inspiration_text <- "🎲 Random adventure awaits!"
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
      updateSelectInput(session, "neighborhood_filter", selected = nb)
      showNotification(paste("🗺️ Filtered to", nb, "neighborhood"), type = "message")
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
    
    # Sections layer — show unselected faint + selected emphasized
    if (!is.null(sections_boundaries) && !is.null(SEC_NAME_COL)) {
      secs_all <- sections_boundaries
      sec_names <- as.character(secs_all[[SEC_NAME_COL]])
      selected <- isolate(input$section_filter)
      selected <- if (is.null(selected)) character(0) else selected
      is_sel <- sec_names %in% selected
      # unselected
      if (any(!is_sel)) proxy %>% addPolygons(
        data = secs_all[!is_sel,], fillColor = "#e0f2fe", fillOpacity = 0.06,
        color = "#38bdf8", weight = 1.1, opacity = 0.8, group = "sections",
        layerId = paste0("section::", sec_names[!is_sel]),
        highlightOptions = highlightOptions(weight = 2, color = "#0ea5a8", fillOpacity = 0.15, bringToFront = TRUE),
        popup = ~paste0("<b>", secs_all[[SEC_NAME_COL]][!is_sel], "</b><br>Click to toggle section filter")
      )
      # selected
      if (any(is_sel)) proxy %>% addPolygons(
        data = secs_all[is_sel,], fillColor = "#99f6e4", fillOpacity = 0.22,
        color = "#0ea5a8", weight = 2, opacity = 1, group = "sections",
        layerId = paste0("section::", sec_names[is_sel]),
        popup = ~paste0("<b>", secs_all[[SEC_NAME_COL]][is_sel], "</b><br>Click to toggle section filter")
      )
    }
    
    # Neighborhoods layer
    if (!is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL)) {
      nb <- neighborhood_boundaries
      nb_names <- as.character(nb[[NEI_NAME_COL]])
      proxy %>% addPolygons(
        data = nb, fillColor = "#ccfbf1", fillOpacity = 0.08, color = "#0ea5a8", weight = 1.2,
        opacity = 0.8, group = "neighborhoods", layerId = paste0("neigh::", nb_names),
        popup = ~paste0("<b>", nb[[NEI_NAME_COL]], "</b><br>Click to filter to this neighborhood"),
        highlightOptions = highlightOptions(weight = 2.2, color = "#0ea5a8", fillOpacity = 0.18, bringToFront = TRUE)
      )
    }
    
    # Home marker
    proxy %>% addCircleMarkers(lng = HOME_LNG, lat = HOME_LAT, label = "🏠 Home", popup = HOME_ADDRESS, radius = 8, color = "#16a34a", fillColor = "#16a34a", opacity = 1, fillOpacity = 0.9)
    
    # Filtered places
    if(nrow(filtered) > 0) proxy %>% addCircleMarkers(
      lng = filtered$lng, lat = filtered$lat, radius = 6, color = "#0ea5a8", fillColor = "#99f6e4", opacity = 0.9, fillOpacity = 0.6,
      popup = paste0("<b>", filtered$title, "</b><br>", filtered$tags, "<br>", round(filtered$distance_mi, 1), " miles from home")
    )
    
    # Visited places
    if(nrow(visited) > 0) proxy %>% addCircleMarkers(lng = visited$lng, lat = visited$lat, radius = 5, color = "#9ca3af", fillColor = "#9ca3af", opacity = 0.9, fillOpacity = 0.7)
    
    # Suggested place
    if(!is.null(suggested)) proxy %>% addCircleMarkers(lng = suggested$lng, lat = suggested$lat, radius = 12, color = "#dc2626", fillColor = "#dc2626", opacity = 1, fillOpacity = 0.85) %>% setView(lng = suggested$lng, lat = suggested$lat, zoom = 15)
  })
  
  # --- Suggestion display ---
  output$suggestion_display <- renderUI({
    if(is.null(values$suggested)) {
      div(class = "suggestion-box", h4("🎯 Ready to explore?"), p("Use 'Find Specific Place' for precise filtering, or 'Random Inspiration' for vague adventure ideas!"))
    } else {
      place <- values$suggested
      div(class = "suggestion-box",
          if(!is.null(values$inspiration_text)) div(h4(values$inspiration_text, style = "color: var(--accent); margin-bottom: 12px;"), h3("✨ ", place$title)) else h3("🌟 ", place$title),
          if(place$tags != "") p(strong("Vibe: "), place$tags),
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
    display_df <- df %>% select(title, tags, neighborhood, distance_mi, source_list) %>% arrange(distance_mi)
    DT::datatable(display_df, options = list(pageLength = 8, scrollX = TRUE, dom = 'tip'), colnames = c("Place","Tags","Neighborhood","Distance (mi)","List"))
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
