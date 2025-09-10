# portland_day_planner.R — Simple Day-Off Planner with Working Geocoding 🌟
# 
# SIMPLE WORKFLOW:
# 1. Export your Google Maps Saved Lists as CSV files
# 2. Place CSV files in Saved/ folder  
# 3. Run this script: source("portland_day_planner.R")
# 4. That's it! The app handles everything automatically.

suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(readr)
  library(stringr)
  library(DT)
  library(stringi)      # For text normalization
  library(tidygeocoder) # For geocoding (proven working version)
  library(tibble)
})

# ---------------- CONFIGURATION ----------------
HOME_ADDRESS <- "3976 N Gantenbein Avenue, Portland, OR 97227"
HOME_LAT <- 45.5623
HOME_LNG <- -122.6754

# Portland center for validation
PDX_CENTER <- c(lat = 45.523, lon = -122.676)

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
TRANSPORT_MODES <- list(
  "🚶 Walking" = 2,      # miles
  "🚲 Biking" = 10,      # miles  
  "🚗 Driving" = 30,     # miles
  "🌍 Any Distance" = 999 # no limit
)

# Simple neighborhood mapping for Portland sections
PORTLAND_SECTIONS <- list(
  # Downtown core
  "downtown" = "Southwest",
  "pearl" = "Northwest", 
  "old town" = "Northwest",
  "nob hill" = "Northwest",
  "northwest" = "Northwest",
  
  # East side
  "hawthorne" = "Southeast",
  "division" = "Southeast", 
  "richmond" = "Southeast",
  "sunnyside" = "Southeast",
  "sellwood" = "Southeast",
  "woodstock" = "Southeast",
  "brooklyn" = "Southeast",
  "hosford" = "Southeast",
  "buckman" = "Southeast",
  "powell" = "Southeast",
  "jade" = "Southeast",
  
  # Northeast  
  "alberta" = "Northeast",
  "fremont" = "Northeast",
  "irvington" = "Northeast", 
  "kerns" = "Northeast",
  "lloyd" = "Northeast",
  "hollywood" = "Northeast",
  "sullivan" = "Northeast",
  "beaumont" = "Northeast",
  "concordia" = "Northeast",
  "vernon" = "Northeast",
  "king" = "Northeast",
  "alameda" = "Northeast",
  "sabin" = "Northeast",
  
  # North
  "mississippi" = "North",
  "boise" = "North", 
  "humboldt" = "North",
  "piedmont" = "North",
  "kenton" = "North",
  "st johns" = "North",
  "cathedral park" = "North",
  
  # Southwest
  "south portland" = "Southwest",
  "burlingame" = "Southwest",
  "hillsdale" = "Southwest"
)

# ---------------- HELPER FUNCTIONS ----------------

# Normalize text - remove accents, umlauts, and convert to simple ASCII
normalize_text <- function(text) {
  if(is.na(text) || text == "") return(text)
  
  # Convert to ASCII, removing accents, umlauts, etc.
  normalized <- stringi::stri_trans_general(text, "Latin-ASCII")
  
  # Clean up any remaining odd characters
  normalized <- gsub("[^A-Za-z0-9 .,!?()'-]", "", normalized)
  
  # Clean up extra spaces
  normalized <- str_squish(normalized)
  
  return(normalized)
}

# Check if coordinates are near Portland
near_portland <- function(lat, lon, max_km = 120) {
  if(is.na(lat) || is.na(lon)) return(FALSE)
  # Rough distance calculation
  d <- sqrt((lat - PDX_CENTER["lat"])^2 + (lon - PDX_CENTER["lon"])^2) * 111
  d <= max_km
}

# Working geocoder from original system - simplified
geocode_place <- function(place_name, city = "Portland, OR") {
  if(is.na(place_name) || place_name == "") {
    return(list(lat = NA, lng = NA))
  }
  
  query <- paste(place_name, city, sep = ", ")
  
  # Try ArcGIS first (free and reliable)
  tryCatch({
    result <- tidygeocoder::geocode(tibble(query = query), 
                                   address = "query",
                                   method = "arcgis", 
                                   limit = 1, 
                                   quiet = TRUE)
    
    if(nrow(result) > 0 && !is.na(result$lat[1]) && !is.na(result$long[1])) {
      lat <- as.numeric(result$lat[1])
      lng <- as.numeric(result$long[1])
      
      if(near_portland(lat, lng)) {
        return(list(lat = lat, lng = lng))
      }
    }
    
    return(list(lat = NA, lng = NA))
  }, error = function(e) {
    return(list(lat = NA, lng = NA))
  })
}

# Extract coordinates from Google Maps URLs (multiple formats)
extract_coords_from_url <- function(url) {
  if(is.na(url) || url == "") return(list(lat = NA, lng = NA))
  
  # Pattern 1: !3d!4d format (most common)
  matches <- str_match(url, "!3d(-?\\d+\\.?\\d*)!4d(-?\\d+\\.?\\d*)")
  if(!is.na(matches[1])) {
    lat <- as.numeric(matches[2])
    lng <- as.numeric(matches[3])
    if(!is.na(lat) && !is.na(lng)) return(list(lat = lat, lng = lng))
  }
  
  # Pattern 2: @lat,lng format
  matches <- str_match(url, "@(-?\\d+\\.?\\d*),(-?\\d+\\.?\\d*)")
  if(!is.na(matches[1])) {
    lat <- as.numeric(matches[2])
    lng <- as.numeric(matches[3])
    if(!is.na(lat) && !is.na(lng)) return(list(lat = lat, lng = lng))
  }
  
  return(list(lat = NA, lng = NA))
}

# Calculate distance from home in miles
calc_distance_miles <- function(lat, lng) {
  if(is.na(lat) || is.na(lng)) return(NA)
  
  # Haversine formula
  R <- 3959  # Earth radius in miles
  lat1 <- HOME_LAT * pi/180
  lat2 <- lat * pi/180
  dlat <- (lat - HOME_LAT) * pi/180
  dlng <- (lng - HOME_LNG) * pi/180
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlng/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  round(R * c, 1)
}

# Guess Portland section from title/address text
guess_portland_section <- function(text) {
  if(is.na(text) || text == "") return(NA)
  text_lower <- tolower(text)
  
  for(area in names(PORTLAND_SECTIONS)) {
    if(str_detect(text_lower, fixed(area))) {
      return(PORTLAND_SECTIONS[[area]])
    }
  }
  
  return(NA)
}

# Check if place matches activity category
matches_activity <- function(tags_text, category_terms) {
  if(is.na(tags_text) || tags_text == "") return(FALSE)
  tags_lower <- tolower(tags_text)
  
  any(sapply(category_terms, function(term) {
    clean_term <- gsub("[^A-Za-z ]", "", term) |> str_trim() |> tolower()
    if(clean_term != "") {
      str_detect(tags_lower, fixed(clean_term))
    } else {
      str_detect(tags_text, fixed(term))  # Keep emoji
    }
  }))
}

# Load and process CSV data with geocoding
load_csv_data <- function() {
  csv_files <- list.files("Saved", pattern = "\\.csv$", full.names = TRUE)
  
  if(length(csv_files) == 0) {
    stop("No CSV files found in Saved/ folder. Please export your Google Maps lists as CSV files.")
  }
  
  cat("📂 Found", length(csv_files), "CSV files\n")
  
  all_data <- data.frame()
  
  for(file in csv_files) {
    tryCatch({
      df <- read_csv(file, show_col_types = FALSE)
      df$source_list <- basename(tools::file_path_sans_ext(file))
      all_data <- bind_rows(all_data, df)
      cat("✓ Loaded:", basename(file), "\n")
    }, error = function(e) {
      cat("⚠️  Could not read", basename(file), ":", e$message, "\n")
    })
  }
  
  if(nrow(all_data) == 0) {
    stop("No data loaded from CSV files")
  }
  
  # Clean and standardize columns
  all_data$title <- ifelse(is.na(all_data$Title) | all_data$Title == "", 
                          paste("Place from", all_data$source_list), 
                          all_data$Title)
  all_data$tags <- ifelse(is.na(all_data$Tags), "", as.character(all_data$Tags))
  all_data$note <- ifelse(is.na(all_data$Note), "", as.character(all_data$Note))
  all_data$url <- ifelse(is.na(all_data$URL), "", as.character(all_data$URL))
  
  # Normalize text to remove accents, umlauts, etc.
  cat("🔤 Normalizing text (removing accents, umlauts, etc.)...\n")
  all_data$title <- sapply(all_data$title, normalize_text)
  all_data$note <- sapply(all_data$note, normalize_text)
  
  # Try to extract coordinates from URLs first
  cat("🗺️  Extracting coordinates from URLs...\n")
  coords <- lapply(all_data$url, extract_coords_from_url)
  all_data$lat <- sapply(coords, function(x) x$lat)
  all_data$lng <- sapply(coords, function(x) x$lng)
  
  url_coords <- !is.na(all_data$lat) & !is.na(all_data$lng)
  cat("   Found coordinates in URLs for", sum(url_coords), "places\n")
  
  # For places without URL coordinates, geocode by name
  need_geocoding <- !url_coords & !is.na(all_data$title) & all_data$title != ""
  
  if(sum(need_geocoding) > 0) {
    cat("🌍 Geocoding", sum(need_geocoding), "places by name...\n")
    
    for(i in which(need_geocoding)) {
      if(i %% 5 == 1) cat("   Progress:", i, "of", sum(need_geocoding), "\n")
      
      geocoded <- geocode_place(all_data$title[i])
      if(!is.na(geocoded$lat)) {
        all_data$lat[i] <- geocoded$lat
        all_data$lng[i] <- geocoded$lng
      }
      
      # Be respectful to geocoding services
      if(i < max(which(need_geocoding))) Sys.sleep(0.5)
    }
  }
  
  # Final validation
  valid_coords <- !is.na(all_data$lat) & !is.na(all_data$lng)
  cat("✅ Total places with coordinates:", sum(valid_coords), "out of", nrow(all_data), "\n")
  
  if(sum(!valid_coords) > 0) {
    failed_places <- all_data$title[!valid_coords]
    cat("⚠️  Could not locate:", sum(!valid_coords), "places\n")
  }
  
  # Keep only valid places
  all_data <- all_data[valid_coords, ]
  
  if(nrow(all_data) == 0) {
    stop("❌ No places could be located! Check your CSV files and internet connection.")
  }
  
  # Add distance from home
  all_data$distance_mi <- mapply(calc_distance_miles, all_data$lat, all_data$lng)
  
  # Guess Portland sections
  all_data$section <- mapply(function(title, note) {
    section_from_title <- guess_portland_section(title)
    if(!is.na(section_from_title)) return(section_from_title)
    guess_portland_section(note)
  }, all_data$title, all_data$note)
  
  # Create unique IDs
  all_data$id <- paste0(all_data$source_list, "_", seq_len(nrow(all_data)))
  
  cat("🎉 Successfully loaded", nrow(all_data), "places!\n")
  
  return(all_data)
}

# Load completed places
load_completed <- function() {
  completed_file <- "data/completed_places.rds"
  if(file.exists(completed_file)) {
    readRDS(completed_file)
  } else {
    character(0)
  }
}

# Save completed places
save_completed <- function(completed_ids) {
  dir.create("data", showWarnings = FALSE)
  saveRDS(completed_ids, "data/completed_places.rds")
}

# ---------------- LOAD DATA ----------------
cat("🚀 Loading Portland Day Planner...\n")
places <- load_csv_data()

# ---------------- SHINY APP ----------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                color: white; padding: 20px; margin: -15px -15px 20px -15px; }
      .control-panel { background: #f8f9fa; border-radius: 15px; padding: 20px; }
      .activity-btn { margin: 3px; padding: 6px 12px; border-radius: 15px; 
                     border: 1px solid #ddd; background: white; cursor: pointer; font-size: 11px; }
      .activity-btn.active { background: #007bff; color: white; }
      .transport-btn { margin: 3px; padding: 8px 12px; border-radius: 8px; 
                      border: 2px solid #ddd; background: white; cursor: pointer; }
      .transport-btn.active { background: #e3f2fd; border-color: #007bff; }
      .suggestion-box { background: white; border-radius: 10px; padding: 15px; 
                       border-left: 4px solid #007bff; margin: 10px 0; }
    "))
  ),
  
  div(class = "header",
    h1("🌟 Portland Day-Off Planner"),
    p(paste("Home base:", HOME_ADDRESS, "• Loaded", nrow(places), "places"))
  ),
  
  fluidRow(
    column(4,
      div(class = "control-panel",
        h4("🎯 What's your vibe?"),
        
        # Activity filter buttons  
        div(id = "activity_buttons",
          lapply(names(ACTIVITY_CATEGORIES), function(cat) {
            actionButton(paste0("act_", gsub("[^A-Za-z0-9]", "", cat)), 
                        cat, class = "activity-btn")
          })
        ),
        
        br(), br(),
        h5("🚶 How are you getting there?"),
        div(id = "transport_buttons",
          lapply(names(TRANSPORT_MODES), function(mode) {
            actionButton(paste0("trans_", gsub("[^A-Za-z0-9]", "", mode)), 
                        paste(mode, "≤", TRANSPORT_MODES[[mode]], "mi"), 
                        class = "transport-btn")
          })
        ),
        
        br(), br(),
        textInput("keyword_filter", "Additional keywords:", ""),
        
        hr(),
        div(style = "text-align: center;",
          actionButton("suggest_place", "🎲 Suggest a Place!", 
                      class = "btn-primary btn-lg", style = "width: 100%;"),
          br(), br(),
          actionButton("mark_visited", "✅ Mark Visited", class = "btn-success"),
          actionButton("unmark", "↩️ Undo", class = "btn-warning"),
          br(), br(),
          actionButton("reset_visited", "♻️ Reset All", class = "btn-outline-secondary")
        ),
        
        hr(),
        h5("✅ Places Visited"),
        verbatimTextOutput("visited_count"),
        uiOutput("visited_preview")
      )
    ),
    
    column(8,
      leafletOutput("map", height = 500),
      br(),
      uiOutput("suggestion_display"),
      br(),
      h5("📋 Filtered Places"),
      DT::dataTableOutput("places_table")
    )
  ),
  
  # JavaScript for button interactions
  tags$script(HTML("
    $(document).on('click', '.activity-btn', function() {
      $(this).toggleClass('active');
      var active_activities = $('.activity-btn.active').map(function() {
        return $(this).text();
      }).get();
      Shiny.setInputValue('selected_activities', active_activities);
    });
    
    $(document).on('click', '.transport-btn', function() {
      $('.transport-btn').removeClass('active');
      $(this).addClass('active');
      Shiny.setInputValue('selected_transport', $(this).text());
    });
  "))
)

server <- function(input, output, session) {
  values <- reactiveValues(
    completed = load_completed(),
    suggested = NULL
  )
  
  # Filter places based on selections
  filtered_places <- reactive({
    df <- places
    
    # Activity filtering
    if(!is.null(input$selected_activities) && length(input$selected_activities) > 0) {
      activity_match <- rep(FALSE, nrow(df))
      
      for(activity in input$selected_activities) {
        if(activity %in% names(ACTIVITY_CATEGORIES)) {
          matches <- sapply(df$tags, function(tags) {
            matches_activity(tags, ACTIVITY_CATEGORIES[[activity]])
          })
          activity_match <- activity_match | matches
        }
      }
      df <- df[activity_match, ]
    }
    
    # Transportation filtering
    if(!is.null(input$selected_transport)) {
      transport_text <- input$selected_transport
      for(mode in names(TRANSPORT_MODES)) {
        if(str_detect(transport_text, fixed(str_sub(mode, 1, 10)))) {
          max_dist <- TRANSPORT_MODES[[mode]]
          if(max_dist < 999) {  # Not "Any Distance"
            df <- df[!is.na(df$distance_mi) & df$distance_mi <= max_dist, ]
          }
          break
        }
      }
    }
    
    # Keyword filtering
    if(!is.null(input$keyword_filter) && input$keyword_filter != "") {
      keywords <- str_split(input$keyword_filter, ",")[[1]] |> str_trim()
      for(kw in keywords[keywords != ""]) {
        kw_match <- str_detect(tolower(paste(df$title, df$tags, df$note)), 
                              fixed(tolower(kw)))
        df <- df[kw_match, ]
      }
    }
    
    df
  })
  
  # Available (unvisited) places
  available_places <- reactive({
    df <- filtered_places()
    df[!(df$id %in% values$completed), ]
  })
  
  # Suggest a place
  observeEvent(input$suggest_place, {
    candidates <- available_places()
    if(nrow(candidates) == 0) {
      showNotification("No unvisited places match your criteria!", type = "warning")
      values$suggested <- NULL
    } else {
      values$suggested <- candidates[sample(nrow(candidates), 1), ]
    }
  })
  
  # Mark as visited
  observeEvent(input$mark_visited, {
    if(is.null(values$suggested)) {
      showNotification("Please suggest a place first!", type = "message")
    } else {
      values$completed <- unique(c(values$completed, values$suggested$id))
      save_completed(values$completed)
      showNotification(paste("✅", values$suggested$title, "marked as visited!"), type = "success")
    }
  })
  
  # Unmark
  observeEvent(input$unmark, {
    if(is.null(values$suggested)) {
      showNotification("Please suggest a place first!", type = "message")
    } else {
      values$completed <- setdiff(values$completed, values$suggested$id)
      save_completed(values$completed)
      showNotification(paste("↩️", values$suggested$title, "unmarked!"), type = "success")
    }
  })
  
  # Reset all
  observeEvent(input$reset_visited, {
    values$completed <- character(0)
    save_completed(values$completed)
    showNotification("♻️ All visited places reset!", type = "info")
  })
  
  # Render map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = HOME_LNG, lat = HOME_LAT, zoom = 12) %>%
      addMarkers(lng = HOME_LNG, lat = HOME_LAT, 
                label = "🏠 Home", popup = HOME_ADDRESS)
  })
  
  # Update map markers
  observe({
    filtered <- filtered_places()
    visited <- places[places$id %in% values$completed, ]
    suggested <- values$suggested
    
    proxy <- leafletProxy("map")
    proxy %>% clearMarkers() %>% clearShapes()
    
    # Home marker
    proxy %>% addMarkers(lng = HOME_LNG, lat = HOME_LAT, 
                        label = "🏠 Home", popup = HOME_ADDRESS)
    
    # Filtered places
    if(nrow(filtered) > 0) {
      proxy %>% addCircleMarkers(
        lng = filtered$lng, lat = filtered$lat,
        radius = 6, color = "#007bff", fillColor = "#cce7ff", 
        opacity = 0.8, fillOpacity = 0.6,
        popup = paste0("<b>", filtered$title, "</b><br>",
                      filtered$tags, "<br>",
                      round(filtered$distance_mi, 1), " miles from home")
      )
    }
    
    # Visited places
    if(nrow(visited) > 0) {
      proxy %>% addCircleMarkers(
        lng = visited$lng, lat = visited$lat,
        radius = 5, color = "#28a745", fillColor = "#28a745",
        opacity = 0.9, fillOpacity = 0.7
      )
    }
    
    # Suggested place
    if(!is.null(suggested)) {
      proxy %>% addCircleMarkers(
        lng = suggested$lng, lat = suggested$lat,
        radius = 12, color = "#dc3545", fillColor = "#dc3545",
        opacity = 1, fillOpacity = 0.8
      ) %>%
      setView(lng = suggested$lng, lat = suggested$lat, zoom = 15)
    }
  })
  
  # Suggestion display
  output$suggestion_display <- renderUI({
    if(is.null(values$suggested)) {
      div(class = "suggestion-box",
        h4("🎯 Ready to explore?"),
        p("Pick your vibe and transportation, then hit 'Suggest a Place!'"))
    } else {
      place <- values$suggested
      div(class = "suggestion-box",
        h3("🌟 ", place$title),
        if(place$tags != "") p(strong("Vibe: "), place$tags),
        p(strong("Distance: "), place$distance_mi, " miles from home"),
        if(!is.na(place$section)) p(strong("Area: "), place$section),
        if(place$note != "") p(strong("Note: "), place$note),
        if(place$url != "") tags$a("📍 View on Google Maps", href = place$url, target = "_blank")
      )
    }
  })
  
  # Places table
  output$places_table <- DT::renderDataTable({
    df <- filtered_places()
    if(nrow(df) == 0) return(NULL)
    
    display_df <- df %>%
      select(title, tags, section, distance_mi, source_list) %>%
      arrange(distance_mi)
    
    DT::datatable(display_df, options = list(pageLength = 8, scrollX = TRUE),
                  colnames = c("Place", "Tags", "Area", "Distance (mi)", "List"))
  })
  
  # Visited count
  output$visited_count <- renderText({
    paste("Visited:", length(values$completed), "places")
  })
  
  # Visited preview
  output$visited_preview <- renderUI({
    if(length(values$completed) == 0) {
      div("None yet - start exploring!")
    } else {
      visited_places <- places[places$id %in% values$completed, ]
      recent <- head(visited_places$title, 5)
      div(
        lapply(recent, function(name) {
          tags$span(style = "background: #d4edda; padding: 2px 6px; margin: 2px; border-radius: 3px; font-size: 11px; display: inline-block;", name)
        }),
        if(length(values$completed) > 5) p(paste("...", length(values$completed) - 5, "more"))
      )
    }
  })
}

# Launch the app
if(nrow(places) > 0) {
  cat("✅ Ready! Launching Portland Day Planner with", nrow(places), "places...\n")
  shinyApp(ui, server)
} else {
  cat("❌ Cannot launch app - no places loaded!\n")
}