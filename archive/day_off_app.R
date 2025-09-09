# day_off_app.R — Portland Day-Off Planner 🌟
# 
# PURPOSE: Interactive Shiny app for planning perfect days off in Portland
# FEATURES:
#   - Mood/activity-based filtering (☕️ coffee, 📚 books, 🍰 treats, etc.)
#   - Transportation mode distance filtering (🚶 walking, 🚲 biking, 🚗 driving)
#   - Fixed home address (3976 N Gantenbein Avenue) with distance calculations
#   - Interactive map with clickable neighborhoods
#   - Place tracking system (mark as visited)
#   - Smart suggestion engine
# 
# USAGE: Run with source("day_off_app.R") - requires master dataset from build_master_dataset.R
# 
# day_off_app.R — Portland Day-Off Planner (Enhanced)
# Features:
# - Mood/activity-based filtering (coffee, restaurants, bookstores, etc.)
# - Fixed home address (3976 N Gantenbein Avenue)
# - Clickable neighborhood selection
# - Transportation mode distance filtering
# - Enhanced day planning workflow

suppressPackageStartupMessages({
  library(shiny)
  library(sf)
  library(leaflet)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(DT)
  library(readr)
})

options(shiny.sanitize.errors = FALSE)
sf::sf_use_s2(TRUE)

# ---------------- CONFIGURATION ----------------
proj_root <- "/Users/sidneybush/Documents/GitHub/Portland_shiny_app"
geo_dir <- file.path(proj_root, "Saved")
out_dir <- file.path(proj_root, "out_lists_portland_tagged")
persist_dir <- file.path(proj_root, "data")
dir.create(persist_dir, recursive = TRUE, showWarnings = FALSE)
completed_rds <- file.path(persist_dir, "completed_ids.rds")

# Fixed home address
HOME_ADDRESS <- "3976 N Gantenbein Avenue, Portland, OR 97227"
HOME_LAT <- 45.5623  # Approximate coordinates for N Gantenbein Avenue
HOME_LNG <- -122.6754

# Neighborhood boundaries
nbhd_candidates <- c(
  file.path(proj_root, "data", "Neighborhood_Boundaries.geojson"),
  file.path(proj_root, "data", "portland_neighborhoods.geojson"),
  file.path(proj_root, "data", "portland_neighborhoods.gpkg")
)
nbhd_path <- {
  exists <- nbhd_candidates[file.exists(nbhd_candidates)]
  if (length(exists) > 0) exists[1] else NA_character_
}

# ---------------- ACTIVITY CATEGORIES ----------------
# Parse emoji tags from your saved lists
activity_categories <- list(
  "Coffee & Cafes" = c("☕️ Coffee", "coffee", "cafe"),
  "Food & Restaurants" = c("🍃 Vegan", "🥞 Breakfast", "🚚 Food carts", "restaurant", "food"),
  "Sweet Treats" = c("🍰 Sweet treats", "🥯 Bagels", "dessert", "bakery"),
  "Drinks & Nightlife" = c("🍸 Beer & Cocktails", "bar", "brewery", "cocktails"),
  "Shopping - Books" = c("📚 Bookstores", "bookstore", "books"),
  "Shopping - Records" = c("💽 Record stores", "music", "records"),
  "Shopping - Vintage" = c("🏷️ Thrift & vintage", "vintage", "thrift"),
  "Shopping - Stationery" = c("📔 Stationery stores", "stationery", "art supplies"),
  "Shopping - Fun" = c("🛴 Fun stores", "toys", "games"),
  "Outdoor & Sports" = c("🚲 Bike shops", "outdoor", "sports", "hikes"),
  "Markets" = c("🥕 Farmer's markets", "market", "farmers"),
  "Creative & Body" = c("🖋️ Tattoo shops", "tattoo", "salon"),
  "Snacks & Quick Bites" = c("😋 Snack stores", "snacks", "quick")
)

# Transportation modes with distance limits
transport_modes <- list(
  "Walking" = list(label = "🚶 Walking", max_miles = 2),
  "Biking" = list(label = "🚲 Biking", max_miles = 10),
  "Driving" = list(label = "🚗 Driving", max_miles = 30),
  "Any" = list(label = "🌍 Any distance", max_miles = 999)
)

# ---------------- HELPER FUNCTIONS ----------------
read_csv_data <- function() {
  csv_files <- list.files(geo_dir, pattern = "\\.csv$", full.names = TRUE)
  
  all_data <- map_dfr(csv_files, function(file) {
    tryCatch({
      df <- read_csv(file, show_col_types = FALSE)
      df$source_file <- basename(tools::file_path_sans_ext(file))
      df
    }, error = function(e) {
      warning(paste("Could not read", file, ":", e$message))
      NULL
    })
  })
  
  # Clean and standardize
  all_data$title <- ifelse(is.na(all_data$Title) | all_data$Title == "", 
                          paste("Place from", all_data$source_file), 
                          all_data$Title)
  all_data$tags <- ifelse(is.na(all_data$Tags), "", all_data$Tags)
  all_data$note <- ifelse(is.na(all_data$Note), "", all_data$Note)
  all_data$url <- ifelse(is.na(all_data$URL), "", all_data$URL)
  
  # Extract coordinates from URLs (simplified)
  coords <- str_extract_all(all_data$url, "@(-?\\d+\\.\\d+),(-?\\d+\\.\\d+)")
  all_data$lat <- NA_real_
  all_data$lng <- NA_real_
  
  for(i in seq_len(nrow(all_data))) {
    if(length(coords[[i]]) > 0) {
      coord_match <- str_match(coords[[i]][1], "@(-?\\d+\\.\\d+),(-?\\d+\\.\\d+)")
      if(!is.na(coord_match[1])) {
        all_data$lat[i] <- as.numeric(coord_match[2])
        all_data$lng[i] <- as.numeric(coord_match[3])
      }
    }
  }
  
  # Filter valid coordinates
  all_data <- all_data[!is.na(all_data$lat) & !is.na(all_data$lng), ]
  all_data$id <- paste0(all_data$source_file, "_", seq_len(nrow(all_data)))
  
  all_data
}

# Load tagged geographic data if available, otherwise process CSV
load_places_data <- function() {
  tagged_files <- list.files(out_dir, pattern = "\\.geojson$", full.names = TRUE)
  
  if(length(tagged_files) > 0) {
    # Use existing tagged data
    places <- tagged_files |>
      map(~tryCatch(st_read(.x, quiet = TRUE), error = function(e) NULL)) |>
      compact() |>
      bind_rows() |>
      st_transform(4326)
    
    # Add missing fields
    if(!"tags" %in% names(places)) places$tags <- ""
    if(!"title" %in% names(places)) places$title <- places$name %||% "Unknown"
    
  } else {
    # Process CSV data
    csv_data <- read_csv_data()
    if(nrow(csv_data) == 0) stop("No data found in CSV files")
    
    # Convert to sf
    places <- st_as_sf(csv_data, coords = c("lng", "lat"), crs = 4326)
  }
  
  # Ensure required columns
  required_cols <- c("id", "title", "tags", "neighborhood", "section")
  for(col in required_cols) {
    if(!col %in% names(places)) places[[col]] <- NA_character_
  }
  
  places
}

to_miles <- function(meters) as.numeric(meters) / 1609.344

compute_distance_mi <- function(home_lat, home_lng, pts) {
  if (is.na(home_lat) || is.na(home_lng) || nrow(pts) == 0) return(rep(NA_real_, nrow(pts)))
  home_pt <- st_sfc(st_point(c(home_lng, home_lat)), crs = 4326)
  as.numeric(st_distance(pts, home_pt)) |> to_miles()
}

match_activity_category <- function(tags_text, category_terms) {
  if(is.na(tags_text) || tags_text == "") return(FALSE)
  tags_lower <- tolower(tags_text)
  any(sapply(category_terms, function(term) {
    # Remove emoji and check
    clean_term <- gsub("[^A-Za-z ]", "", term) |> str_trim() |> tolower()
    if(clean_term != "") {
      str_detect(tags_lower, fixed(clean_term))
    } else {
      str_detect(tags_text, fixed(term))  # Keep emoji for exact match
    }
  }))
}

load_completed <- function() {
  if (file.exists(completed_rds)) {
    out <- tryCatch(readRDS(completed_rds), error = function(e) character(0))
    as.character(out)
  } else character(0)
}

save_completed <- function(ids) saveRDS(as.character(ids), completed_rds)

# ---------------- LOAD DATA ----------------
places <- load_places_data()
if(nrow(places) == 0) stop("No places data loaded")

# Add distance from home
places$distance_mi <- compute_distance_mi(HOME_LAT, HOME_LNG, places)

# Load neighborhood boundaries
nbhd_polys <- NULL
if (!is.na(nbhd_path)) {
  nbhd_polys <- tryCatch(st_read(nbhd_path, quiet = TRUE), error = function(e) NULL)
  if (!is.null(nbhd_polys)) nbhd_polys <- st_transform(nbhd_polys, 4326)
}

# Get unique values for filters
section_vals <- sort(unique(na.omit(as.character(places$section))))
neighborhood_vals <- sort(unique(na.omit(as.character(places$neighborhood))))
activity_names <- names(activity_categories)

# ---------------- UI ----------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .content-header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                       color: white; padding: 20px; margin: -15px -15px 20px -15px; }
      .filter-panel { background: #f8f9fa; border-radius: 15px; padding: 20px; }
      .activity-chips { margin: 10px 0; }
      .activity-chip { display: inline-block; margin: 3px; padding: 5px 10px; 
                      background: #e3f2fd; border-radius: 15px; cursor: pointer; 
                      font-size: 12px; border: 1px solid #bbdefb; }
      .activity-chip.selected { background: #2196f3; color: white; }
      .transport-mode { margin: 5px; padding: 8px; border-radius: 8px; 
                       background: #fff; border: 2px solid #ddd; cursor: pointer; }
      .transport-mode.selected { border-color: #2196f3; background: #e3f2fd; }
      .suggestion-card { background: white; border-radius: 10px; padding: 15px; 
                        border-left: 4px solid #2196f3; margin: 10px 0; }
    "))
  ),
  
  div(class = "content-header",
    h1("🌟 Portland Day-Off Planner"),
    p("Find the perfect spots for your mood and transportation style")
  ),
  
  fluidRow(
    column(4,
      div(class = "filter-panel",
        h4("🎯 What's your vibe today?"),
        
        div(class = "activity-chips",
          lapply(names(activity_categories), function(cat) {
            div(class = "activity-chip", 
                id = paste0("activity_", gsub("[^A-Za-z0-9]", "_", cat)),
                onclick = paste0("toggleActivity('", cat, "')"),
                cat)
          })
        ),
        
        br(),
        h5("🚶 How are you getting around?"),
        div(id = "transport_modes",
          lapply(names(transport_modes), function(mode) {
            div(class = "transport-mode",
                id = paste0("transport_", gsub("[^A-Za-z0-9]", "_", mode)),
                onclick = paste0("selectTransport('", mode, "')"),
                transport_modes[[mode]]$label,
                br(),
                small(paste("<=", transport_modes[[mode]]$max_miles, "miles"))
            )
          })
        ),
        
        br(),
        h5("📍 Filter by Area"),
        selectizeInput("sections", "Sections", 
                      choices = c("All" = "", section_vals), 
                      multiple = TRUE),
        selectizeInput("neighborhoods", "Neighborhoods", 
                      choices = c("All" = "", neighborhood_vals), 
                      multiple = TRUE),
        
        textInput("keyword_text", "Additional Keywords", ""),
        
        hr(),
        div(style = "text-align: center;",
          actionButton("suggest", "🎲 Suggest a Spot!", 
                      class = "btn-primary btn-lg", 
                      style = "width: 100%; margin: 10px 0;"),
          br(),
          actionButton("mark_done", "✅ Been There", class = "btn-success"),
          actionButton("unmark_done", "↩️ Undo", class = "btn-warning"),
          br(), br(),
          actionButton("reset_done", "♻️ Reset All", class = "btn-outline-secondary")
        ),
        
        hr(),
        h5("🏠 Home Base"),
        p(HOME_ADDRESS, style = "font-size: 12px; color: #666;"),
        
        br(),
        h5("✅ Visited Places"),
        verbatimTextOutput("completed_count"),
        uiOutput("completed_preview")
      )
    ),
    
    column(8,
      leafletOutput("map", height = 500),
      br(),
      div(id = "suggestion_area",
        uiOutput("suggestion_card"),
        br(),
        DT::dataTableOutput("places_table")
      )
    )
  ),
  
  # JavaScript for interactivity
  tags$script(HTML("
    var selectedActivities = [];
    var selectedTransport = 'Any';
    
    function toggleActivity(category) {
      const chip = document.getElementById('activity_' + category.replace(/[^A-Za-z0-9]/g, '_'));
      const index = selectedActivities.indexOf(category);
      
      if (index > -1) {
        selectedActivities.splice(index, 1);
        chip.classList.remove('selected');
      } else {
        selectedActivities.push(category);
        chip.classList.add('selected');
      }
      
      Shiny.setInputValue('selected_activities', selectedActivities);
    }
    
    function selectTransport(mode) {
      document.querySelectorAll('.transport-mode').forEach(el => el.classList.remove('selected'));
      document.getElementById('transport_' + mode.replace(/[^A-Za-z0-9]/g, '_')).classList.add('selected');
      selectedTransport = mode;
      Shiny.setInputValue('selected_transport', mode);
    }
    
    // Initialize with 'Any' transport selected
    document.addEventListener('DOMContentLoaded', function() {
      selectTransport('Any');
    });
  "))
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  completed_ids <- reactiveVal(load_completed())
  chosen_place <- reactiveVal(NULL)
  
  # Reactive filters
  filtered_places <- reactive({
    df <- places
    
    # Activity category filtering
    if (!is.null(input$selected_activities) && length(input$selected_activities) > 0) {
      activity_matches <- rep(FALSE, nrow(df))
      
      for (activity in input$selected_activities) {
        if (activity %in% names(activity_categories)) {
          matches <- sapply(df$tags, function(tags) {
            match_activity_category(tags, activity_categories[[activity]])
          })
          activity_matches <- activity_matches | matches
        }
      }
      
      df <- df[activity_matches, , drop = FALSE]
    }
    
    # Geographic filtering
    if (!is.null(input$sections) && length(input$sections) > 0) {
      df <- df[df$section %in% input$sections, , drop = FALSE]
    }
    if (!is.null(input$neighborhoods) && length(input$neighborhoods) > 0) {
      df <- df[df$neighborhood %in% input$neighborhoods, , drop = FALSE]
    }
    
    # Transport mode distance filtering
    if (!is.null(input$selected_transport) && input$selected_transport != "Any") {
      max_dist <- transport_modes[[input$selected_transport]]$max_miles
      df <- df[!is.na(df$distance_mi) & df$distance_mi <= max_dist, , drop = FALSE]
    }
    
    # Keyword filtering
    if (!is.null(input$keyword_text) && input$keyword_text != "") {
      keywords <- str_split(input$keyword_text, ",")[[1]] |> str_trim()
      for (kw in keywords) {
        if (kw != "") {
          kw_match <- str_detect(tolower(paste(df$title, df$tags, df$note)), 
                                fixed(tolower(kw)))
          df <- df[kw_match, , drop = FALSE]
        }
      }
    }
    
    df
  })
  
  available_places <- reactive({
    df <- filtered_places()
    completed <- as.character(completed_ids())
    df[!(df$id %in% completed), , drop = FALSE]
  })
  
  # Suggestion logic
  observeEvent(input$suggest, {
    candidates <- available_places()
    if (nrow(candidates) == 0) {
      showNotification("No places match your criteria. Try adjusting filters!", 
                      type = "warning")
      chosen_place(NULL)
    } else {
      suggestion <- candidates[sample(nrow(candidates), 1), ]
      chosen_place(suggestion)
    }
  })
  
  # Mark as completed
  observeEvent(input$mark_done, {
    place <- chosen_place()
    if (is.null(place)) {
      showNotification("Pick a place first!", type = "message")
    } else {
      new_completed <- unique(c(as.character(completed_ids()), place$id))
      completed_ids(new_completed)
      save_completed(new_completed)
      showNotification(paste("Added", place$title, "to your visited list!"), type = "success")
    }
  })
  
  # Unmark
  observeEvent(input$unmark_done, {
    place <- chosen_place()
    if (is.null(place)) {
      showNotification("Pick a place first!", type = "message")
    } else {
      new_completed <- setdiff(as.character(completed_ids()), place$id)
      completed_ids(new_completed)
      save_completed(new_completed)
      showNotification(paste("Removed", place$title, "from visited list!"), type = "success")
    }
  })
  
  # Reset completed
  observeEvent(input$reset_done, {
    completed_ids(character(0))
    save_completed(character(0))
    showNotification("Reset all completed places!", type = "info")
  })
  
  # Map rendering
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = HOME_LNG, lat = HOME_LAT, zoom = 12) %>%
      addMarkers(lng = HOME_LNG, lat = HOME_LAT, 
                label = "🏠 Home", 
                popup = HOME_ADDRESS)
  })
  
  # Update map with places
  observe({
    filtered <- filtered_places()
    completed <- places[places$id %in% as.character(completed_ids()), , drop = FALSE]
    chosen <- chosen_place()
    
    proxy <- leafletProxy("map")
    proxy %>% clearMarkers() %>% clearShapes()
    
    # Add home marker
    proxy %>% addMarkers(lng = HOME_LNG, lat = HOME_LAT, 
                        label = "🏠 Home", popup = HOME_ADDRESS)
    
    # Add neighborhood boundaries if available
    if (!is.null(nbhd_polys)) {
      proxy %>% addPolygons(data = nbhd_polys, 
                           weight = 1, color = "#666666", fill = FALSE,
                           opacity = 0.5, group = "Neighborhoods")
    }
    
    # Add filtered places
    if (nrow(filtered) > 0) {
      coords <- st_coordinates(filtered)
      proxy %>% addCircleMarkers(
        lng = coords[, 1], lat = coords[, 2],
        radius = 6, color = "#2196f3", fillColor = "#bbdefb",
        opacity = 0.8, fillOpacity = 0.6,
        popup = paste0("<strong>", filtered$title, "</strong><br>",
                      "Tags: ", filtered$tags, "<br>",
                      "Distance: ", round(filtered$distance_mi, 1), " miles")
      )
    }
    
    # Add completed places
    if (nrow(completed) > 0) {
      coords_completed <- st_coordinates(completed)
      proxy %>% addCircleMarkers(
        lng = coords_completed[, 1], lat = coords_completed[, 2],
        radius = 5, color = "#4caf50", fillColor = "#4caf50",
        opacity = 0.9, fillOpacity = 0.7
      )
    }
    
    # Highlight chosen place
    if (!is.null(chosen)) {
      coords_chosen <- st_coordinates(chosen)
      proxy %>% addCircleMarkers(
        lng = coords_chosen[1], lat = coords_chosen[2],
        radius = 12, color = "#ff5722", fillColor = "#ff5722",
        opacity = 1, fillOpacity = 0.8
      )
      
      # Center on chosen place
      proxy %>% setView(lng = coords_chosen[1], lat = coords_chosen[2], zoom = 15)
    }
  })
  
  # Suggestion card
  output$suggestion_card <- renderUI({
    place <- chosen_place()
    if (is.null(place)) {
      return(div(class = "suggestion-card",
                h4("🎯 Ready for an adventure?"),
                p("Select your mood and transportation, then hit 'Suggest a Spot!' to discover your next destination.")))
    }
    
    div(class = "suggestion-card",
      h3("🌟 ", place$title),
      if (!is.na(place$tags) && place$tags != "") 
        p(strong("Vibe:"), place$tags),
      if (!is.na(place$neighborhood)) 
        p(strong("Area:"), place$neighborhood, 
          if (!is.na(place$section)) paste("-", place$section)),
      p(strong("Distance:"), round(place$distance_mi, 1), "miles from home"),
      if (!is.na(place$note) && place$note != "") 
        p(strong("Note:"), place$note)
    )
  })
  
  # Data table
  output$places_table <- DT::renderDataTable({
    df <- filtered_places() %>% st_drop_geometry()
    if (nrow(df) == 0) return(NULL)
    
    display_df <- df %>%
      select(title, tags, neighborhood, section, distance_mi) %>%
      mutate(distance_mi = round(distance_mi, 1)) %>%
      arrange(distance_mi)
    
    DT::datatable(display_df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  colnames = c("Place", "Tags", "Neighborhood", "Section", "Distance (mi)"))
  })
  
  # Completed count
  output$completed_count <- renderText({
    n <- length(completed_ids())
    paste("Places visited:", n)
  })
  
  # Completed preview
  output$completed_preview <- renderUI({
    ids <- completed_ids()
    if (length(ids) == 0) return(div("None yet - start exploring!"))
    
    completed_places <- places[places$id %in% ids, ] %>% st_drop_geometry()
    recent <- head(completed_places$title, 5)
    
    div(
      lapply(recent, function(name) {
        tags$span(class = "badge badge-success", style = "margin: 2px;", name)
      }),
      if (length(ids) > 5) p(paste("...", length(ids) - 5, "more"))
    )
  })
}

shinyApp(ui, server)