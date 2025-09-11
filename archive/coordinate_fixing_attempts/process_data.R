# process_data.R - Data Processing for Portland Day Planner
# 
# PURPOSE: Process your Google Maps CSV exports once, creating a fast-loading dataset
# 
# WORKFLOW:
# 1. Export your Google Maps Saved Lists as CSV files  
# 2. Place CSV files in Saved/ folder
# 3. Run this script: source("process_data.R") 
# 4. Then launch the app: source("portland_day_planner.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringi)      # For text normalization
  library(tidygeocoder) # For geocoding
  library(tibble)
  library(sf)           # For spatial operations
})

# ---------------- CONFIGURATION ----------------
HOME_ADDRESS <- "3976 N Gantenbein Avenue, Portland, OR 97227"
HOME_LAT <- 45.5623
HOME_LNG <- -122.6754

# Portland center for validation
PDX_CENTER <- c(lat = 45.523, lon = -122.676)

# Load neighborhood boundary data for spatial intersection
load_neighborhood_boundaries <- function() {
  boundaries_file <- "archive/Neighborhood_Boundaries.geojson"
  if (!file.exists(boundaries_file)) {
    cat("Warning: Neighborhood boundaries file not found, using simplified sections\n")
    return(NULL)
  }
  
  tryCatch({
    st_read(boundaries_file, quiet = TRUE)
  }, error = function(e) {
    cat("Warning: Could not load neighborhood boundaries:", e$message, "\n")
    return(NULL)
  })
}

# Load sextant boundary data for spatial intersection
load_sextant_boundaries <- function() {
  sextants_file <- "archive/Portland_Administrative_Sextants.geojson"
  if (!file.exists(sextants_file)) {
    cat("Warning: Sextants boundaries file not found\n")
    return(NULL)
  }
  
  tryCatch({
    st_read(sextants_file, quiet = TRUE)
  }, error = function(e) {
    cat("Warning: Could not load sextant boundaries:", e$message, "\n")
    return(NULL)
  })
}

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

# Working geocoder
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

# Automatically lookup business address from the web
lookup_business_address <- function(business_name, city = "Portland, OR") {
  if(is.na(business_name) || business_name == "") {
    return(NA)
  }
  
  tryCatch({
    # Create search query for business address
    query <- paste(business_name, city, "address")
    
    # Try to get address using a simple web search approach
    # We'll use the tidygeocoder's internal address search capabilities
    # by trying different geocoding services that return address info
    
    # Method 1: Try OpenStreetMap Nominatim which often returns full addresses
    result <- tidygeocoder::geocode(tibble(query = paste(business_name, city, sep = ", ")), 
                                   address = "query",
                                   method = "osm", 
                                   limit = 1, 
                                   full_results = TRUE,
                                   quiet = TRUE)
    
    if(nrow(result) > 0 && !is.na(result$lat[1])) {
      # Try to extract address components if available
      if("display_name" %in% names(result) && !is.na(result$display_name[1])) {
        address_parts <- strsplit(result$display_name[1], ",")[[1]]
        if(length(address_parts) >= 2) {
          # Usually the first part is the street address
          potential_address <- trimws(address_parts[1])
          if(grepl("\\d+", potential_address)) {
            return(paste(potential_address, city, sep = ", "))
          }
        }
      }
    }
    
    # Method 2: Try Google geocoding with business name
    result2 <- tidygeocoder::geocode(tibble(query = paste(business_name, city, sep = ", ")), 
                                    address = "query",
                                    method = "arcgis", 
                                    limit = 1, 
                                    full_results = TRUE,
                                    quiet = TRUE)
    
    if(nrow(result2) > 0 && "address" %in% names(result2) && !is.na(result2$address[1])) {
      return(result2$address[1])
    }
    
    return(NA)
    
  }, error = function(e) {
    return(NA)
  })
}

# Extract coordinates from Google Maps URLs
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

# Assign neighborhoods using spatial intersection
assign_neighborhoods <- function(places_df, boundaries = NULL) {
  if (is.null(boundaries) || nrow(places_df) == 0) {
    return(rep(NA, nrow(places_df)))
  }
  
  tryCatch({
    # Convert places to spatial points
    valid_coords <- !is.na(places_df$lat) & !is.na(places_df$lng)
    neighborhoods <- rep(NA, nrow(places_df))
    
    if (sum(valid_coords) == 0) return(neighborhoods)
    
    places_sf <- st_as_sf(
      places_df[valid_coords, ], 
      coords = c("lng", "lat"), 
      crs = 4326
    )
    
    # Ensure both have same CRS
    boundaries <- st_transform(boundaries, 4326)
    
    # Find intersections
    intersections <- st_intersection(places_sf, boundaries)
    
    # Extract neighborhood names
    if (nrow(intersections) > 0 && "MAPLABEL" %in% names(intersections)) {
      valid_indices <- which(valid_coords)
      for (i in seq_len(nrow(intersections))) {
        original_idx <- valid_indices[as.numeric(row.names(intersections)[i])]
        neighborhoods[original_idx] <- intersections$MAPLABEL[i]
      }
    }
    
    return(neighborhoods)
  }, error = function(e) {
    cat("Warning: Spatial intersection failed:", e$message, "\n")
    return(rep(NA, nrow(places_df)))
  })
}

# Assign sextants using spatial intersection
assign_sextants <- function(places_df, sextant_boundaries = NULL) {
  if (is.null(sextant_boundaries) || nrow(places_df) == 0) {
    return(rep(NA, nrow(places_df)))
  }
  
  tryCatch({
    # Convert places to spatial points
    valid_coords <- !is.na(places_df$lat) & !is.na(places_df$lng)
    sextants <- rep(NA, nrow(places_df))
    
    if (sum(valid_coords) == 0) return(sextants)
    
    places_sf <- st_as_sf(
      places_df[valid_coords, ], 
      coords = c("lng", "lat"), 
      crs = 4326
    )
    
    # Ensure both have same CRS
    sextant_boundaries <- st_transform(sextant_boundaries, 4326)
    
    # Find intersections
    intersections <- st_intersection(places_sf, sextant_boundaries)
    
    # Extract sextant names
    if (nrow(intersections) > 0 && "Sextant" %in% names(intersections)) {
      valid_indices <- which(valid_coords)
      for (i in seq_len(nrow(intersections))) {
        original_idx <- valid_indices[as.numeric(row.names(intersections)[i])]
        sextants[original_idx] <- intersections$Sextant[i]
      }
    }
    
    return(sextants)
  }, error = function(e) {
    cat("Warning: Sextant spatial intersection failed:", e$message, "\n")
    return(rep(NA, nrow(places_df)))
  })
}

# ---------------- MAIN PROCESSING FUNCTION ----------------

process_csv_data <- function() {
  all_csv_files <- list.files("Saved", pattern = "\\.csv$", full.names = TRUE)
  
  # Filter to only Portland-relevant files (containing hiking trails, PDX, or Portland)
  relevant_patterns <- c("trail", "Trail", "PDX", "Portland", "portland")
  csv_files <- all_csv_files[sapply(all_csv_files, function(file) {
    any(sapply(relevant_patterns, function(pattern) {
      grepl(pattern, basename(file), fixed = TRUE)
    }))
  })]
  
  if(length(csv_files) == 0) {
    stop("No CSV files found in Saved/ folder. Please export your Google Maps lists as CSV files.")
  }
  
  cat("Processing Portland Places Data...\n")
  cat("Found", length(csv_files), "CSV files\n")
  
  all_data <- data.frame()
  
  for(file in csv_files) {
    tryCatch({
      df <- read_csv(file, show_col_types = FALSE)
      df$source_list <- basename(tools::file_path_sans_ext(file))
      all_data <- bind_rows(all_data, df)
      cat("Loaded:", basename(file), "\n")
    }, error = function(e) {
      cat("Warning: Could not read", basename(file), ":", e$message, "\n")
    })
  }
  
  if(nrow(all_data) == 0) {
    stop("No data loaded from CSV files")
  }
  
  # Clean and standardize columns - be more aggressive about getting titles
  all_data$title <- ifelse(is.na(all_data$Title) | all_data$Title == "", 
                          ifelse(!is.na(all_data$Note) & all_data$Note != "",
                                 substr(all_data$Note, 1, 50), # Use first 50 chars of note
                                 paste("Unnamed place from", all_data$source_list)), 
                          all_data$Title)
  all_data$tags <- ifelse(is.na(all_data$Tags), "", as.character(all_data$Tags))
  all_data$note <- ifelse(is.na(all_data$Note), "", as.character(all_data$Note))
  all_data$url <- ifelse(is.na(all_data$URL), "", as.character(all_data$URL))
  
  # Normalize text to remove accents, umlauts, etc.
  cat("Normalizing text (removing accents, umlauts, etc.)...\n")
  all_data$title <- sapply(all_data$title, normalize_text)
  all_data$note <- sapply(all_data$note, normalize_text)
  
  # Clean tags to make them more readable
  cat("Cleaning tags (removing excessive symbols)...\n")
  clean_tags_func <- function(tags_text) {
    if(is.na(tags_text) || tags_text == "") return("")
    # Remove excessive emoji and symbols, keep only readable text
    cleaned <- gsub("[^A-Za-z0-9 .,!?()&-]", " ", tags_text)
    cleaned <- str_squish(cleaned)
    # Remove single letters that are likely artifacts
    words <- strsplit(cleaned, " ")[[1]]
    words <- words[nchar(words) > 2 | words %in% c("&", "or", "of")]
    return(paste(words, collapse = " "))
  }
  all_data$tags <- sapply(all_data$tags, clean_tags_func)
  
  # Replace "Portland Parks" with just "Park"
  all_data$tags <- gsub("\\bPortland Parks\\b", "Park", all_data$tags, ignore.case = TRUE)
  
  # Add specific tags based on source list names
  cat("Adding list-specific tags...\n")
  
  # Add "Museums" tag for PDX museums list
  museum_mask <- grepl("PDX.*museums|museums.*PDX", all_data$source_list, ignore.case = TRUE)
  if (any(museum_mask)) {
    all_data$tags[museum_mask] <- ifelse(
      nzchar(all_data$tags[museum_mask]), 
      paste(all_data$tags[museum_mask], "Museums", sep = "; "),
      "Museums"
    )
    cat("Added 'Museums' tag to", sum(museum_mask), "places from PDX museums list\n")
  }
  
  # Add "Trail" tag for Hiking & Trail Running PDX list
  trail_mask <- grepl("Hiking.*Trail.*Running.*PDX|Trail.*Running.*PDX", all_data$source_list, ignore.case = TRUE)
  if (any(trail_mask)) {
    all_data$tags[trail_mask] <- ifelse(
      nzchar(all_data$tags[trail_mask]), 
      paste(all_data$tags[trail_mask], "Trail", sep = "; "),
      "Trail"
    )
    cat("Added 'Trail' tag to", sum(trail_mask), "places from Hiking & Trail Running PDX list\n")
  }
  
  # Add "Park" tag for Portland Parks list
  park_mask <- grepl("Portland.*Parks|Parks.*Portland", all_data$source_list, ignore.case = TRUE)
  if (any(park_mask)) {
    all_data$tags[park_mask] <- ifelse(
      nzchar(all_data$tags[park_mask]), 
      paste(all_data$tags[park_mask], "Park", sep = "; "),
      "Park"
    )
    cat("Added 'Park' tag to", sum(park_mask), "places from Portland Parks list\n")
  }
  
  # Try to extract coordinates from URLs first
  cat("Extracting coordinates from URLs...\n")
  coords <- lapply(all_data$url, extract_coords_from_url)
  all_data$lat <- sapply(coords, function(x) x$lat)
  all_data$lng <- sapply(coords, function(x) x$lng)
  
  url_coords <- !is.na(all_data$lat) & !is.na(all_data$lng)
  cat("   Found coordinates in URLs for", sum(url_coords), "places\n")
  
  # For places without URL coordinates, try geocoding by name, then by note if available
  need_geocoding <- !url_coords & !is.na(all_data$title) & all_data$title != ""
  
  if(sum(need_geocoding) > 0) {
    cat("Geocoding", sum(need_geocoding), "places by name and address...\n")
    cat("   This may take a few minutes - please wait...\n")
    
    for(i in which(need_geocoding)) {
      if(i %% 5 == 1) cat("   Progress:", i, "of", sum(need_geocoding), "\n")
      
      geocoded <- list(lat = NA, lng = NA)
      
      # First priority: If note contains address-like patterns, try geocoding that
      if(!is.na(all_data$note[i]) && all_data$note[i] != "") {
        note_text <- all_data$note[i]
        # Check if note contains address-like patterns
        if(grepl("\\d+.*\\b(Street|St|Avenue|Ave|Road|Rd|Drive|Dr|Boulevard|Blvd|Lane|Ln|Way|Court|Ct|Place|Pl)\\b", note_text, ignore.case = TRUE) ||
           grepl("\\b(Portland|OR|Oregon|97\\d{3})\\b", note_text, ignore.case = TRUE)) {
          cat("   Trying address from note for:", all_data$title[i], "\n")
          geocoded <- geocode_place(note_text)
        }
      }
      
      # Second priority: If address geocoding failed, try automatic address lookup for businesses only
      if(is.na(geocoded$lat)) {
        # Temporarily skip address lookup for faster processing
        # (Can be re-enabled later for more accurate coordinates)
      }
      
      # Third priority: If address lookup failed, try by title alone
      if(is.na(geocoded$lat)) {
        geocoded <- geocode_place(all_data$title[i])
      }
      
      if(!is.na(geocoded$lat)) {
        all_data$lat[i] <- geocoded$lat
        all_data$lng[i] <- geocoded$lng
      }
      
      # Be respectful to geocoding services
      if(i < max(which(need_geocoding))) Sys.sleep(0.5)
    }
  }
  
  # Skip auto-verification for faster processing - can be enabled later if needed
  cat("Skipping address verification for faster processing\n")
  
  # Final validation
  valid_coords <- !is.na(all_data$lat) & !is.na(all_data$lng)
  cat("Total places with coordinates:", sum(valid_coords), "out of", nrow(all_data), "\n")
  
  if(sum(!valid_coords) > 0) {
    failed_places <- all_data$title[!valid_coords]
    cat("Warning: Could not locate:", sum(!valid_coords), "places\n")
    if(sum(!valid_coords) <= 5) {
      cat("   Failed places:", paste(failed_places, collapse = ", "), "\n")
    }
  }
  
  # Keep only valid places
  all_data <- all_data[valid_coords, ]
  
  if(nrow(all_data) == 0) {
    stop("❌ No places could be located! Check your CSV files and internet connection.")
  }
  
  # Add distance from home
  cat("Calculating distances from home...\n")
  all_data$distance_mi <- mapply(calc_distance_miles, all_data$lat, all_data$lng)
  
  # Load neighborhood boundaries and assign neighborhoods
  cat("Loading neighborhood boundaries...\n")
  boundaries <- load_neighborhood_boundaries()
  
  if (!is.null(boundaries)) {
    cat("Assigning neighborhoods using spatial intersection...\n")
    all_data$neighborhood <- assign_neighborhoods(all_data, boundaries)
    neighborhoods_assigned <- sum(!is.na(all_data$neighborhood))
    cat("   Successfully assigned", neighborhoods_assigned, "neighborhoods\n")
  } else {
    cat("Using simplified section assignment...\n")
    all_data$neighborhood <- NA
  }
  
  # Load sextant boundaries and assign sextants
  cat("Loading sextant boundaries...\n")
  sextant_boundaries <- load_sextant_boundaries()
  
  if (!is.null(sextant_boundaries)) {
    cat("Assigning sextants using spatial intersection...\n")
    all_data$section <- assign_sextants(all_data, sextant_boundaries)
    sextants_assigned <- sum(!is.na(all_data$section))
    cat("   Successfully assigned", sextants_assigned, "sextants\n")
  } else {
    cat("No sextant assignment...\n")
    all_data$section <- NA
  }
  
  # Create unique IDs
  all_data$id <- paste0(all_data$source_list, "_", seq_len(nrow(all_data)))
  
  # Add processing metadata
  all_data$processed_date <- Sys.Date()
  all_data$home_address <- HOME_ADDRESS
  
  cat("Saving processed data...\n")
  
  # Create data directory
  dir.create("data", showWarnings = FALSE)
  
  # Save as RDS for fast loading
  saveRDS(all_data, "data/portland_places_processed.rds")
  
  # Also save as CSV for inspection
  write_csv(all_data, "data/portland_places_processed.csv")
  
  cat("Processing complete!\n")
  cat("Summary:\n")
  cat("   Total places:", nrow(all_data), "\n")
  cat("   From URLs:", sum(url_coords), "\n") 
  cat("   Geocoded:", sum(valid_coords) - sum(url_coords), "\n")
  if (!is.null(boundaries)) {
    neighborhoods_with_data <- sum(!is.na(all_data$neighborhood))
    cat("   Neighborhoods assigned:", neighborhoods_with_data, "\n")
  }
  if (!is.null(sextant_boundaries)) {
    sextants_with_data <- sum(!is.na(all_data$section))
    cat("   Sextants assigned:", sextants_with_data, "\n")
  }
  cat("   Average distance from home:", round(mean(all_data$distance_mi, na.rm = TRUE), 1), "miles\n")
  cat("   Data saved to: data/portland_places_processed.rds\n")
  cat("\n Ready to launch the app with: source('portland_day_planner.R')\n")
  
  return(all_data)
}

# ---------------- RUN PROCESSING ----------------

# Check if processed data already exists and is recent
processed_file <- "data/portland_places_processed.rds"
csv_files <- list.files("Saved", pattern = "\\.csv$", full.names = TRUE)

should_reprocess <- TRUE

if(file.exists(processed_file) && length(csv_files) > 0) {
  processed_date <- file.mtime(processed_file)
  csv_dates <- sapply(csv_files, file.mtime)
  newest_csv <- max(csv_dates)
  
  if(processed_date > newest_csv) {
    cat("Processed data is up-to-date (", format(processed_date), ")\n")
    cat("CSV files haven't changed since last processing.\n")
    cat("You can launch the app directly: source('portland_day_planner.R')\n")
    cat("Or run this script again to force reprocessing.\n")
    
    response <- readline("Reprocess anyway? (y/N): ")
    should_reprocess <- tolower(substr(response, 1, 1)) == "y"
  }
}

if(should_reprocess) {
  places_data <- process_csv_data()
} else {
  cat("⏭️  Skipping processing - using existing data.\n")
}