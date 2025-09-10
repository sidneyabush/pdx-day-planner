# Extract ACTUAL coordinates from Google Maps URLs - no more approximations
library(dplyr)
library(stringr)
library(httr)

# Load current data
data <- readRDS('data/portland_places_processed.rds')

HOME_LAT <- 45.5623
HOME_LNG <- -122.6754

# Function to calculate distance
calc_distance_miles <- function(lat, lng, home_lat = HOME_LAT, home_lng = HOME_LNG) {
  R <- 3959  # Earth radius in miles
  lat1 <- home_lat * pi/180
  lat2 <- lat * pi/180
  dlat <- (lat - home_lat) * pi/180
  dlng <- (lng - home_lng) * pi/180
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlng/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  round(R * c, 1)
}

# Function to expand Google Maps URLs and get actual coordinates
get_real_coordinates <- function(url, place_name) {
  cat("Processing:", substr(place_name, 1, 50), "...\\n")
  
  if(is.na(url) || url == "") {
    cat("  No URL\\n")
    return(c(NA, NA))
  }
  
  # First try to extract from the URL directly (if it's already expanded)
  direct_coords <- extract_coords_from_url(url)
  if(!is.na(direct_coords[1])) {
    cat("  Direct extraction:", direct_coords[1], direct_coords[2], "\\n")
    return(direct_coords)
  }
  
  # If that fails, expand the URL
  tryCatch({
    # Follow redirects to get the full URL
    response <- httr::GET(url, httr::timeout(15))
    final_url <- response$url
    
    # Extract coordinates from expanded URL
    coords <- extract_coords_from_url(final_url)
    if(!is.na(coords[1])) {
      cat("  Expanded extraction:", coords[1], coords[2], "\\n")
      return(coords)
    }
    
    cat("  No coordinates found in expanded URL\\n")
    return(c(NA, NA))
    
  }, error = function(e) {
    cat("  Error expanding URL:", e$message, "\\n")
    return(c(NA, NA))
  })
}

# Comprehensive coordinate extraction function
extract_coords_from_url <- function(url) {
  if(is.na(url) || url == "") return(c(NA, NA))
  
  # Pattern 1: @lat,lng (most common in expanded URLs)
  match <- str_match(url, "@(-?[0-9]+\\\\.?[0-9]*),(-?[0-9]+\\\\.?[0-9]*)")
  if(!is.na(match[1,1])) {
    lat <- as.numeric(match[1,2])
    lng <- as.numeric(match[1,3])
    if(is.finite(lat) && is.finite(lng)) return(c(lat, lng))
  }
  
  # Pattern 2: !3d!4d format (place coordinates)
  match <- str_match(url, "!3d(-?[0-9]+\\\\.?[0-9]*)!4d(-?[0-9]+\\\\.?[0-9]*)")
  if(!is.na(match[1,1])) {
    lat <- as.numeric(match[1,2])
    lng <- as.numeric(match[1,3])
    if(is.finite(lat) && is.finite(lng)) return(c(lat, lng))
  }
  
  # Pattern 3: !2d!3d format (alternative place coordinates)
  match <- str_match(url, "!2d(-?[0-9]+\\\\.?[0-9]*)!3d(-?[0-9]+\\\\.?[0-9]*)")
  if(!is.na(match[1,1])) {
    lat <- as.numeric(match[1,3])  # Note: 3d is lat, 2d is lng
    lng <- as.numeric(match[1,2])
    if(is.finite(lat) && is.finite(lng)) return(c(lat, lng))
  }
  
  # Pattern 4: query parameters
  match <- str_match(url, "[?&]q=(-?[0-9]+\\\\.?[0-9]*),(-?[0-9]+\\\\.?[0-9]*)")
  if(!is.na(match[1,1])) {
    lat <- as.numeric(match[1,2])
    lng <- as.numeric(match[1,3])
    if(is.finite(lat) && is.finite(lng)) return(c(lat, lng))
  }
  
  return(c(NA, NA))
}

cat("Extracting REAL coordinates from URLs...\\n\\n")

# Test with the example you mentioned - "flower"
flower_idx <- which(grepl("flower", data$title, ignore.case = TRUE))
if(length(flower_idx) > 0) {
  cat("Testing with 'flower' entry:\\n")
  test_coords <- get_real_coordinates(data$url[flower_idx[1]], data$title[flower_idx[1]])
  cat("Expected: 45.5124285, -122.6282771\\n")
  cat("Got:", test_coords[1], test_coords[2], "\\n\\n")
}

improvements <- 0
total_processed <- 0

# Process all URLs to extract real coordinates
for(i in 1:nrow(data)) {
  if(i %% 50 == 0) cat("Processed", i, "of", nrow(data), "\\n")
  
  if(!is.na(data$url[i]) && data$url[i] != "") {
    total_processed <- total_processed + 1
    coords <- get_real_coordinates(data$url[i], data$title[i])
    
    if(!is.na(coords[1]) && !is.na(coords[2])) {
      # Validate coordinates are reasonable (Portland area)
      if(coords[1] >= 45.3 && coords[1] <= 45.8 && coords[2] >= -123.0 && coords[2] <= -122.4) {
        old_distance <- data$distance_mi[i]
        new_distance <- calc_distance_miles(coords[1], coords[2])
        
        # Update if coordinates are different
        if(abs(data$lat[i] - coords[1]) > 0.001 || abs(data$lng[i] - coords[2]) > 0.001) {
          cat("UPDATED:", data$title[i], "\\n")
          cat("  OLD:", round(data$lat[i], 6), round(data$lng[i], 6), "(", old_distance, "mi)\\n")
          cat("  NEW:", round(coords[1], 6), round(coords[2], 6), "(", new_distance, "mi)\\n\\n")
          
          data$lat[i] <- coords[1]
          data$lng[i] <- coords[2]
          data$distance_mi[i] <- new_distance
          improvements <- improvements + 1
        }
      } else {
        cat("  Coordinates outside Portland area, skipping\\n")
      }
    }
    
    # Small delay to be respectful
    Sys.sleep(0.5)
  }
}

if(improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("\\nData saved with", improvements, "real coordinate extractions\\n")
}

# Final statistics
final_distances <- data$distance_mi
cat("\\nReal URL coordinate extraction complete!\\n")
cat("URLs processed:", total_processed, "\\n")
cat("Coordinates extracted and updated:", improvements, "\\n")
cat("Final distance range:", range(final_distances, na.rm = TRUE), "\\n")
cat("Places > 6 miles:", sum(final_distances > 6, na.rm = TRUE), "\\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\\n")

# Check coordinate distribution
cat("\\nCoordinate distribution check:\\n")
lat_counts <- table(round(data$lat, 3))
lng_counts <- table(round(data$lng, 3))
cat("Max places at same latitude (to 3 decimals):", max(lat_counts), "\\n")
cat("Max places at same longitude (to 3 decimals):", max(lng_counts), "\\n")