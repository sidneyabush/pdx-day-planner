# Expand Google Maps URLs to get full URLs with coordinates
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

# Function to expand a Google Maps URL and extract coordinates
expand_and_extract <- function(short_url, place_name) {
  cat("Processing:", place_name, "\n")
  
  if(is.na(short_url) || short_url == "") {
    cat("  No URL available\n")
    return(list(lat = NA, lng = NA))
  }
  
  tryCatch({
    # Make a HEAD request to get the redirect without downloading content
    response <- HEAD(short_url, timeout(10))
    
    if(status_code(response) == 200) {
      # Get the final URL after redirects
      final_url <- response$url
      cat("  Final URL:", substr(final_url, 1, 100), "...\n")
      
      # Try to extract coordinates using both patterns
      # Pattern 1: !3d!4d format
      matches1 <- str_match(final_url, '!3d(-?[0-9]+\\.?[0-9]*)!4d(-?[0-9]+\\.?[0-9]*)')
      if(!is.na(matches1[1])) {
        lat <- as.numeric(matches1[2])
        lng <- as.numeric(matches1[3])
        cat("  Found coordinates (3d/4d):", lat, lng, "\n")
        return(list(lat = lat, lng = lng))
      }
      
      # Pattern 2: @lat,lng format
      matches2 <- str_match(final_url, '@(-?[0-9]+\\.?[0-9]*),(-?[0-9]+\\.?[0-9]*)')
      if(!is.na(matches2[1])) {
        lat <- as.numeric(matches2[2])  
        lng <- as.numeric(matches2[3])
        cat("  Found coordinates (@):", lat, lng, "\n")
        return(list(lat = lat, lng = lng))
      }
      
      cat("  No coordinates found in expanded URL\n")
    } else {
      cat("  HTTP error:", status_code(response), "\n")
    }
  }, error = function(e) {
    cat("  Error expanding URL:", e$message, "\n")
  })
  
  return(list(lat = NA, lng = NA))
}

# Test with a few of the problematic places first
test_places <- data[data$distance_mi > 7, ]
cat("Testing", nrow(test_places), "places with distance > 7 miles\n\n")

improvements <- 0

# Process first 5 as a test
for(i in 1:min(5, nrow(test_places))) {
  idx <- which(data$id == test_places$id[i])
  result <- expand_and_extract(test_places$url[i], test_places$title[i])
  
  if(!is.na(result$lat) && !is.na(result$lng)) {
    # Check if coordinates are in reasonable Portland bounds
    if(result$lat >= 45.3 && result$lat <= 45.8 && result$lng >= -123.0 && result$lng <= -122.4) {
      new_distance <- calc_distance_miles(result$lat, result$lng)
      old_distance <- data$distance_mi[idx]
      
      cat("  Updating coordinates: ", data$lat[idx], ",", data$lng[idx], 
          " -> ", result$lat, ",", result$lng, "\n")
      cat("  Distance change: ", old_distance, " -> ", new_distance, " miles\n\n")
      
      data$lat[idx] <- result$lat
      data$lng[idx] <- result$lng  
      data$distance_mi[idx] <- new_distance
      improvements <- improvements + 1
    } else {
      cat("  Coordinates outside Portland bounds, skipping\n\n")
    }
  } else {
    cat("  Could not extract coordinates\n\n")
  }
  
  # Small delay to be respectful
  Sys.sleep(1)
}

cat("Test complete!\n")
cat("Improvements made:", improvements, "\n")

if(improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("Data saved with improvements\n")
}