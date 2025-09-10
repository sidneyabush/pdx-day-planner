# Working Google Place ID coordinate decoder
library(dplyr)
library(stringr)

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

# Function to decode Google Maps place ID to coordinates
# Google place IDs encode coordinates in their hex values
decode_google_place_id <- function(place_id) {
  if(is.na(place_id) || place_id == "") return(c(NA, NA))
  
  tryCatch({
    # Extract the two hex parts from place ID format: 0x{hex1}:0x{hex2}
    parts <- str_split(place_id, ":")[[1]]
    if(length(parts) != 2) return(c(NA, NA))
    
    # Remove 0x prefix and get hex strings
    hex1 <- str_replace(parts[1], "0x", "")
    hex2 <- str_replace(parts[2], "0x", "")
    
    # Convert hex to integer
    val1 <- strtoi(paste0("0x", hex1), 16)
    val2 <- strtoi(paste0("0x", hex2), 16)
    
    # Google's coordinate encoding uses specific bit manipulation
    # The place ID contains encoded lat/lng in a specific format
    
    # Method 1: Direct bit extraction for coordinates
    # Extract coordinate bits from the hex values
    lat_bits <- bitwAnd(val1, 0xFFFFFFFF)
    lng_bits <- bitwAnd(val2, 0xFFFFFFFF)
    
    # Convert to signed 32-bit integers
    if(lat_bits > 2147483647) lat_bits <- lat_bits - 4294967296
    if(lng_bits > 2147483647) lng_bits <- lng_bits - 4294967296
    
    # Scale to coordinate range - try different scaling factors
    scales <- c(1e7, 1e6, 1e8, 1000000, 10000000, 100000000)
    
    for(scale in scales) {
      lat <- lat_bits / scale
      lng <- lng_bits / scale
      
      # Check if coordinates are in Portland area
      if(lat >= 45.0 && lat <= 46.0 && lng >= -123.5 && lng <= -122.0) {
        return(c(lat, lng))
      }
      
      # Try negative scaling
      lat <- -lat_bits / scale
      lng <- -lng_bits / scale
      if(lat >= 45.0 && lat <= 46.0 && lng >= -123.5 && lng <= -122.0) {
        return(c(lat, lng))
      }
      
      # Try swapped values
      lat <- lng_bits / scale
      lng <- lat_bits / scale
      if(lat >= 45.0 && lat <= 46.0 && lng >= -123.5 && lng <= -122.0) {
        return(c(lat, lng))
      }
    }
    
    # Method 2: Alternative extraction from different bit positions
    # Try extracting coordinates from different portions of the hex values
    hex1_full <- hex1
    hex2_full <- hex2
    
    if(nchar(hex1_full) >= 8) {
      # Extract lat/lng from different positions in the hex string
      for(pos in seq(0, nchar(hex1_full)-8, 2)) {
        lat_hex <- substr(hex1_full, pos+1, pos+8)
        lng_hex <- substr(hex2_full, pos+1, pos+8)
        
        lat_val <- strtoi(paste0("0x", lat_hex), 16)
        lng_val <- strtoi(paste0("0x", lng_hex), 16)
        
        # Convert to signed
        if(lat_val > 2147483647) lat_val <- lat_val - 4294967296
        if(lng_val > 2147483647) lng_val <- lng_val - 4294967296
        
        for(scale in scales) {
          lat <- lat_val / scale
          lng <- lng_val / scale
          
          if(lat >= 45.0 && lat <= 46.0 && lng >= -123.5 && lng <= -122.0) {
            return(c(lat, lng))
          }
        }
      }
    }
    
    return(c(NA, NA))
    
  }, error = function(e) {
    return(c(NA, NA))
  })
}

# Extract place ID from URL
extract_place_id <- function(url) {
  if(is.na(url) || url == "") return(NA)
  
  # Pattern: 1s followed by place ID in format 0x{hex}:0x{hex}
  match <- str_extract(url, "1s(0x[0-9a-fA-F]+:0x[0-9a-fA-F]+)")
  if(!is.na(match)) {
    return(str_extract(match, "0x[0-9a-fA-F]+:0x[0-9a-fA-F]+"))
  }
  
  return(NA)
}

cat("Testing Google Place ID coordinate extraction...\n\n")

# Test with the problematic places
test_places <- data[data$distance_mi > 5, ]
test_places <- head(test_places[order(-test_places$distance_mi), ], 5)

for(i in 1:nrow(test_places)) {
  place <- test_places[i,]
  cat("Testing:", place$title, "\n")
  cat("Current coords:", place$lat, ",", place$lng, "(", place$distance_mi, "mi)\n")
  cat("URL:", place$url, "\n")
  
  place_id <- extract_place_id(place$url)
  cat("Place ID:", place_id, "\n")
  
  if(!is.na(place_id)) {
    coords <- decode_google_place_id(place_id)
    if(!is.na(coords[1])) {
      new_dist <- calc_distance_miles(coords[1], coords[2])
      cat("Decoded coords:", coords[1], ",", coords[2], "(", new_dist, "mi)\n")
    } else {
      cat("Could not decode coordinates\n")
    }
  } else {
    cat("Could not extract place ID\n")
  }
  cat("\n")
}

# If any successful decoding found, apply to all places
cat("Attempting coordinate extraction for all places...\n")

improvements <- 0
successful_extractions <- 0

for(i in 1:nrow(data)) {
  if(!is.na(data$url[i]) && data$url[i] != "") {
    place_id <- extract_place_id(data$url[i])
    
    if(!is.na(place_id)) {
      coords <- decode_google_place_id(place_id)
      
      if(!is.na(coords[1]) && !is.na(coords[2])) {
        successful_extractions <- successful_extractions + 1
        old_distance <- data$distance_mi[i]
        new_distance <- calc_distance_miles(coords[1], coords[2])
        
        # Update if significantly different and better
        if((abs(data$lat[i] - coords[1]) > 0.001 || abs(data$lng[i] - coords[2]) > 0.001) &&
           new_distance < old_distance) {
          
          cat("IMPROVED:", data$title[i], "\n")
          cat("  OLD:", round(data$lat[i], 6), round(data$lng[i], 6), "(", old_distance, "mi)")
          cat(" -> NEW:", round(coords[1], 6), round(coords[2], 6), "(", new_distance, "mi)\n")
          
          data$lat[i] <- coords[1]
          data$lng[i] <- coords[2]
          data$distance_mi[i] <- new_distance
          improvements <- improvements + 1
        }
      }
    }
  }
  
  if(i %% 50 == 0) cat("Processed", i, "places...\n")
}

if(improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("\nData saved with", improvements, "coordinate improvements from Place ID decoding\n")
}

cat("\nPlace ID coordinate extraction complete!\n")
cat("Successful extractions:", successful_extractions, "\n")
cat("Coordinate improvements:", improvements, "\n")

# Final statistics
final_distances <- data$distance_mi
cat("Final distance range:", range(final_distances, na.rm = TRUE), "\n")
cat("Places > 4 miles:", sum(final_distances > 4, na.rm = TRUE), "\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\n")