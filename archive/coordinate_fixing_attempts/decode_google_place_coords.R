# Decode coordinates from Google Place IDs in URLs
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

# Function to decode coordinates from Google Place ID hex string
decode_place_id_coords <- function(place_id) {
  if(is.na(place_id) || nchar(place_id) < 16) return(c(NA, NA))
  
  tryCatch({
    # Extract hex string (after 0x)
    hex_part <- str_extract(place_id, "(?<=0x)[0-9a-fA-F]+")
    if(is.na(hex_part) || nchar(hex_part) < 16) return(c(NA, NA))
    
    # Convert hex to raw bytes
    hex_bytes <- as.raw(strtoi(substring(hex_part, seq(1, nchar(hex_part), 2), 
                                         seq(2, nchar(hex_part), 2)), 16))
    
    # Google uses specific encoding - try multiple decoding approaches
    
    # Method 1: Simple coordinate extraction from hex pattern
    # Look for coordinate-like patterns in the hex string
    if(nchar(hex_part) >= 16) {
      # Extract coordinate candidates from different positions
      coords_found <- FALSE
      
      # Try extracting from different byte positions
      for(offset in seq(0, min(8, nchar(hex_part)-16), 2)) {
        lat_hex <- substr(hex_part, offset+1, offset+8)
        lng_hex <- substr(hex_part, offset+9, offset+16)
        
        # Convert to signed integers and scale to coordinate range
        lat_raw <- strtoi(paste0("0x", lat_hex), 16)
        lng_raw <- strtoi(paste0("0x", lng_hex), 16)
        
        # Convert to signed 32-bit integers
        if(lat_raw > 2147483647) lat_raw <- lat_raw - 4294967296
        if(lng_raw > 2147483647) lng_raw <- lng_raw - 4294967296
        
        # Scale to coordinate range (common Google encoding)
        lat <- lat_raw / 1e7
        lng <- lng_raw / 1e7
        
        # Check if coordinates are reasonable for Portland area
        if(lat >= 45.0 && lat <= 46.0 && lng >= -123.5 && lng <= -122.0) {
          return(c(lat, lng))
        }
        
        # Try different scaling factors
        for(scale in c(1e6, 1e8, 1000000, 10000000)) {
          lat_scaled <- lat_raw / scale
          lng_scaled <- lng_raw / scale
          
          if(lat_scaled >= 45.0 && lat_scaled <= 46.0 && 
             lng_scaled >= -123.5 && lng_scaled <= -122.0) {
            return(c(lat_scaled, lng_scaled))
          }
        }
      }
    }
    
    return(c(NA, NA))
    
  }, error = function(e) {
    return(c(NA, NA))
  })
}

# Function to extract Place ID from URL
extract_place_id <- function(url) {
  if(is.na(url) || url == "") return(NA)
  
  # Look for place ID patterns in Google Maps URLs
  # Pattern: !1s followed by hex string
  place_id <- str_extract(url, "!1s(0x[0-9a-fA-F]+:[0-9a-fA-F]+)")
  if(!is.na(place_id)) {
    return(str_extract(place_id, "0x[0-9a-fA-F]+:[0-9a-fA-F]+"))
  }
  
  # Alternative pattern
  place_id <- str_extract(url, "1s(0x[0-9a-fA-F]+:[0-9a-fA-F]+)")
  if(!is.na(place_id)) {
    return(str_extract(place_id, "0x[0-9a-fA-F]+:[0-9a-fA-F]+"))
  }
  
  return(NA)
}

# Test with a few examples first
cat("Testing Place ID coordinate extraction...\n\n")

# Test with first 5 URLs
test_urls <- head(data[!is.na(data$url) & data$url != "", ], 5)

for(i in 1:nrow(test_urls)) {
  cat("Testing:", test_urls$title[i], "\n")
  cat("URL:", test_urls$url[i], "\n")
  
  place_id <- extract_place_id(test_urls$url[i])
  cat("Place ID:", place_id, "\n")
  
  if(!is.na(place_id)) {
    coords <- decode_place_id_coords(place_id)
    cat("Extracted coords:", coords[1], coords[2], "\n")
    
    if(!is.na(coords[1])) {
      dist <- calc_distance_miles(coords[1], coords[2])
      cat("Distance from home:", dist, "miles\n")
    }
  }
  cat("\n")
}

# If the test above shows promise, process all URLs
cat("Processing all URLs for Place ID coordinate extraction...\n\n")

improvements <- 0
processed <- 0

for(i in 1:nrow(data)) {
  if(!is.na(data$url[i]) && data$url[i] != "") {
    processed <- processed + 1
    
    if(processed %% 50 == 0) {
      cat("Processed", processed, "URLs...\n")
    }
    
    place_id <- extract_place_id(data$url[i])
    
    if(!is.na(place_id)) {
      coords <- decode_place_id_coords(place_id)
      
      if(!is.na(coords[1]) && !is.na(coords[2])) {
        old_distance <- data$distance_mi[i]
        new_distance <- calc_distance_miles(coords[1], coords[2])
        
        # Update if coordinates are significantly different
        if(abs(data$lat[i] - coords[1]) > 0.001 || 
           abs(data$lng[i] - coords[2]) > 0.001) {
          
          cat("UPDATED:", data$title[i], "\n")
          cat("  OLD:", round(data$lat[i], 6), round(data$lng[i], 6), "(", old_distance, "mi)")
          cat(" -> NEW:", round(coords[1], 6), round(coords[2], 6), "(", new_distance, "mi)\n\n")
          
          data$lat[i] <- coords[1]
          data$lng[i] <- coords[2]
          data$distance_mi[i] <- new_distance
          improvements <- improvements + 1
        }
      }
    }
  }
}

if(improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("\nData saved with", improvements, "Place ID coordinate extractions\n")
} else {
  cat("\nNo coordinate improvements found from Place ID extraction\n")
}

cat("\nPlace ID coordinate extraction complete!\n")
cat("URLs processed:", processed, "\n")
cat("Coordinates extracted and updated:", improvements, "\n")

# Final statistics
final_distances <- data$distance_mi
cat("Final distance range:", range(final_distances, na.rm = TRUE), "\n")
cat("Places > 4 miles:", sum(final_distances > 4, na.rm = TRUE), "\n")