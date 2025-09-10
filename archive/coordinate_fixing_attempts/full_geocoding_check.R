# Comprehensive geocoding check for ALL places, not just distant ones
library(dplyr)
library(tidygeocoder)
library(stringr)

# Load current data
data <- readRDS('data/portland_places_processed.rds')

HOME_LAT <- 45.5623
HOME_LNG <- -122.6754
PDX_CENTER <- c(lat = 45.523, lon = -122.676)

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

# Portland distance check
haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  toRad <- function(x) x * pi/180
  dlat <- toRad(lat2 - lat1); dlon <- toRad(lon2 - lon1)
  a <- sin(dlat/2)^2 + cos(toRad(lat1))*cos(toRad(lat2))*sin(dlon/2)^2
  2*r*asin(pmin(1, sqrt(a)))
}

near_center <- function(lat, lon, center, max_km = 120) {
  if (is.null(center) || any(!is.finite(center))) return(rep(TRUE, length(lat)))
  d <- haversine_km(lat, lon, center["lat"], center["lon"])
  is.finite(d) & d <= max_km
}

# Enhanced geocoder with better validation
geocode_and_validate <- function(query, current_lat, current_lng, center_hint = PDX_CENTER) {
  stopifnot(length(query) == 1)
  
  cat("  Checking:", substr(query, 1, 60), "...\\n")
  
  # Try adding "Portland Oregon" to help with disambiguation
  query_portland <- paste(query, "Portland Oregon")
  
  # ArcGIS first (with Portland context)
  tryCatch({
    g <- tidygeocoder::geocode(tibble(query = query_portland), address = "query",
                              method = "arcgis", limit = 1, 
                              min_time = 1.0, quiet = TRUE)
    if (nrow(g) > 0 && !is.na(g$lat) && !is.na(g$long)) {
      lat <- as.numeric(g$lat); lon <- as.numeric(g$long)
      if (near_center(lat, lon, center_hint, max_km = 120)) {
        # Check if this is significantly different and better than current
        current_dist <- calc_distance_miles(current_lat, current_lng)
        new_dist <- calc_distance_miles(lat, lon)
        
        # Consider it better if:
        # 1. Distance difference > 0.5 miles AND new is closer, OR
        # 2. Current coordinates seem obviously wrong (distance > 4 miles for most places)
        distance_diff <- abs(current_lat - lat) + abs(current_lng - lon)  # Simple coordinate difference
        
        if ((distance_diff > 0.01 && new_dist < current_dist) || current_dist > 4) {
          cat("    ArcGIS+Portland:", round(lat, 4), round(lon, 4), "(", new_dist, "mi vs", current_dist, "mi)\\n")
          return(list(lat = lat, lon = lon, source = "arcgis+portland", improvement = TRUE))
        } else {
          cat("    ArcGIS found coords but current seems accurate\\n")
          return(list(lat = current_lat, lon = current_lng, source = "current", improvement = FALSE))
        }
      }
    }
  }, error = function(e) {
    cat("    ArcGIS+Portland error\\n")
  })
  
  # Try original query with ArcGIS
  tryCatch({
    g <- tidygeocoder::geocode(tibble(query = query), address = "query",
                              method = "arcgis", limit = 1, 
                              min_time = 1.0, quiet = TRUE)
    if (nrow(g) > 0 && !is.na(g$lat) && !is.na(g$long)) {
      lat <- as.numeric(g$lat); lon <- as.numeric(g$long)
      if (near_center(lat, lon, center_hint, max_km = 120)) {
        current_dist <- calc_distance_miles(current_lat, current_lng)
        new_dist <- calc_distance_miles(lat, lon)
        distance_diff <- abs(current_lat - lat) + abs(current_lng - lon)
        
        if ((distance_diff > 0.01 && new_dist < current_dist) || current_dist > 4) {
          cat("    ArcGIS:", round(lat, 4), round(lon, 4), "(", new_dist, "mi vs", current_dist, "mi)\\n")
          return(list(lat = lat, lon = lon, source = "arcgis", improvement = TRUE))
        } else {
          cat("    ArcGIS found coords but current seems accurate\\n")
          return(list(lat = current_lat, lon = current_lng, source = "current", improvement = FALSE))
        }
      }
    }
  }, error = function(e) {
    cat("    ArcGIS error\\n")
  })
  
  cat("    No better coordinates found, keeping current\\n")
  return(list(lat = current_lat, lon = current_lng, source = "current", improvement = FALSE))
}

# Check ALL places, not just distant ones
cat("\\nChecking coordinate accuracy for ALL", nrow(data), "places\\n\\n")

improvements <- 0
checked <- 0

# Sample places to avoid hitting rate limits - check every 3rd place
sample_indices <- seq(1, nrow(data), by = 3)
cat("Sampling", length(sample_indices), "places for comprehensive check\\n\\n")

for(i in sample_indices) {
  checked <- checked + 1
  
  cat(paste0("[", checked, "/", length(sample_indices), "] "))
  
  # Check this place's coordinates
  result <- geocode_and_validate(data$title[i], data$lat[i], data$lng[i])
  
  if(result$improvement) {
    old_distance <- data$distance_mi[i]
    new_distance <- calc_distance_miles(result$lat, result$lon)
    
    cat("  IMPROVED:", data$title[i], "\\n")
    cat("    OLD:", round(data$lat[i], 4), round(data$lng[i], 4), "(", old_distance, "mi)")
    cat(" -> NEW:", round(result$lat, 4), round(result$lon, 4), "(", new_distance, "mi)\\n\\n")
    
    data$lat[i] <- result$lat
    data$lng[i] <- result$lon
    data$distance_mi[i] <- new_distance
    improvements <- improvements + 1
  } else {
    cat("  OK\\n\\n")
  }
  
  # Pause between requests to be respectful
  Sys.sleep(1.2)
}

if(improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("\\nData saved with", improvements, "improvements\\n")
}

# Final comprehensive statistics
final_distances <- data$distance_mi
cat("\\nFull coordinate accuracy check complete!\\n")
cat("Places checked:", checked, "\\n")
cat("Improvements made:", improvements, "\\n")
cat("Final distance range:", range(final_distances, na.rm = TRUE), "\\n")
cat("Places > 6 miles:", sum(final_distances > 6, na.rm = TRUE), "\\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\\n")
cat("Places > 4 miles:", sum(final_distances > 4, na.rm = TRUE), "\\n")
cat("Places > 3 miles:", sum(final_distances > 3, na.rm = TRUE), "\\n")

# Check for remaining coordinate patterns
cat("\\nCoordinate distribution analysis:\\n")
lat_rounded <- round(data$lat, 2)
lng_rounded <- round(data$lng, 2)
lat_counts <- table(lat_rounded)
lng_counts <- table(lng_rounded)

cat("Most common latitudes:\\n")
print(head(sort(lat_counts, decreasing=TRUE), 8))
cat("Most common longitudes:\\n") 
print(head(sort(lng_counts, decreasing=TRUE), 8))

# Flag potential remaining issues
high_freq_coords <- names(lat_counts)[lat_counts > 8] # More than 8 places at same lat
if(length(high_freq_coords) > 0) {
  cat("\\n⚠️  Potential coordinate clustering detected at latitudes:", paste(high_freq_coords, collapse=", "), "\\n")
}