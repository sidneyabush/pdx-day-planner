# Comprehensive coordinate fix for all Portland places
# This will re-geocode places systematically to improve accuracy

library(dplyr)
library(readr)
library(tidygeocoder)

# Load current processed data
data <- readRDS('data/portland_places_processed.rds')

cat("Starting comprehensive coordinate fix...\n")
cat("Total places:", nrow(data), "\n")

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

# Function to re-geocode a place
geocode_place <- function(title, current_lat, current_lng, current_distance) {
  if(title == "Lei Brilla") {
    # We already fixed this one with known coordinates
    return(list(lat = current_lat, lng = current_lng, distance = current_distance, method = "known"))
  }
  
  # Skip if current distance seems reasonable (less than 5 miles)
  if(current_distance <= 5) {
    return(list(lat = current_lat, lng = current_lng, distance = current_distance, method = "kept"))
  }
  
  cat("Re-geocoding:", title, "(current distance:", current_distance, "miles)\n")
  
  # Try different query formats
  queries <- c(
    paste(title, "Portland OR"),
    paste(title, "Portland Oregon"), 
    paste(title, "Portland")
  )
  
  methods <- c("osm", "cascade")
  
  for(method in methods) {
    for(query in queries) {
      result <- tryCatch({
        tidygeocoder::geo(query, method = method, quiet = TRUE)
      }, error = function(e) {
        data.frame(lat = NA, long = NA)
      })
      
      if(!is.na(result$lat) && !is.na(result$long)) {
        new_distance <- calc_distance_miles(result$lat, result$long)
        
        # Only accept if it's closer and within reasonable Portland area
        if(new_distance < current_distance && new_distance <= 8 && 
           result$lat >= 45.3 && result$lat <= 45.8 && 
           result$long >= -123.0 && result$long <= -122.4) {
          cat("  -> Improved:", query, "with", method, "- new distance:", new_distance, "miles\n")
          return(list(lat = result$lat, lng = result$long, distance = new_distance, method = paste(method, query)))
        }
      }
      
      # Small delay to be respectful to geocoding services
      Sys.sleep(0.1)
    }
  }
  
  # If no improvement found, keep original
  return(list(lat = current_lat, lng = current_lng, distance = current_distance, method = "no_improvement"))
}

# Process places with suspiciously high distances first
suspicious_places <- which(data$distance_mi > 5)
cat("Found", length(suspicious_places), "places with distance > 5 miles to re-geocode\n")

improvements <- 0
processed <- 0

for(i in suspicious_places) {
  processed <- processed + 1
  if(processed %% 10 == 0) cat("Progress:", processed, "of", length(suspicious_places), "\n")
  
  result <- geocode_place(data$title[i], data$lat[i], data$lng[i], data$distance_mi[i])
  
  if(result$method != "kept" && result$method != "no_improvement") {
    data$lat[i] <- result$lat
    data$lng[i] <- result$lng
    data$distance_mi[i] <- result$distance
    improvements <- improvements + 1
  }
}

# Save the improved data
saveRDS(data, 'data/portland_places_processed.rds')

cat("\nCoordinate fix complete!\n")
cat("Total improvements made:", improvements, "\n")
cat("Places processed:", processed, "\n")

# Show final statistics
final_distances <- data$distance_mi
cat("Final distance range:", range(final_distances, na.rm = TRUE), "\n")
cat("Places > 6 miles:", sum(final_distances > 6, na.rm = TRUE), "\n")
cat("Places > 8 miles:", sum(final_distances > 8, na.rm = TRUE), "\n")

# Show some of the farthest places
far_places <- data[data$distance_mi > 6, c('title', 'distance_mi')]
if(nrow(far_places) > 0) {
  cat("\nRemaining places > 6 miles:\n")
  print(head(far_places[order(-far_places$distance_mi), ], 10))
}