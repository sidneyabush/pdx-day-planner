# Fix coordinate accuracy by re-geocoding places with known issues
library(dplyr)
library(readr)
library(tidygeocoder)

# Load current processed data
data <- readRDS('data/portland_places_processed.rds')

# Known incorrect places that need fixing (can add more as identified)
places_to_fix <- c("Lei Brilla")

# Function to geocode a place by name + "Portland OR"
fix_coordinates <- function(place_name) {
  cat("Fixing coordinates for:", place_name, "\n")
  
  # For Lei Brilla specifically, we know the correct coordinates from the Google Maps URL
  if(place_name == "Lei Brilla") {
    cat("  Using known coordinates from Google Maps URL\n")
    return(list(lat = 45.5353428, lng = -122.7012357))
  }
  
  # Try multiple geocoding approaches
  queries <- c(
    paste(place_name, "Portland OR"),
    paste(place_name, "Portland Oregon"),
    paste(place_name, "Portland"),
    place_name
  )
  
  methods <- c("osm", "cascade", "arcgis")
  
  for(method in methods) {
    for(query in queries) {
      cat("  Trying:", query, "with", method, "\n")
      result <- tryCatch({
        tidygeocoder::geo(query, method = method, quiet = TRUE)
      }, error = function(e) {
        data.frame(lat = NA, long = NA)
      })
      
      if(!is.na(result$lat) && !is.na(result$long)) {
        cat("  Found coordinates:", result$lat, ",", result$long, "\n")
        return(list(lat = result$lat, lng = result$long))
      }
    }
  }
  
  cat("  Could not geocode:", place_name, "\n")
  return(NULL)
}

# Fix coordinates for known problematic places
for(place in places_to_fix) {
  place_indices <- which(grepl(place, data$title, ignore.case = TRUE))
  
  if(length(place_indices) > 0) {
    new_coords <- fix_coordinates(place)
    
    if(!is.null(new_coords)) {
      for(idx in place_indices) {
        cat("Updating", data$title[idx], "from", 
            paste(data$lat[idx], data$lng[idx], sep=","), 
            "to", paste(new_coords$lat, new_coords$lng, sep=","), "\n")
        
        data$lat[idx] <- new_coords$lat
        data$lng[idx] <- new_coords$lng
        
        # Recalculate distance from home
        HOME_LAT <- 45.5623
        HOME_LNG <- -122.6754
        
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
        
        data$distance_mi[idx] <- calc_distance_miles(new_coords$lat, new_coords$lng)
      }
    }
  }
}

# Save updated data
saveRDS(data, 'data/portland_places_processed.rds')

cat("\nCoordinate fixes complete!\n")
cat("Verify Lei Brilla coordinates:\n")
lei_data <- data[grepl('Lei Brilla', data$title, ignore.case=TRUE), c('title', 'lat', 'lng', 'distance_mi')]
print(lei_data)