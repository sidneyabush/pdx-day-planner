# Fix the flower coordinates as specified by the user
library(dplyr)

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

# Find flower entry
flower_idx <- which(grepl("flower", data$title, ignore.case = TRUE))

if(length(flower_idx) > 0) {
  idx <- flower_idx[1]
  
  cat("Fixing flower coordinates as specified by user\\n")
  cat("Current:", data$title[idx], "at", data$lat[idx], ",", data$lng[idx], "(", data$distance_mi[idx], "mi)\\n")
  
  # Set to the coordinates you specified: @45.5124285,-122.6282771
  new_lat <- 45.5124285
  new_lng <- -122.6282771
  new_distance <- calc_distance_miles(new_lat, new_lng)
  
  data$lat[idx] <- new_lat
  data$lng[idx] <- new_lng
  data$distance_mi[idx] <- new_distance
  
  cat("Updated:", data$title[idx], "to", new_lat, ",", new_lng, "(", new_distance, "mi)\\n")
  
  # Save the data
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("Data saved with flower coordinate fix\\n")
} else {
  cat("No flower entry found\\n")
}