# Manual coordinate fixes for places with known accurate coordinates
# This is more reliable than geocoding for specific places

library(dplyr)

# Load current processed data
data <- readRDS('data/portland_places_processed.rds')

cat("Applying manual coordinate fixes for known accurate locations...\n")

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

# Known accurate coordinates from Google Maps URLs (you provided Lei Brilla example)
# Add more as we identify them with wrong coordinates
manual_fixes <- list(
  "Lei Brilla" = list(lat = 45.5353428, lng = -122.7012357),
  
  # Add other places with known coordinates here
  # You can get these from the full Google Maps URLs when you click on places
  
  # Example format:
  # "Place Name" = list(lat = 45.XXXXX, lng = -122.XXXXX),
  
  # Common Portland landmarks with well-known coordinates
  "Powell's City of Books" = list(lat = 45.52306, lng = -122.68194),
  "Pioneer Courthouse Square" = list(lat = 45.51919, lng = -122.67935),
  "Portland Saturday Market" = list(lat = 45.52266, lng = -122.67089),
  "OMSI" = list(lat = 45.50805, lng = -122.66516),
  "Portland Japanese Garden" = list(lat = 45.51847, lng = -122.71597),
  "Mt. Tabor Park" = list(lat = 45.51640, lng = -122.59340),
  "Forest Park" = list(lat = 45.57640, lng = -122.76950)
)

improvements <- 0

for(place_name in names(manual_fixes)) {
  # Find matching places (case-insensitive partial match)
  place_indices <- which(grepl(place_name, data$title, ignore.case = TRUE))
  
  if(length(place_indices) > 0) {
    coords <- manual_fixes[[place_name]]
    new_distance <- calc_distance_miles(coords$lat, coords$lng)
    
    for(idx in place_indices) {
      old_distance <- data$distance_mi[idx]
      
      cat("Fixing", data$title[idx], 
          "from", paste(data$lat[idx], data$lng[idx], sep=","), 
          "to", paste(coords$lat, coords$lng, sep=","), "\n")
      cat("  Distance: ", old_distance, "->", new_distance, "miles\n")
      
      data$lat[idx] <- coords$lat
      data$lng[idx] <- coords$lng
      data$distance_mi[idx] <- new_distance
      improvements <- improvements + 1
    }
  }
}

# Save updated data
saveRDS(data, 'data/portland_places_processed.rds')

cat("\nManual coordinate fixes complete!\n")
cat("Total fixes applied:", improvements, "\n")

# Show final statistics
final_distances <- data$distance_mi
cat("Final distance range:", range(final_distances, na.rm = TRUE), "\n")
cat("Places > 6 miles:", sum(final_distances > 6, na.rm = TRUE), "\n")

# Show some examples of the fixed places
cat("\nFixed places:\n")
for(place_name in names(manual_fixes)) {
  fixed_places <- data[grepl(place_name, data$title, ignore.case = TRUE), c("title", "distance_mi")]
  if(nrow(fixed_places) > 0) print(fixed_places)
}