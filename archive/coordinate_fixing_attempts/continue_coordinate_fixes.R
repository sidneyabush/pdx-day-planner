# Continue fetching coordinates for remaining problem places
library(dplyr)
library(httr)
library(stringr)

source("fetch_real_coordinates.R", local = TRUE)

# Load updated data
data <- readRDS('data/portland_places_processed.rds')

cat("Continuing coordinate fixes...\n")
cat("Current status:\n")
cat("Places > 5 miles:", sum(data$distance_mi > 5, na.rm = TRUE), "\n")
cat("Places > 4 miles:", sum(data$distance_mi > 4, na.rm = TRUE), "\n\n")

# Get remaining problem places (>4 miles)
problem_places <- data[data$distance_mi > 4 & !is.na(data$distance_mi), ]
problem_places <- problem_places[order(-problem_places$distance_mi), ]

cat("Processing next", min(20, nrow(problem_places)), "problematic places...\n\n")

improvements <- 0

for(i in 1:min(20, nrow(problem_places))) {
  place_idx <- which(data$title == problem_places$title[i])[1]
  
  coords <- get_real_coordinates(problem_places$url[i], problem_places$title[i])
  
  if(!is.na(coords[1]) && !is.na(coords[2])) {
    old_distance <- data$distance_mi[place_idx]
    new_distance <- calc_distance_miles(coords[1], coords[2])
    
    # Only update if it's actually better
    if(new_distance < old_distance) {
      cat("UPDATING:", data$title[place_idx], "\n")
      cat("  OLD:", round(data$lat[place_idx], 4), round(data$lng[place_idx], 4), "(", old_distance, "mi)")
      cat(" -> NEW:", round(coords[1], 4), round(coords[2], 4), "(", new_distance, "mi)\n\n")
      
      data$lat[place_idx] <- coords[1]
      data$lng[place_idx] <- coords[2]
      data$distance_mi[place_idx] <- new_distance
      improvements <- improvements + 1
    } else {
      cat("FOUND coordinates for", data$title[place_idx], "but they're not better (", new_distance, "vs", old_distance, ")\n\n")
    }
  }
  
  # Be respectful to Google's servers
  Sys.sleep(1.5)
}

if(improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("Data saved with", improvements, "additional coordinate improvements\n")
}

cat("\nBatch complete! Improvements made:", improvements, "\n")

# Final status
final_distances <- data$distance_mi
cat("Final status:\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\n")
cat("Places > 4 miles:", sum(final_distances > 4, na.rm = TRUE), "\n")
cat("Places > 3 miles:", sum(final_distances > 3, na.rm = TRUE), "\n")