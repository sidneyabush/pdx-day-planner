# Fetch real coordinates by expanding Google Maps URLs
library(dplyr)
library(httr)
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

# Function to get real coordinates by following URL redirects
get_real_coordinates <- function(url, place_name) {
  if(is.na(url) || url == "") return(c(NA, NA))
  
  tryCatch({
    cat("Fetching:", substr(place_name, 1, 40), "...\n")
    
    # Follow redirects to get the expanded URL
    response <- GET(url, timeout(10))
    final_url <- response$url
    
    cat("  Final URL length:", nchar(final_url), "\n")
    
    # Extract coordinates from the final URL
    coords <- extract_coords_from_url(final_url)
    if(!is.na(coords[1])) {
      cat("  Found:", coords[1], coords[2], "\n")
      return(coords)
    }
    
    # Also try to extract from the response content if it's HTML
    if(response$headers$`content-type` %>% str_detect("text/html")) {
      content_text <- content(response, "text")
      
      # Look for coordinate patterns in the HTML content
      coord_patterns <- c(
        '"(-?\\d+\\.\\d+),(-?\\d+\\.\\d+)"',
        '@(-?\\d+\\.\\d+),(-?\\d+\\.\\d+)',
        'center=(-?\\d+\\.\\d+),(-?\\d+\\.\\d+)',
        'lat.*?(-?\\d+\\.\\d+).*?lng.*?(-?\\d+\\.\\d+)',
        'lng.*?(-?\\d+\\.\\d+).*?lat.*?(-?\\d+\\.\\d+)'
      )
      
      for(pattern in coord_patterns) {
        matches <- str_match_all(content_text, pattern)[[1]]
        if(nrow(matches) > 0) {
          for(i in 1:nrow(matches)) {
            lat <- as.numeric(matches[i, 2])
            lng <- as.numeric(matches[i, 3])
            
            # Check if coordinates are in Portland area
            if(!is.na(lat) && !is.na(lng) && 
               lat >= 45.0 && lat <= 46.0 && lng >= -123.5 && lng <= -122.0) {
              cat("  Found in content:", lat, lng, "\n")
              return(c(lat, lng))
            }
          }
        }
      }
    }
    
    cat("  No coordinates found\n")
    return(c(NA, NA))
    
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    return(c(NA, NA))
  })
}

# Function to extract coordinates from URL
extract_coords_from_url <- function(url) {
  if(is.na(url) || url == "") return(c(NA, NA))
  
  # Multiple coordinate patterns to try
  patterns <- c(
    "@(-?[0-9]+\\.?[0-9]*),(-?[0-9]+\\.?[0-9]*)",
    "!3d(-?[0-9]+\\.?[0-9]*)!4d(-?[0-9]+\\.?[0-9]*)",
    "!2d(-?[0-9]+\\.?[0-9]*)!3d(-?[0-9]+\\.?[0-9]*)",
    "center=(-?[0-9]+\\.?[0-9]*),(-?[0-9]+\\.?[0-9]*)",
    "[?&]q=(-?[0-9]+\\.?[0-9]*),(-?[0-9]+\\.?[0-9]*)"
  )
  
  for(pattern in patterns) {
    match <- str_match(url, pattern)
    if(!is.na(match[1,1])) {
      lat <- as.numeric(match[1,2])
      lng <- as.numeric(match[1,3])
      
      # For !2d!3d pattern, coordinates might be swapped
      if(str_detect(pattern, "!2d.*!3d")) {
        lng <- as.numeric(match[1,2])
        lat <- as.numeric(match[1,3])
      }
      
      # Validate coordinates are in Portland area
      if(is.finite(lat) && is.finite(lng) && 
         lat >= 45.0 && lat <= 46.0 && lng >= -123.5 && lng <= -122.0) {
        return(c(lat, lng))
      }
    }
  }
  
  return(c(NA, NA))
}

cat("Fetching real coordinates from Google Maps URLs...\n\n")

# Start with the most problematic places (>5 miles)
problem_places <- data[data$distance_mi > 5 & !is.na(data$distance_mi), ]
problem_places <- problem_places[order(-problem_places$distance_mi), ]

cat("Processing", nrow(problem_places), "places with distances > 5 miles...\n\n")

improvements <- 0

for(i in 1:min(10, nrow(problem_places))) {  # Start with top 10 worst
  place_idx <- which(data$title == problem_places$title[i])[1]
  
  coords <- get_real_coordinates(problem_places$url[i], problem_places$title[i])
  
  if(!is.na(coords[1]) && !is.na(coords[2])) {
    old_distance <- data$distance_mi[place_idx]
    new_distance <- calc_distance_miles(coords[1], coords[2])
    
    cat("UPDATING:", data$title[place_idx], "\n")
    cat("  OLD:", round(data$lat[place_idx], 4), round(data$lng[place_idx], 4), "(", old_distance, "mi)")
    cat(" -> NEW:", round(coords[1], 4), round(coords[2], 4), "(", new_distance, "mi)\n\n")
    
    data$lat[place_idx] <- coords[1]
    data$lng[place_idx] <- coords[2]
    data$distance_mi[place_idx] <- new_distance
    improvements <- improvements + 1
  }
  
  # Be respectful to Google's servers
  Sys.sleep(2)
}

if(improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("Data saved with", improvements, "coordinate improvements\n")
}

cat("\nReal coordinate fetching complete!\n")
cat("Improvements made:", improvements, "\n")

# Check remaining issues
final_distances <- data$distance_mi
cat("Places still > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\n")
cat("Places still > 4 miles:", sum(final_distances > 4, na.rm = TRUE), "\n")