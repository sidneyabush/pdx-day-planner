# Fix coordinates for ALL places, regardless of distance
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
    response <- GET(url, timeout(15))
    final_url <- response$url
    
    # Extract coordinates from the final URL
    coords <- extract_coords_from_url(final_url)
    if(!is.na(coords[1])) {
      cat("  Found in URL:", coords[1], coords[2], "\n")
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

cat("Fixing coordinates for ALL", nrow(data), "places...\n\n")

# Filter to places that have URLs
places_with_urls <- data[!is.na(data$url) & data$url != "", ]
cat("Places with URLs:", nrow(places_with_urls), "\n\n")

improvements <- 0
processed <- 0

# Process every place with a URL
for(i in 1:nrow(places_with_urls)) {
  processed <- processed + 1
  place_title <- places_with_urls$title[i]
  
  # Find the index in the original data
  place_idx <- which(data$title == place_title)[1]
  
  cat(paste0("[", processed, "/", nrow(places_with_urls), "] "))
  
  coords <- get_real_coordinates(places_with_urls$url[i], place_title)
  
  if(!is.na(coords[1]) && !is.na(coords[2])) {
    old_distance <- data$distance_mi[place_idx]
    new_distance <- calc_distance_miles(coords[1], coords[2])
    
    # Update if coordinates are different (even if distance is similar)
    if(abs(data$lat[place_idx] - coords[1]) > 0.0001 || 
       abs(data$lng[place_idx] - coords[2]) > 0.0001) {
      
      cat("UPDATED:", place_title, "\n")
      cat("  OLD:", round(data$lat[place_idx], 5), round(data$lng[place_idx], 5), "(", old_distance, "mi)")
      cat(" -> NEW:", round(coords[1], 5), round(coords[2], 5), "(", new_distance, "mi)\n\n")
      
      data$lat[place_idx] <- coords[1]
      data$lng[place_idx] <- coords[2]
      data$distance_mi[place_idx] <- new_distance
      improvements <- improvements + 1
    } else {
      cat("Already accurate:", place_title, "\n\n")
    }
  }
  
  # Save periodically
  if(processed %% 25 == 0) {
    cat("--- Saving progress after", processed, "places ---\n")
    saveRDS(data, 'data/portland_places_processed.rds')
  }
  
  # Be respectful to Google's servers
  Sys.sleep(1)
}

# Final save
saveRDS(data, 'data/portland_places_processed.rds')

cat("\n=== COORDINATE FIX COMPLETE ===\n")
cat("Places processed:", processed, "\n")
cat("Coordinates improved:", improvements, "\n\n")

# Final statistics
final_distances <- data$distance_mi
cat("FINAL COORDINATE ACCURACY:\n")
cat("Distance range:", range(final_distances, na.rm = TRUE), "\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\n")
cat("Places > 4 miles:", sum(final_distances > 4, na.rm = TRUE), "\n")
cat("Places > 3 miles:", sum(final_distances > 3, na.rm = TRUE), "\n")
cat("Places > 2 miles:", sum(final_distances > 2, na.rm = TRUE), "\n")

# Check coordinate distribution
cat("\nCoordinate distribution check:\n")
lat_rounded <- round(data$lat, 3)
lng_rounded <- round(data$lng, 3)
lat_counts <- table(lat_rounded)
lng_counts <- table(lng_rounded)

cat("Max places at same latitude (3 decimals):", max(lat_counts), "\n")
cat("Max places at same longitude (3 decimals):", max(lng_counts), "\n")

# Show most common coordinates to check for clustering
cat("Most clustered latitudes:\n")
print(head(sort(lat_counts, decreasing=TRUE), 5))
cat("Most clustered longitudes:\n")
print(head(sort(lng_counts, decreasing=TRUE), 5))