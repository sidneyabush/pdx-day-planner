# Comprehensive geocoding for all problematic places
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

# Improved geocoder with better error handling
geocode_one_best <- function(query, center_hint = PDX_CENTER) {
  stopifnot(length(query) == 1)
  
  cat("  Geocoding:", substr(query, 1, 60), "...\\n")
  
  # Try adding "Portland Oregon" to help with disambiguation
  query_portland <- paste(query, "Portland Oregon")
  
  # ArcGIS first (with Portland context)
  tryCatch({
    g <- tidygeocoder::geocode(tibble(query = query_portland), address = "query",
                              method = "arcgis", limit = 1, 
                              min_time = 1.2, quiet = TRUE)
    if (nrow(g) > 0 && !is.na(g$lat) && !is.na(g$long)) {
      lat <- as.numeric(g$lat); lon <- as.numeric(g$long)
      if (near_center(lat, lon, center_hint, max_km = 120)) {
        cat("    ArcGIS+Portland success:", round(lat, 4), round(lon, 4), "\\n")
        return(c(lat, lon))
      }
    }
  }, error = function(e) {
    cat("    ArcGIS+Portland error\\n")
  })
  
  # Try original query with ArcGIS
  tryCatch({
    g <- tidygeocoder::geocode(tibble(query = query), address = "query",
                              method = "arcgis", limit = 1, 
                              min_time = 1.2, quiet = TRUE)
    if (nrow(g) > 0 && !is.na(g$lat) && !is.na(g$long)) {
      lat <- as.numeric(g$lat); lon <- as.numeric(g$long)
      if (near_center(lat, lon, center_hint, max_km = 120)) {
        cat("    ArcGIS success:", round(lat, 4), round(lon, 4), "\\n")
        return(c(lat, lon))
      }
    }
  }, error = function(e) {
    cat("    ArcGIS error\\n")
  })
  
  # OSM with Portland context (use return_input=FALSE to avoid limit error)
  tryCatch({
    g <- tidygeocoder::geocode(tibble(query = query_portland), address = "query",
                              method = "osm", limit = 3, return_input = FALSE,
                              min_time = 1.2, quiet = TRUE,
                              custom_query = list(countrycodes = "us"))
    if (nrow(g) > 0) {
      lat <- suppressWarnings(as.numeric(g$lat)); lon <- suppressWarnings(as.numeric(g$long))
      keep <- is.finite(lat) & is.finite(lon)
      if (any(keep)) {
        lat <- lat[keep]; lon <- lon[keep]
        d2 <- (lat - center_hint["lat"])^2 + (lon - center_hint["lon"])^2
        i <- which.min(d2)
        if (near_center(lat[i], lon[i], center_hint, max_km = 120)) {
          cat("    OSM+Portland success:", round(lat[i], 4), round(lon[i], 4), "\\n")
          return(c(lat[i], lon[i]))
        }
      }
    }
  }, error = function(e) {
    cat("    OSM+Portland error\\n")
  })
  
  cat("    No valid coordinates found\\n")
  return(c(NA_real_, NA_real_))
}

# Get all problematic places (> 5.0 miles)
problematic <- which(data$distance_mi > 5.0)
cat("\\nGeocoding", length(problematic), "places > 5.0 miles from home\\n\\n")

geocode_fixes <- 0

# Process ALL problematic places
for(i in 1:length(problematic)) {
  idx <- problematic[i]
  
  cat(paste0("[", i, "/", length(problematic), "] "))
  
  # Try geocoding the title
  result <- geocode_one_best(data$title[idx])
  
  if(!is.na(result[1]) && !is.na(result[2])) {
    old_distance <- data$distance_mi[idx]
    new_distance <- calc_distance_miles(result[1], result[2])
    
    # Update if we found coordinates and they're an improvement
    if(new_distance < old_distance) {
      cat("  FIXED:", data$title[idx], "\\n")
      cat("    ", round(data$lat[idx], 4), round(data$lng[idx], 4), "(", old_distance, "mi) ->")
      cat("    ", round(result[1], 4), round(result[2], 4), "(", new_distance, "mi)\\n\\n")
      
      data$lat[idx] <- result[1]
      data$lng[idx] <- result[2]
      data$distance_mi[idx] <- new_distance
      geocode_fixes <- geocode_fixes + 1
    } else {
      cat("  Found coords but not an improvement\\n\\n")
    }
  } else {
    cat("  No valid coordinates\\n\\n")
  }
  
  # Pause between requests to be respectful
  Sys.sleep(1.5)
}

if(geocode_fixes > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("\\nData saved with", geocode_fixes, "improvements\\n")
}

# Final statistics
final_distances <- data$distance_mi
cat("\\nComprehensive geocoding complete!\\n")
cat("Total fixes:", geocode_fixes, "\\n")
cat("Final distance range:", range(final_distances, na.rm = TRUE), "\\n")
cat("Places > 6 miles:", sum(final_distances > 6, na.rm = TRUE), "\\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\\n")
cat("Places > 4 miles:", sum(final_distances > 4, na.rm = TRUE), "\\n")