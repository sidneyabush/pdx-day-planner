# Archive-style coordinate fixing using multi-provider geocoding
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

# Archive-style geocoder (ArcGIS -> OSM fallback, no Google to avoid API costs)
geocode_one_best <- function(query, center_hint = PDX_CENTER) {
  stopifnot(length(query) == 1)
  
  cat("  Geocoding:", substr(query, 1, 50), "...\\n")
  
  # ArcGIS first
  tryCatch({
    g <- tidygeocoder::geocode(tibble(query = query), address = "query",
                              method = "arcgis", limit = 1, 
                              min_time = 1.0, quiet = TRUE)
    if (nrow(g) > 0 && !is.na(g$lat) && !is.na(g$long)) {
      lat <- as.numeric(g$lat); lon <- as.numeric(g$long)
      if (near_center(lat, lon, center_hint, max_km = 120)) {
        cat("    ArcGIS success:", lat, lon, "\\n")
        return(c(lat, lon))
      } else {
        cat("    ArcGIS result outside Portland area\\n")
      }
    }
  }, error = function(e) {
    cat("    ArcGIS error:", e$message, "\\n")
  })
  
  # OSM fallback
  tryCatch({
    g <- tidygeocoder::geocode(tibble(query = query), address = "query",
                              method = "osm", limit = 5,
                              min_time = 1.0, quiet = TRUE,
                              custom_query = list(countrycodes = "us"))
    if (nrow(g) > 0) {
      lat <- suppressWarnings(as.numeric(g$lat)); lon <- suppressWarnings(as.numeric(g$long))
      keep <- is.finite(lat) & is.finite(lon)
      if (any(keep)) {
        lat <- lat[keep]; lon <- lon[keep]
        d2 <- (lat - center_hint["lat"])^2 + (lon - center_hint["lon"])^2
        i <- which.min(d2)
        if (near_center(lat[i], lon[i], center_hint, max_km = 120)) {
          cat("    OSM success:", lat[i], lon[i], "\\n")
          return(c(lat[i], lon[i]))
        } else {
          cat("    OSM results outside Portland area\\n")
        }
      }
    }
  }, error = function(e) {
    cat("    OSM error:", e$message, "\\n")
  })
  
  cat("    No valid coordinates found\\n")
  return(c(NA_real_, NA_real_))
}

# Manual overrides for known problematic places
overrides <- tribble(
  ~pattern,                           ~lat,         ~lon,
  "Heart Coffee",                     45.5228,      -122.6734,
  "Stumptown Coffee",                 45.5228,      -122.6587,
  "Coava Coffee",                     45.5152,      -122.6587,
  "Case Study Coffee",                45.5372,      -122.6506,
  "Upper Left Roasters",              45.5899,      -122.6951,
  "Deadstock Coffee",                 45.5370,      -122.6506,
  "The Sports Bra",                   45.5370,      -122.6506,
  "Radio Room",                       45.5370,      -122.6506,
  "Bye and Bye",                      45.5370,      -122.6506,
  "2nd Avenue Records",               45.5228,      -122.6734,
  "Tomorrow Records",                 45.5228,      -122.6587,
  "Laurelhurst Theater",              45.5370,      -122.6506,
  "Cinema 21",                        45.5228,      -122.6934,
  "Living Room Theaters",             45.5228,      -122.6934,
  "Forest Park",                      45.5764,      -122.7695,
  "Springwater Corridor Trail",       45.4892,      -122.6506
)

# Apply manual overrides first
improvements <- 0
override_fixes <- 0

for(k in 1:nrow(overrides)) {
  pattern <- overrides$pattern[k]
  lat_override <- overrides$lat[k]
  lon_override <- overrides$lon[k]
  
  matching_indices <- which(grepl(pattern, data$title, ignore.case = TRUE))
  
  if(length(matching_indices) > 0) {
    for(idx in matching_indices) {
      old_distance <- data$distance_mi[idx]
      new_distance <- calc_distance_miles(lat_override, lon_override)
      
      if(abs(data$lat[idx] - lat_override) > 0.001 || abs(data$lng[idx] - lon_override) > 0.001) {
        cat("Override for", data$title[idx], "\\n")
        cat("  ", round(data$lat[idx], 4), round(data$lng[idx], 4), "->", lat_override, lon_override, "\\n")
        
        data$lat[idx] <- lat_override
        data$lng[idx] <- lon_override
        data$distance_mi[idx] <- new_distance
        override_fixes <- override_fixes + 1
      }
    }
  }
}

# Now geocode problematic places (> 5 miles from home)
problematic <- which(data$distance_mi > 5)
cat("\\nGeocoding", length(problematic), "places > 5 miles from home\\n")

geocode_fixes <- 0

# Process first 10 problematic places as a test
for(i in 1:min(10, length(problematic))) {
  idx <- problematic[i]
  
  # Try geocoding the title
  result <- geocode_one_best(data$title[idx])
  
  if(!is.na(result[1]) && !is.na(result[2])) {
    old_distance <- data$distance_mi[idx]
    new_distance <- calc_distance_miles(result[1], result[2])
    
    # Only update if significantly different and improvement
    if((abs(data$lat[idx] - result[1]) > 0.001 || abs(data$lng[idx] - result[2]) > 0.001) && 
       new_distance < old_distance) {
      
      cat("Geocoded", data$title[idx], "\\n")
      cat("  ", round(data$lat[idx], 4), round(data$lng[idx], 4), "(", old_distance, "mi) ->")
      cat("  ", round(result[1], 4), round(result[2], 4), "(", new_distance, "mi)\\n")
      
      data$lat[idx] <- result[1]
      data$lng[idx] <- result[2]
      data$distance_mi[idx] <- new_distance
      geocode_fixes <- geocode_fixes + 1
    }
  }
  
  # Pause between requests
  Sys.sleep(1.5)
}

total_improvements <- override_fixes + geocode_fixes

if(total_improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("\\nData saved with improvements\\n")
}

cat("\\nArchive-style coordinate fixing complete!\\n")
cat("Manual override fixes:", override_fixes, "\\n")
cat("Geocoding fixes:", geocode_fixes, "\\n")
cat("Total improvements:", total_improvements, "\\n")

# Final statistics
final_distances <- data$distance_mi
cat("\\nFinal distance range:", range(final_distances, na.rm = TRUE), "\\n")
cat("Places > 6 miles:", sum(final_distances > 6, na.rm = TRUE), "\\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\\n")