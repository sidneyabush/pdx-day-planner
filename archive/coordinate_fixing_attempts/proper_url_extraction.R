# Use the proven coordinate extraction method from the archived workflow
library(dplyr)
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

# Portland distance check (from the working archive)
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

normalize_latlon <- function(lat, lon){
  lat <- suppressWarnings(as.numeric(lat)); lon <- suppressWarnings(as.numeric(lon))
  bad <- any(abs(lat) > 90, na.rm=TRUE) || any(abs(lon) > 180, na.rm=TRUE)
  if (bad){ tmp <- lat; lat <- lon; lon <- tmp }
  list(lat=lat, lon=lon)
}

# The PROPER coordinate extraction function from the working archive
coords_from_url_strict <- function(url, center = PDX_CENTER, max_km = 120, prefer = "place") {
  if (is.na(url) || !nzchar(url)) return(c(NA_real_, NA_real_))
  u <- as.character(url); cand <- NULL
  
  # TRUE PLACE COORDINATES (the main patterns)
  m <- stringr::str_match_all(u, "!3d(-?\\\\d+\\\\.?\\\\d*)!4d(-?\\\\d+\\\\.?\\\\d*)")[[1]]
  if (nrow(m)) cand <- rbind(cand, data.frame(lat = as.numeric(m[,2]), lon = as.numeric(m[,3]), src = "34", ord = seq_len(nrow(m))))
  m <- stringr::str_match_all(u, "!2d(-?\\\\d+\\\\.?\\\\d*)!3d(-?\\\\d+\\\\.?\\\\d*)")[[1]]
  if (nrow(m)) cand <- rbind(cand, data.frame(lat = as.numeric(m[,3]), lon = as.numeric(m[,2]), src = "23", ord = seq_len(nrow(m))))
  
  # VIEWPORT/QUERY FALLBACKS
  m <- stringr::str_match(u, "@(-?\\\\d+\\\\.?\\\\d*),(-?\\\\d+\\\\.?\\\\d*)")
  if (!is.na(m[1,2])) cand <- rbind(cand, data.frame(lat = as.numeric(m[1,2]), lon = as.numeric(m[1,3]), src = "@",  ord = 1))
  m <- stringr::str_match(u, "(?:[?&]q=)(-?\\\\d+\\\\.?\\\\d*),(-?\\\\d+\\\\.?\\\\d*)")
  if (!is.na(m[1,2])) cand <- rbind(cand, data.frame(lat = as.numeric(m[1,2]), lon = as.numeric(m[1,3]), src = "q",  ord = 1))
  m <- stringr::str_match(u, "(?:[?&](?:ll|sll)=)(-?\\\\d+\\\\.?\\\\d*),(-?\\\\d+\\\\.?\\\\d*)")
  if (!is.na(m[1,2])) cand <- rbind(cand, data.frame(lat = as.numeric(m[1,2]), lon = as.numeric(m[1,3]), src = "ll", ord = 1))
  
  if (is.null(cand) || !nrow(cand)) return(c(NA_real_, NA_real_))
  
  sw <- normalize_latlon(cand$lat, cand$lon)
  cand$lat <- sw$lat; cand$lon <- sw$lon
  ok <- is.finite(cand$lat) & is.finite(cand$lon) & abs(cand$lat) <= 90 & abs(cand$lon) <= 180
  cand <- cand[ok, , drop = FALSE]
  if (!nrow(cand)) return(c(NA_real_, NA_real_))
  
  # MUST BE PORTLAND-ISH (the key validation from the working system!)
  near <- near_center(cand$lat, cand$lon, center, max_km = max_km)
  cand <- cand[near, , drop = FALSE]
  if (!nrow(cand)) return(c(NA_real_, NA_real_))
  
  # PRIORITY BY PREFERENCE
  if (prefer == "place") prio <- match(cand$src, c("34","23","@","q","ll"), nomatch = 99)
  else                   prio <- match(cand$src, c("@","34","23","q","ll"), nomatch = 99)
  cand <- cand[order(prio, cand$ord), , drop = FALSE]
  c(cand$lat[1], cand$lon[1])
}

cat("Using the PROPER coordinate extraction from the working archive...\n")

# Test the function on a sample URL first
test_url <- data$url[1]
cat("Testing with URL:", test_url, "\n")
test_coords <- coords_from_url_strict(test_url)
cat("Result:", test_coords[1], test_coords[2], "\n")

# Now apply it to all URLs
improvements <- 0
url_coords_found <- 0

for(i in 1:nrow(data)) {
  if(i %% 50 == 0) cat("Processing", i, "of", nrow(data), "\n")
  
  if(!is.na(data$url[i]) && nzchar(data$url[i])) {
    coords <- coords_from_url_strict(data$url[i])
    
    if(!is.na(coords[1]) && !is.na(coords[2])) {
      url_coords_found <- url_coords_found + 1
      new_distance <- calc_distance_miles(coords[1], coords[2])
      old_distance <- data$distance_mi[i]
      
      # Only update if the URL coordinates are different from current ones
      if(abs(coords[1] - data$lat[i]) > 0.001 || abs(coords[2] - data$lng[i]) > 0.001) {
        cat("Found URL coordinates for", data$title[i], "\n")
        cat("  Old:", round(data$lat[i], 4), round(data$lng[i], 4), "(", old_distance, "mi )\n")
        cat("  New:", round(coords[1], 4), round(coords[2], 4), "(", new_distance, "mi )\n")
        
        data$lat[i] <- coords[1]
        data$lng[i] <- coords[2]
        data$distance_mi[i] <- new_distance
        improvements <- improvements + 1
      }
    }
  }
}

cat("\nProper URL coordinate extraction complete!\n")
cat("URLs with coordinates found:", url_coords_found, "\n")
cat("Coordinate improvements made:", improvements, "\n")

if(improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("Data saved with URL coordinate improvements\n")
}

# Final statistics
final_distances <- data$distance_mi
cat("Final distance range:", range(final_distances, na.rm = TRUE), "\n")
cat("Places > 6 miles:", sum(final_distances > 6, na.rm = TRUE), "\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\n")