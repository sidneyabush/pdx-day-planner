# SMART MANUAL COORDINATE FIXES
# These are places with coordinates outside Portland area or very distant
# Copy URLs into Google Maps, find @lat,lng in URL bar, and update coordinates below

manual_fixes <- list(
  # Scouters Mountain Nature Park (11.4 mi, very_distant)
  # Current coords: 45.44689, -122.50798
  # URL: https://www.google.com/maps/place/Scouters+Mountain+Nature+Park/data=!4m2!3m1!1s0x54959e57b67b87cb:0x56da20da65ec58d2
  Scouters_Mountain_Nature_Park = c(45.4468894, -122.5079771),  # UPDATE WITH REAL @lat,lng FROM URL

  # Springwater Corridor Trail (10.2 mi, very_distant)
  # Current coords: 45.47852, -122.50264
  # URL: https://www.google.com/maps/place/Springwater+Corridor+Trail/data=!4m2!3m1!1s0x549598c66f140a1d:0xedb5386099f308cf
  Springwater_Corridor_Trail = c(45.4785168, -122.5026403),  # UPDATE WITH REAL @lat,lng FROM URL

  # Powell Butte Nature Park (10 mi, very_distant)
  # Current coords: 45.4907, -122.49699
  # URL: https://www.google.com/maps/place/Powell+Butte+Nature+Park/data=!4m2!3m1!1s0x54959f449b3bdd11:0xa38faa35582a3ddf
  Powell_Butte_Nature_Park = c(45.4906961, -122.4969916),  # UPDATE WITH REAL @lat,lng FROM URL

  # Joy Cinema (9.9 mi, very_distant)
  # Current coords: 45.43389, -122.76583
  # URL: https://www.google.com/maps/place/Joy+Cinema/data=!4m2!3m1!1s0x54950ce3ffffffff:0xcba6d5ecbb04ddaa
  Joy_Cinema = c(45.4338889, -122.7658333),  # UPDATE WITH REAL @lat,lng FROM URL

  # Powell Butte Trailhead (9.3 mi, very_distant)
  # Current coords: 45.49241, -122.51133
  # URL: https://www.google.com/maps/place/Powell+Butte+Trailhead/data=!4m2!3m1!1s0x54959f8d73e33877:0x941748e5c14b749f
  Powell_Butte_Trailhead = c(45.4924057, -122.5113281),  # UPDATE WITH REAL @lat,lng FROM URL

)

# Apply the manual fixes
library(dplyr)
data <- readRDS("data/portland_places_processed.rds")

HOME_LAT <- 45.5623
HOME_LNG <- -122.6754

calc_distance_miles <- function(lat, lng, home_lat = HOME_LAT, home_lng = HOME_LNG) {
  R <- 3959
  lat1 <- home_lat * pi/180
  lat2 <- lat * pi/180
  dlat <- (lat - home_lat) * pi/180
  dlng <- (lng - home_lng) * pi/180
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlng/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  round(R * c, 1)
}

# Apply manual fixes
improvements <- 0
for(fix_name in names(manual_fixes)) {
  coords <- manual_fixes[[fix_name]]
  if(length(coords) == 2 && all(is.numeric(coords))) {
    # Find matching place (fuzzy match on title)
    title_pattern <- gsub("_", ".*", fix_name)
    matches <- grep(title_pattern, data$title, ignore.case = TRUE)
    
    if(length(matches) > 0) {
      idx <- matches[1]
      old_distance <- data$distance_mi[idx]
      new_distance <- calc_distance_miles(coords[1], coords[2])
      
      cat("UPDATING:", data$title[idx], "\\n")
      cat("  OLD:", round(data$lat[idx], 4), round(data$lng[idx], 4), "(", old_distance, "mi)")
      cat(" -> NEW:", round(coords[1], 4), round(coords[2], 4), "(", new_distance, "mi)\\n\\n")
      
      data$lat[idx] <- coords[1]
      data$lng[idx] <- coords[2]
      data$distance_mi[idx] <- new_distance
      improvements <- improvements + 1
    }
  }
}

if(improvements > 0) {
  saveRDS(data, "data/portland_places_processed.rds")
  cat("\\nData saved with", improvements, "manual coordinate fixes\\n")
} else {
  cat("\\nNo manual fixes applied\\n")
}
