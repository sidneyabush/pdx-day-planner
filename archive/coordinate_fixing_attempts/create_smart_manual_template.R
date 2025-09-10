# Create smart manual template for only places with coordinates outside Portland area
library(dplyr)

# Wait for the coordinate fixing to complete, then generate template
wait_for_completion <- function() {
  while(TRUE) {
    # Check if the main coordinate fixing is still running
    if(!file.exists('fix_all_coordinates.R')) {
      break  # File moved, process likely complete
    }
    
    # Check for completion message in a log or just wait a bit
    Sys.sleep(5)
    cat("Waiting for coordinate fixing to complete...\n")
  }
}

# Load the most recent data
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

cat("Analyzing coordinate accuracy after URL extraction...\n\n")

# Identify places with coordinates outside reasonable Portland bounds
# Portland area roughly: lat 45.0-46.0, lng -123.5 to -122.0
outside_portland <- data %>%
  filter(!is.na(lat) & !is.na(lng)) %>%
  filter(lat < 45.0 | lat > 46.0 | lng < -123.5 | lng > -122.0) %>%
  arrange(desc(distance_mi))

cat("Places with coordinates outside Portland area bounds:\n")
cat("Count:", nrow(outside_portland), "\n\n")

if(nrow(outside_portland) > 0) {
  cat("Examples of out-of-bounds places:\n")
  for(i in 1:min(5, nrow(outside_portland))) {
    place <- outside_portland[i,]
    cat(i, ":", place$title, "at (", round(place$lat, 4), ",", round(place$lng, 4), ") -", place$distance_mi, "miles\n")
  }
  cat("\n")
}

# Also identify places that are still very far (>8 miles) even within bounds
distant_places <- data %>%
  filter(!is.na(distance_mi) & distance_mi > 8) %>%
  arrange(desc(distance_mi))

cat("Places > 8 miles from home (may need manual verification):\n")
cat("Count:", nrow(distant_places), "\n\n")

# Combine the two categories for manual template
manual_fix_candidates <- bind_rows(
  outside_portland %>% mutate(issue = "outside_portland_bounds"),
  distant_places %>% mutate(issue = "very_distant")
) %>%
  distinct(title, .keep_all = TRUE) %>%
  arrange(desc(distance_mi))

cat("Total places needing manual review:", nrow(manual_fix_candidates), "\n\n")

if(nrow(manual_fix_candidates) == 0) {
  cat("🎉 EXCELLENT! All coordinates appear to be accurate!\n")
  cat("No manual fixes needed - the URL extraction worked perfectly.\n")
  writeLines("# No manual fixes needed! All coordinates are accurate.", "no_manual_fixes_needed.txt")
} else {
  # Create the smart manual template
  template_content <- paste0(
    "# SMART MANUAL COORDINATE FIXES\n",
    "# These are places with coordinates outside Portland area or very distant\n",
    "# Copy URLs into Google Maps, find @lat,lng in URL bar, and update coordinates below\n\n",
    "manual_fixes <- list(\n"
  )
  
  for(i in 1:nrow(manual_fix_candidates)) {
    place <- manual_fix_candidates[i,]
    safe_title <- gsub("[^A-Za-z0-9_]", "_", place$title)
    safe_title <- gsub("_{2,}", "_", safe_title)
    safe_title <- gsub("^_|_$", "", safe_title)
    
    template_content <- paste0(template_content,
      "  # ", place$title, " (", place$distance_mi, " mi, ", place$issue, ")\n",
      "  # Current coords: ", round(place$lat, 5), ", ", round(place$lng, 5), "\n",
      "  # URL: ", place$url, "\n",
      "  ", safe_title, " = c(", place$lat, ", ", place$lng, "),  # UPDATE WITH REAL @lat,lng FROM URL\n\n"
    )
  }
  
  template_content <- paste0(template_content, ")\n\n")
  
  # Add the application code
  template_content <- paste0(template_content, 
    '# Apply the manual fixes\n',
    'library(dplyr)\n',
    'data <- readRDS("data/portland_places_processed.rds")\n\n',
    
    'HOME_LAT <- 45.5623\n',
    'HOME_LNG <- -122.6754\n\n',
    
    'calc_distance_miles <- function(lat, lng, home_lat = HOME_LAT, home_lng = HOME_LNG) {\n',
    '  R <- 3959\n',
    '  lat1 <- home_lat * pi/180\n',
    '  lat2 <- lat * pi/180\n',
    '  dlat <- (lat - home_lat) * pi/180\n',
    '  dlng <- (lng - home_lng) * pi/180\n',
    '  \n',
    '  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlng/2)^2\n',
    '  c <- 2 * atan2(sqrt(a), sqrt(1-a))\n',
    '  \n',
    '  round(R * c, 1)\n',
    '}\n\n',
    
    '# Apply manual fixes\n',
    'improvements <- 0\n',
    'for(fix_name in names(manual_fixes)) {\n',
    '  coords <- manual_fixes[[fix_name]]\n',
    '  if(length(coords) == 2 && all(is.numeric(coords))) {\n',
    '    # Find matching place (fuzzy match on title)\n',
    '    title_pattern <- gsub("_", ".*", fix_name)\n',
    '    matches <- grep(title_pattern, data$title, ignore.case = TRUE)\n',
    '    \n',
    '    if(length(matches) > 0) {\n',
    '      idx <- matches[1]\n',
    '      old_distance <- data$distance_mi[idx]\n',
    '      new_distance <- calc_distance_miles(coords[1], coords[2])\n',
    '      \n',
    '      cat("UPDATING:", data$title[idx], "\\\\n")\n',
    '      cat("  OLD:", round(data$lat[idx], 4), round(data$lng[idx], 4), "(", old_distance, "mi)")\n',
    '      cat(" -> NEW:", round(coords[1], 4), round(coords[2], 4), "(", new_distance, "mi)\\\\n\\\\n")\n',
    '      \n',
    '      data$lat[idx] <- coords[1]\n',
    '      data$lng[idx] <- coords[2]\n',
    '      data$distance_mi[idx] <- new_distance\n',
    '      improvements <- improvements + 1\n',
    '    }\n',
    '  }\n',
    '}\n\n',
    
    'if(improvements > 0) {\n',
    '  saveRDS(data, "data/portland_places_processed.rds")\n',
    '  cat("\\\\nData saved with", improvements, "manual coordinate fixes\\\\n")\n',
    '} else {\n',
    '  cat("\\\\nNo manual fixes applied\\\\n")\n',
    '}'
  )
  
  # Write the smart template
  writeLines(template_content, "smart_manual_fixes.R")
  
  cat("Created smart manual template: smart_manual_fixes.R\n")
  cat("This includes only", nrow(manual_fix_candidates), "places that need manual review:\n")
  cat("- Places outside Portland area bounds:", sum(manual_fix_candidates$issue == "outside_portland_bounds"), "\n")
  cat("- Places > 8 miles from home:", sum(manual_fix_candidates$issue == "very_distant"), "\n")
}

# Final summary
final_distances <- data$distance_mi
cat("\nFINAL COORDINATE ANALYSIS:\n")
cat("Total places:", nrow(data), "\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\n")
cat("Places > 4 miles:", sum(final_distances > 4, na.rm = TRUE), "\n")
cat("Places > 3 miles:", sum(final_distances > 3, na.rm = TRUE), "\n")
cat("Distance range:", range(final_distances, na.rm = TRUE), "\n")