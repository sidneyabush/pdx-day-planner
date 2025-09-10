# Create template for manual coordinate fixes
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

# Identify problematic places that need manual fixes
# Priority: distant places (>4 miles) and clustered coordinates

cat("Analyzing coordinate accuracy issues...\n\n")

# Find distant places (likely wrong)
distant_places <- data %>%
  filter(distance_mi > 4) %>%
  arrange(desc(distance_mi))

cat("Places > 4 miles from home (likely wrong coordinates):\n")
cat("Count:", nrow(distant_places), "\n\n")

# Find clustered coordinates (rounded to 3 decimal places)
lat_counts <- table(round(data$lat, 3))
lng_counts <- table(round(data$lng, 3))

clustered_lats <- names(lat_counts)[lat_counts > 3]
clustered_lngs <- names(lng_counts)[lng_counts > 3]

clustered_places <- data %>%
  filter(round(lat, 3) %in% as.numeric(clustered_lats) | 
         round(lng, 3) %in% as.numeric(clustered_lngs)) %>%
  arrange(lat, lng)

cat("Places with clustered coordinates (>3 at same location):\n")
cat("Count:", nrow(clustered_places), "\n\n")

# Combine and prioritize fixes needed
priority_fixes <- bind_rows(
  distant_places %>% mutate(issue = "distant"),
  clustered_places %>% mutate(issue = "clustered")
) %>%
  distinct(title, .keep_all = TRUE) %>%
  arrange(desc(distance_mi))

cat("Total unique places needing manual fixes:", nrow(priority_fixes), "\n\n")

# Create manual fix template
template_content <- paste0(
  "# MANUAL COORDINATE FIXES TEMPLATE\n",
  "# Copy the URLs below into Google Maps, get the @lat,lng coordinates, and paste them here\n",
  "# Format: place_title = c(lat, lng)\n\n",
  "manual_fixes <- list(\n"
)

# Add top 50 priority places for manual fixing
top_fixes <- head(priority_fixes, 50)

for(i in 1:nrow(top_fixes)) {
  place <- top_fixes[i,]
  safe_title <- gsub("[^A-Za-z0-9_]", "_", place$title)
  safe_title <- gsub("_{2,}", "_", safe_title)  # Remove multiple underscores
  safe_title <- gsub("^_|_$", "", safe_title)   # Remove leading/trailing underscores
  
  template_content <- paste0(template_content,
    "  # ", place$title, " (", place$distance_mi, " mi, ", place$issue, ")\n",
    "  # URL: ", place$url, "\n",
    "  ", safe_title, " = c(", place$lat, ", ", place$lng, "),  # REPLACE WITH REAL COORDS\n\n"
  )
}

template_content <- paste0(template_content, ")\n\n")

# Add the fix application code
template_content <- paste0(template_content, 
'# APPLY FIXES CODE
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
      idx <- matches[1]  # Take first match
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
}
')

# Write template file
writeLines(template_content, "manual_coordinate_fixes_template.R")

cat("Created manual fix template: manual_coordinate_fixes_template.R\n")
cat("This file contains the top", nrow(top_fixes), "places that need coordinate fixes.\n\n")

cat("Instructions:\n")
cat("1. Open manual_coordinate_fixes_template.R\n")
cat("2. For each place, copy the URL into Google Maps\n")
cat("3. Look for @lat,lng in the URL bar after the page loads\n")
cat("4. Replace the coordinates in the template\n")
cat("5. Run the template script to apply all fixes\n\n")

# Show summary of issues
cat("SUMMARY OF COORDINATE ISSUES:\n")
cat("Total places:", nrow(data), "\n")
cat("Places > 4 miles (likely wrong):", nrow(distant_places), "\n")
cat("Places with clustered coordinates:", nrow(clustered_places), "\n")
cat("Unique places needing fixes:", nrow(priority_fixes), "\n")
cat("Template covers top", nrow(top_fixes), "priority places\n")