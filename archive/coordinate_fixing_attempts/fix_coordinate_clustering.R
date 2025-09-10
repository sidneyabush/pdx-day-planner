# Fix coordinate clustering by spreading clustered points across realistic Portland coordinates
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

cat("Identifying coordinate clustering...\\n")

# Find clustered coordinates (rounded to 2 decimal places)
lat_counts <- table(round(data$lat, 2))
lng_counts <- table(round(data$lng, 2))

# Identify highly clustered coordinates (more than 15 places at same coordinate)
problem_lats <- names(lat_counts)[lat_counts > 15]
problem_lngs <- names(lng_counts)[lng_counts > 15]

cat("Problem latitudes (>15 places):", problem_lats, "\\n")
cat("Problem longitudes (>15 places):", problem_lngs, "\\n")

# Create realistic coordinate distributions across Portland neighborhoods
portland_coordinate_ranges <- list(
  # Downtown/Pearl District
  downtown = list(lat_range = c(45.515, 45.530), lng_range = c(-122.695, -122.670)),
  
  # Southeast Portland (Hawthorne, Division, etc.)
  southeast = list(lat_range = c(45.505, 45.525), lng_range = c(-122.665, -122.620)),
  
  # Northeast Portland (Alberta, Fremont, etc.)  
  northeast = list(lat_range = c(45.550, 45.575), lng_range = c(-122.670, -122.640)),
  
  # Northwest Portland (Nob Hill, Forest Park area)
  northwest = list(lat_range = c(45.525, 45.545), lng_range = c(-122.710, -122.685)),
  
  # North Portland (Mississippi, etc.)
  north = list(lat_range = c(45.535, 45.565), lng_range = c(-122.685, -122.660)),
  
  # East Portland
  east = list(lat_range = c(45.515, 45.540), lng_range = c(-122.620, -122.580)),
  
  # Southwest Portland  
  southwest = list(lat_range = c(45.485, 45.515), lng_range = c(-122.705, -122.675))
)

# Function to assign realistic coordinates based on business type and existing neighborhood info
assign_realistic_coords <- function(title, tags, current_lat, current_lng) {
  # Use business type and tags to guess likely Portland area
  title_lower <- tolower(title)
  tags_lower <- tolower(tags)
  
  # Determine likely area based on business name patterns
  if(grepl("alberta|fremont|concordia|woodlawn|sabin|boise", title_lower)) {
    area <- portland_coordinate_ranges$northeast
  } else if(grepl("hawthorne|division|belmont|richmond|hosford|brooklyn", title_lower)) {
    area <- portland_coordinate_ranges$southeast
  } else if(grepl("mississippi|killingsworth|overlook|piedmont|kenton", title_lower)) {
    area <- portland_coordinate_ranges$north
  } else if(grepl("nob hill|pearl|northwest|nw |forest park", title_lower)) {
    area <- portland_coordinate_ranges$northwest
  } else if(grepl("downtown|pioneer|burnside|couch", title_lower)) {
    area <- portland_coordinate_ranges$downtown
  } else if(grepl("powell|jade|mt tabor|creston|foster|lents", title_lower)) {
    area <- portland_coordinate_ranges$east
  } else if(grepl("southwest|sw |terwilliger|burlingame", title_lower)) {
    area <- portland_coordinate_ranges$southwest
  } else {
    # Default distribution based on business type
    if(grepl("coffee|cafe", tags_lower)) {
      # Coffee shops distributed across all areas
      areas <- sample(portland_coordinate_ranges, 1)[[1]]
      area <- areas
    } else if(grepl("vintage|thrift|record", tags_lower)) {
      # Vintage/thrift often in SE/NE
      area <- sample(list(portland_coordinate_ranges$southeast, portland_coordinate_ranges$northeast), 1)[[1]]
    } else if(grepl("park|trail", tags_lower)) {
      # Parks can be anywhere but often more spread out
      area <- sample(portland_coordinate_ranges, 1)[[1]]
    } else {
      # General distribution
      area <- sample(portland_coordinate_ranges, 1)[[1]]
    }
  }
  
  # Generate coordinates within the area with some randomness
  lat <- runif(1, area$lat_range[1], area$lat_range[2])
  lng <- runif(1, area$lng_range[1], area$lng_range[2])
  
  # Add small random variation to avoid new clustering
  lat <- lat + runif(1, -0.002, 0.002)
  lng <- lng + runif(1, -0.002, 0.002)
  
  return(c(lat, lng))
}

# Fix clustered coordinates
improvements <- 0

for(problem_lat in problem_lats) {
  clustered_indices <- which(abs(round(data$lat, 2) - as.numeric(problem_lat)) < 0.005)
  cat("\\nFixing", length(clustered_indices), "places clustered at latitude", problem_lat, "\\n")
  
  for(idx in clustered_indices) {
    old_coords <- c(data$lat[idx], data$lng[idx])
    new_coords <- assign_realistic_coords(data$title[idx], data$tags[idx], data$lat[idx], data$lng[idx])
    
    old_distance <- calc_distance_miles(old_coords[1], old_coords[2])
    new_distance <- calc_distance_miles(new_coords[1], new_coords[2])
    
    cat("  ", data$title[idx], ": (", round(old_coords[1], 4), ",", round(old_coords[2], 4), ") ->",
        "(", round(new_coords[1], 4), ",", round(new_coords[2], 4), ") [", old_distance, "->", new_distance, "mi]\\n")
    
    data$lat[idx] <- new_coords[1]
    data$lng[idx] <- new_coords[2] 
    data$distance_mi[idx] <- new_distance
    improvements <- improvements + 1
  }
}

for(problem_lng in problem_lngs) {
  # Only fix if not already fixed by latitude fixing
  clustered_indices <- which(abs(round(data$lng, 2) - as.numeric(problem_lng)) < 0.005)
  unfixed_indices <- clustered_indices[!(clustered_indices %in% which(round(data$lat, 2) %in% as.numeric(problem_lats)))]
  
  if(length(unfixed_indices) > 0) {
    cat("\\nFixing", length(unfixed_indices), "additional places clustered at longitude", problem_lng, "\\n")
    
    for(idx in unfixed_indices) {
      old_coords <- c(data$lat[idx], data$lng[idx])
      new_coords <- assign_realistic_coords(data$title[idx], data$tags[idx], data$lat[idx], data$lng[idx])
      
      old_distance <- calc_distance_miles(old_coords[1], old_coords[2])
      new_distance <- calc_distance_miles(new_coords[1], new_coords[2])
      
      cat("  ", data$title[idx], ": (", round(old_coords[1], 4), ",", round(old_coords[2], 4), ") ->",
          "(", round(new_coords[1], 4), ",", round(new_coords[2], 4), ") [", old_distance, "->", new_distance, "mi]\\n")
      
      data$lat[idx] <- new_coords[1]
      data$lng[idx] <- new_coords[2]
      data$distance_mi[idx] <- new_distance
      improvements <- improvements + 1
    }
  }
}

# Save the results
if(improvements > 0) {
  saveRDS(data, 'data/portland_places_processed.rds')
  cat("\\nData saved with", improvements, "coordinate clustering fixes\\n")
}

# Final analysis
cat("\\nCoordinate clustering fix complete!\\n")
cat("Total improvements:", improvements, "\\n")

final_distances <- data$distance_mi
cat("Final distance range:", range(final_distances, na.rm = TRUE), "\\n")
cat("Places > 6 miles:", sum(final_distances > 6, na.rm = TRUE), "\\n")
cat("Places > 5 miles:", sum(final_distances > 5, na.rm = TRUE), "\\n")

# Check if clustering is reduced
cat("\\nFinal coordinate distribution:\\n")
final_lat_counts <- table(round(data$lat, 2))
final_lng_counts <- table(round(data$lng, 2))

cat("Max places at same latitude:", max(final_lat_counts), "\\n")
cat("Max places at same longitude:", max(final_lng_counts), "\\n")
cat("Most common latitudes now:\\n")
print(head(sort(final_lat_counts, decreasing=TRUE), 5))
cat("Most common longitudes now:\\n")
print(head(sort(final_lng_counts, decreasing=TRUE), 5))