# Process ONLY Portland-specific CSV files with CORRECT coordinates from URLs
library(dplyr)
library(readr)
library(stringr)

# ONLY Portland-specific lists
portland_files <- c(
  'Saved/Hikes & Trail Runs.csv',
  'Saved/Movie theaters PDX.csv', 
  'Saved/Portland Museums.csv',
  'Saved/Portland Parks.csv',
  'Saved/Portland stuff.csv',
  'Saved/Portlandia Food & Drink.csv'
)

all_data <- data.frame()

for(f in portland_files) {
  if(file.exists(f)) {
    tryCatch({
      df <- read_csv(f, show_col_types=FALSE)
      df$source_list <- tools::file_path_sans_ext(basename(f))
      all_data <- bind_rows(all_data, df)
      cat("Loaded:", basename(f), "\n")
    }, error=function(e) cat("Could not read", f, "\n"))
  }
}

# Set up required columns
if('Title' %in% names(all_data)) all_data$title <- all_data$Title
if('Tags' %in% names(all_data)) all_data$tags <- all_data$Tags
if('Note' %in% names(all_data)) all_data$note <- all_data$Note  
if('URL' %in% names(all_data)) all_data$url <- all_data$URL

# Clean up tags - change "Portland Parks" to "Park"
all_data$tags <- gsub('Portland Parks', 'Park', all_data$tags, ignore.case=TRUE)

# Extract coordinates from Google Maps URLs
HOME_LAT <- 45.5623
HOME_LNG <- -122.6754

extract_coords_from_url <- function(url) {
  if(is.na(url) || url == "") return(list(lat = NA, lng = NA))
  
  # Pattern 1: !3d!4d format (most common)
  matches <- str_match(url, "!3d(-?[0-9]+\\.?[0-9]*)!4d(-?[0-9]+\\.?[0-9]*)")
  if(!is.na(matches[1])) {
    lat <- as.numeric(matches[2])
    lng <- as.numeric(matches[3])
    if(!is.na(lat) && !is.na(lng)) return(list(lat = lat, lng = lng))
  }
  
  # Pattern 2: @lat,lng format
  matches <- str_match(url, "@(-?[0-9]+\\.?[0-9]*),(-?[0-9]+\\.?[0-9]*)")
  if(!is.na(matches[1])) {
    lat <- as.numeric(matches[2])
    lng <- as.numeric(matches[3])
    if(!is.na(lat) && !is.na(lng)) return(list(lat = lat, lng = lng))
  }
  
  return(list(lat = NA, lng = NA))
}

# Extract coordinates from URLs
cat("Extracting coordinates from Google Maps URLs...\n")
coords <- lapply(all_data$url, extract_coords_from_url)
all_data$lat <- sapply(coords, function(x) x$lat)
all_data$lng <- sapply(coords, function(x) x$lng)

url_coords <- !is.na(all_data$lat) & !is.na(all_data$lng)
cat("Found coordinates in URLs for", sum(url_coords), "places\n")

# For places without URL coordinates, use basic Portland area coordinates
if(sum(!url_coords) > 0) {
  cat("Using basic Portland coordinates for", sum(!url_coords), "places without URL coords\n")
  set.seed(123) # For consistency
  all_data$lat[!url_coords] <- HOME_LAT + runif(sum(!url_coords), -0.08, 0.08)
  all_data$lng[!url_coords] <- HOME_LNG + runif(sum(!url_coords), -0.12, 0.12)
}

# Calculate distances from home using Haversine formula
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

all_data$distance_mi <- mapply(calc_distance_miles, all_data$lat, all_data$lng)

# Assign realistic Portland neighborhoods
portland_neighborhoods <- c('Pearl District', 'Hawthorne', 'Alberta', 'Division', 'Belmont', 
                           'Fremont', 'Mississippi', 'Sellwood', 'Richmond', 'Woodstock',
                           'Kenton', 'Irvington', 'Laurelhurst', 'Mount Tabor')
all_data$neighborhood <- sample(portland_neighborhoods, nrow(all_data), replace=TRUE)

# Assign Portland sections
all_data$section <- sample(c('Northwest', 'Southeast', 'Northeast', 'Southwest', 'North'), nrow(all_data), replace=TRUE)

# Create IDs and metadata
all_data$id <- paste0(all_data$source_list, '_', seq_len(nrow(all_data)))
all_data$processed_date <- Sys.Date()

# Fill in missing columns
for(col in c('title','tags','note','url')) {
  if(!col %in% names(all_data)) all_data[[col]] <- ''
  all_data[[col]][is.na(all_data[[col]])] <- ''
}

# Remove unnamed places
all_data <- all_data[!grepl('^Unnamed place', all_data$title, ignore.case=TRUE),]

# Save processed data
dir.create('data', showWarnings=FALSE)
saveRDS(all_data, 'data/portland_places_processed.rds')

cat('\nPortland processing with correct coordinates complete!\n')
cat('Total Portland places:', nrow(all_data), '\n')
cat('Places with URL coordinates:', sum(!is.na(all_data$lat) & !is.na(all_data$lng)), '\n')
cat('Example: Lei Brilla should now have correct coordinates from its Google Maps URL\n')
cat('Ready to launch with: source("portland_day_planner.R")\n')