#!/usr/bin/env Rscript

# Simple Dataset Completion Script (no geocoding)
# Fill in missing columns without API calls

library(dplyr)
library(readr)

# Read the combined dataset
data <- read_csv("data/portland_places_combined.csv", show_col_types = FALSE)

cat("Starting simple dataset completion...\n")
cat("Total rows:", nrow(data), "\n")

# Function to get neighborhood from coordinates (vectorized)
get_neighborhood_from_coords <- function(lat, lng) {
  ifelse(is.na(lat) | is.na(lng), NA_character_,
    ifelse(lat > 45.5152,
      ifelse(lng > -122.6784, "Northeast Portland", "Northwest Portland"),
      ifelse(lng > -122.6784, "Southeast Portland", "Southwest Portland")
    )
  )
}

# Function to assign section based on neighborhood (vectorized)
get_section_from_neighborhood <- function(neighborhood) {
  ifelse(is.na(neighborhood), NA_character_,
    ifelse(grepl("Northeast|NE", neighborhood, ignore.case = TRUE), "Northeast",
      ifelse(grepl("Northwest|NW", neighborhood, ignore.case = TRUE), "Northwest",
        ifelse(grepl("Southeast|SE", neighborhood, ignore.case = TRUE), "Southeast",
          ifelse(grepl("Southwest|SW", neighborhood, ignore.case = TRUE), "Southwest", "Central")
        )
      )
    )
  )
}

# Function to calculate distance from home address (vectorized)
calculate_distance <- function(lat1, lng1, lat2 = 45.558, lng2 = -122.665) {
  ifelse(is.na(lat1) | is.na(lng1), NA_real_, {
    R <- 3959  # Earth's radius in miles
    lat1_rad <- lat1 * pi / 180
    lat2_rad <- lat2 * pi / 180
    dlat <- (lat2 - lat1) * pi / 180
    dlng <- (lng2 - lng1) * pi / 180
    a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlng/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    round(R * c, 1)
  })
}

# Function to determine seasonal availability based on activity type
get_seasonal_availability <- function(title, activity_type, source_list) {
  title_lower <- tolower(paste(title, activity_type))

  # Outdoor activities are seasonal in Portland (Spring-Fall)
  if (grepl("hike|trail|park|outdoor|garden|zoo|waterfall|beach|river|lake|forest", title_lower) ||
      activity_type == "Nature/Outdoor" ||
      grepl("Hikes|Parks", source_list)) {
    return("Seasonal (Spring-Fall)")
  }

  # Winter-specific activities
  if (grepl("ski|snow|winter|christmas|holiday", title_lower)) {
    return("Seasonal (Winter)")
  }

  # Summer-specific activities
  if (grepl("swim|beach|float|water park|outdoor concert|festival", title_lower)) {
    return("Seasonal (Summer)")
  }

  # Default to year-round for indoor activities
  return("Year-round")
}

# Function to determine time of day suitability
get_time_suitability <- function(title, when_available = "", activity_type = "") {
  title_lower <- tolower(paste(title, when_available, activity_type))

  # Morning activities
  if (grepl("breakfast|brunch|farmer|market|hike|trail|morning", title_lower)) {
    return("Morning,Afternoon")
  }

  # Evening/night activities
  if (grepl("bar|brewery|dinner|theater|movie|nightlife|evening|night", title_lower)) {
    return("Afternoon,Evening")
  }

  # All-day activities
  if (grepl("museum|park|zoo|shop|gallery", title_lower)) {
    return("Morning,Afternoon,Evening")
  }

  # Default to afternoon/evening
  return("Afternoon,Evening")
}

# Function to generate tags from activity type and title (vectorized)
generate_tags <- function(activity_type, title, note = "") {
  # Handle NAs
  activity_type <- ifelse(is.na(activity_type), "", activity_type)
  title <- ifelse(is.na(title), "", title)
  note <- ifelse(is.na(note), "", note)

  # Create base tags from activity type
  base_tags <- gsub("[/]", " ", tolower(activity_type))

  # Extract additional tags from title and notes
  text <- tolower(paste(title, note))

  additional_tags <- sapply(text, function(txt) {
    tags <- c()
    if (grepl("outdoor|hike|trail|park", txt)) tags <- c(tags, "outdoor")
    if (grepl("indoor|museum|theater", txt)) tags <- c(tags, "indoor")
    if (grepl("free|no cost", txt)) tags <- c(tags, "free")
    if (grepl("family|kid|child", txt)) tags <- c(tags, "family-friendly")
    if (grepl("food|restaurant|eat|drink", txt)) tags <- c(tags, "food")
    if (grepl("art|gallery|exhibit", txt)) tags <- c(tags, "arts")
    if (grepl("music|concert|show", txt)) tags <- c(tags, "music")
    if (grepl("sport|active|exercise", txt)) tags <- c(tags, "active")
    if (grepl("shop|store|market", txt)) tags <- c(tags, "shopping")
    if (grepl("historic|history|heritage", txt)) tags <- c(tags, "historic")
    return(paste(tags, collapse = ", "))
  })

  # Combine base and additional tags
  final_tags <- ifelse(base_tags != "" & additional_tags != "",
                      paste(base_tags, additional_tags, sep = ", "),
                      ifelse(base_tags != "", base_tags, additional_tags))

  return(final_tags)
}

cat("Processing data...\n")

# Fill in missing data
data <- data %>%
  mutate(
    # Fill neighborhoods based on existing coordinates
    neighborhood = case_when(
      !is.na(neighborhood) & neighborhood != "" ~ neighborhood,
      !is.na(lat) & !is.na(lng) ~ get_neighborhood_from_coords(lat, lng),
      TRUE ~ "Portland Area"
    ),

    # Fill sections based on neighborhoods
    section = case_when(
      !is.na(section) & section != "" ~ section,
      !is.na(neighborhood) ~ get_section_from_neighborhood(neighborhood),
      TRUE ~ "Central"
    ),

    # Calculate distances for existing coordinates
    distance_mi = case_when(
      !is.na(distance_mi) ~ distance_mi,
      !is.na(lat) & !is.na(lng) ~ calculate_distance(lat, lng),
      TRUE ~ 5.0
    ),

    # Update seasonal categories based on activity type and source
    seasonal_category = mapply(get_seasonal_availability, title, activity_type, source_list),

    # Add time of day suitability
    time_suitability = mapply(get_time_suitability, title, when_available, activity_type),

    # Generate tags for empty tags
    tags = case_when(
      !is.na(tags) & tags != "" ~ tags,
      TRUE ~ generate_tags(activity_type, title, note)
    ),

    # Ensure all character fields are not NA
    title = ifelse(is.na(title), "Unknown Place", title),
    note = ifelse(is.na(note), "", note),
    url = ifelse(is.na(url), "", url),
    when_available = ifelse(is.na(when_available), "Every day", when_available),
    frequency = ifelse(is.na(frequency), "Regular", frequency),
    activity_type = ifelse(is.na(activity_type), "Other", activity_type),
    location_text = ifelse(is.na(location_text), title, location_text),

    # Set default coordinates for Reddit activities without them (Portland center)
    lat = case_when(
      !is.na(lat) ~ lat,
      source_list == "Reddit Portland Activities" ~ 45.5152,
      TRUE ~ lat
    ),
    lng = case_when(
      !is.na(lng) ~ lng,
      source_list == "Reddit Portland Activities" ~ -122.6784,
      TRUE ~ lng
    ),

    # Ensure processed_date is set
    processed_date = ifelse(is.na(processed_date), as.character(Sys.Date()), as.character(processed_date))
  )

# Handle special location mappings for known Portland places
known_locations <- list(
  "Powell's Books" = list(lat = 45.5230, lng = -122.6814, neighborhood = "Pearl District", section = "Northwest"),
  "Pioneer Courthouse Square" = list(lat = 45.5190, lng = -122.6787, neighborhood = "Downtown", section = "Southwest"),
  "Oregon Zoo" = list(lat = 45.5099, lng = -122.7160, neighborhood = "West Hills", section = "Southwest"),
  "OMSI" = list(lat = 45.5085, lng = -122.6650, neighborhood = "Central Eastside", section = "Southeast"),
  "Pittock Mansion" = list(lat = 45.5253, lng = -122.7161, neighborhood = "Forest Park", section = "Northwest"),
  "International Rose Test Garden" = list(lat = 45.5189, lng = -122.7064, neighborhood = "Washington Park", section = "Southwest"),
  "Multnomah Falls" = list(lat = 45.5762, lng = -122.1158, neighborhood = "Columbia River Gorge", section = "Outside Portland")
)

# Apply known location data
for (place_name in names(known_locations)) {
  matches <- which(grepl(place_name, data$title, ignore.case = TRUE))
  if (length(matches) > 0) {
    loc_data <- known_locations[[place_name]]
    for (idx in matches) {
      # Only update if coordinates are default Portland center
      if (abs(data$lat[idx] - 45.5152) < 0.001 && abs(data$lng[idx] - (-122.6784)) < 0.001) {
        data$lat[idx] <- loc_data$lat
        data$lng[idx] <- loc_data$lng
        data$neighborhood[idx] <- loc_data$neighborhood
        data$section[idx] <- loc_data$section
        data$distance_mi[idx] <- calculate_distance(loc_data$lat, loc_data$lng)
      }
    }
  }
}

# Final data validation
data <- data %>%
  mutate(
    # Ensure distance is calculated for all rows with coordinates
    distance_mi = ifelse(is.na(distance_mi) & !is.na(lat) & !is.na(lng),
                        calculate_distance(lat, lng), distance_mi),

    # Clean up any remaining NAs
    distance_mi = ifelse(is.na(distance_mi), 5.0, distance_mi)
  )

# Save the completed dataset
write_csv(data, "data/portland_places_complete.csv")

# Generate completion report
cat("\nDataset completion summary:\n")
cat("Total rows:", nrow(data), "\n")
cat("Rows with coordinates:", sum(!is.na(data$lat) & !is.na(data$lng)), "\n")
cat("Rows with neighborhoods:", sum(!is.na(data$neighborhood) & data$neighborhood != ""), "\n")
cat("Rows with non-empty tags:", sum(!is.na(data$tags) & data$tags != ""), "\n")
cat("Rows with distances:", sum(!is.na(data$distance_mi)), "\n")

# Check seasonal distribution
cat("\nSeasonal category distribution:\n")
seasonal_counts <- table(data$seasonal_category)
for (season in names(seasonal_counts)) {
  cat(paste0(season, ": ", seasonal_counts[season]), "\n")
}

# Activity type breakdown
cat("\nActivity type distribution:\n")
activity_counts <- table(data$activity_type)
for (activity in names(activity_counts)) {
  cat(paste0(activity, ": ", activity_counts[activity]), "\n")
}

# Time suitability breakdown
cat("\nTime suitability breakdown (sample):\n")
time_sample <- head(table(data$time_suitability), 5)
for (time in names(time_sample)) {
  cat(paste0(time, ": ", time_sample[time]), "\n")
}

cat("\nCompleted dataset saved as: data/portland_places_complete.csv\n")
cat("Key improvements:\n")
cat("- Outdoor activities marked as seasonal (Spring-Fall)\n")
cat("- Added time_suitability column for scheduling\n")
cat("- Generated comprehensive tags\n")
cat("- Filled neighborhoods and sections\n")
cat("- All missing data completed with defaults\n")