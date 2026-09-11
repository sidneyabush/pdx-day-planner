
# complete dataset script
# fill in all missing columns in the harmonized dataset

library(dplyr)
library(readr)
library(httr)
library(jsonlite)

# read the combined dataset
data <- read_csv("data/portland_places_combined.csv", show_col_types = FALSE)

cat("Starting dataset completion...\n")
cat("Total rows:", nrow(data), "\n")

# function to geocode addresses using nominatim
geocode_address <- function(address, city = "Portland, OR") {
  if (is.na(address) || address == "") return(list(lat = NA, lng = NA))

  # clean and prepare address
  full_address <- paste(address, city, sep = ", ")

  # url encode the address
  encoded_address <- URLencode(full_address)

  # nominatim api call
  url <- paste0("https://nominatim.openstreetmap.org/search?format=json&q=", encoded_address, "&limit=1")

  tryCatch(
    {
      response <- GET(url, user_agent("Portland Day Planner App"))
      if (status_code(response) == 200) {
        result <- fromJSON(content(response, "text"))
        if (length(result) > 0) {
          return(list(
            lat = as.numeric(result$lat[1]),
            lng = as.numeric(result$lon[1])
          ))
        }
      }
      return(list(lat = NA, lng = NA))
    },
    error = function(e) {
      return(list(lat = NA, lng = NA))
    })
}

# function to get neighborhood from coordinates (vectorized)
get_neighborhood_from_coords <- function(lat, lng) {
  ifelse(is.na(lat) | is.na(lng), NA_character_,
    ifelse(lat > 45.5152,
      ifelse(lng > -122.6784, "Northeast Portland", "Northwest Portland"),
      ifelse(lng > -122.6784, "Southeast Portland", "Southwest Portland")
    )
  )
}

# function to assign section based on neighborhood (vectorized)
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

# function to calculate distance from home address (vectorized)
calculate_distance <- function(lat1, lng1, lat2 = 45.558, lng2 = -122.665) {
  ifelse(is.na(lat1) | is.na(lng1), NA_real_, {
    R <- 3959  # earth's radius in miles
    lat1_rad <- lat1 * pi / 180
    lat2_rad <- lat2 * pi / 180
    dlat <- (lat2 - lat1) * pi / 180
    dlng <- (lng2 - lng1) * pi / 180
    a <- sin(dlat / 2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlng / 2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    round(R * c, 1)
  })
}

# function to determine seasonal availability based on activity type
get_seasonal_availability <- function(title, activity_type, source_list, original_seasonal = NULL) {
  if (!is.null(original_seasonal) && !is.na(original_seasonal) && original_seasonal != "Year-round") {
    return(original_seasonal)  # keep existing seasonal data
  }

  title_lower <- tolower(paste(title, activity_type))

  # outdoor activities are seasonal in portland
  if (grepl("hike|trail|park|outdoor|garden|zoo|waterfall|beach|river|lake", title_lower) ||
    activity_type == "Nature/Outdoor" ||
    grepl("Hikes|Parks", source_list)) {
    return("Seasonal (Spring-Fall)")
  }

  # winter-specific activities
  if (grepl("ski|snow|winter|christmas|holiday", title_lower)) {
    return("Seasonal (Winter)")
  }

  # summer-specific activities
  if (grepl("swim|beach|float|water park|outdoor concert", title_lower)) {
    return("Seasonal (Summer)")
  }

  # default to year-round for indoor activities
  return("Year-round")
}

# function to determine time of day suitability
get_time_suitability <- function(title, when_available = "", activity_type = "") {
  title_lower <- tolower(paste(title, when_available, activity_type))

  # morning activities
  if (grepl("breakfast|brunch|farmer|market|hike|trail|morning", title_lower)) {
    return("Morning,Afternoon")
  }

  # evening/night activities
  if (grepl("bar|brewery|dinner|theater|movie|nightlife|evening|night", title_lower)) {
    return("Afternoon,Evening")
  }

  # all-day activities
  if (grepl("museum|park|zoo|shop|gallery", title_lower)) {
    return("Morning,Afternoon,Evening")
  }

  # default to afternoon/evening
  return("Afternoon,Evening")
}

# function to generate tags from activity type and title (vectorized)
generate_tags <- function(activity_type, title, note = "") {
  # handle nas
  activity_type <- ifelse(is.na(activity_type), "", activity_type)
  title <- ifelse(is.na(title), "", title)
  note <- ifelse(is.na(note), "", note)

  # create base tags from activity type
  base_tags <- gsub("[/]", " ", tolower(activity_type))

  # extract additional tags from title and notes
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

  # combine base and additional tags
  final_tags <- ifelse(base_tags != "" & additional_tags != "",
    paste(base_tags, additional_tags, sep = ", "),
    ifelse(base_tags != "", base_tags, additional_tags))

  return(final_tags)
}

# process reddit activities that need geocoding
reddit_missing_coords <- data %>%
  filter(source_list == "Reddit Portland Activities" & (is.na(lat) | is.na(lng)))

cat("Geocoding", nrow(reddit_missing_coords), "Reddit activities...\n")

if (nrow(reddit_missing_coords) > 0) {
  # add rate limiting to avoid overwhelming the api
  reddit_geocoded <- reddit_missing_coords %>%
    rowwise() %>%
    mutate(
      coords = list({
        if (row_number() %% 10 == 0) cat("Processed", row_number(), "of", nrow(reddit_missing_coords), "\n")
        Sys.sleep(1)  # rate limiting
        geocode_address(location_text)
      }),
      lat = coords$lat,
      lng = coords$lng
    ) %>%
    select(-coords)

  # update the main dataset with geocoded coordinates
  for (i in 1:nrow(reddit_geocoded)) {
    idx <- which(data$id == reddit_geocoded$id[i])
    if (length(idx) > 0) {
      data$lat[idx] <- reddit_geocoded$lat[i]
      data$lng[idx] <- reddit_geocoded$lng[i]
    }
  }
}

cat("Filling in neighborhoods and sections...\n")

# fill in missing neighborhoods and sections
data <- data %>%
  mutate(
    # fill neighborhoods based on coordinates
    neighborhood = ifelse(
      is.na(neighborhood) & !is.na(lat) & !is.na(lng),
      get_neighborhood_from_coords(lat, lng),
      neighborhood
    ),

    # fill sections based on neighborhoods
    section = ifelse(
      is.na(section) & !is.na(neighborhood),
      get_section_from_neighborhood(neighborhood),
      section
    ),

    # calculate distances for missing values
    distance_mi = ifelse(
      is.na(distance_mi) & !is.na(lat) & !is.na(lng),
      calculate_distance(lat, lng),
      distance_mi
    ),

    # update seasonal categories based on activity type
    seasonal_category = mapply(get_seasonal_availability, title, activity_type, source_list, seasonal_category),

    # add time of day suitability
    time_suitability = mapply(get_time_suitability, title, when_available, activity_type),

    # generate tags for empty tags
    tags = ifelse(
      is.na(tags) | tags == "",
      generate_tags(activity_type, title, note),
      tags
    )
  )

# handle special location mappings for known portland places
location_mappings <- list(
  "Powell's Books" = list(lat = 45.5230, lng = -122.6814, neighborhood = "Pearl District", section = "Northwest"),
  "Pioneer Courthouse Square" = list(lat = 45.5190, lng = -122.6787, neighborhood = "Downtown", section = "Southwest"),
  "Multnomah Falls" = list(lat = 45.5762, lng = -122.1158, neighborhood = "Columbia River Gorge", section = "Outside Portland"),
  "Oregon Zoo" = list(lat = 45.5099, lng = -122.7160, neighborhood = "West Hills", section = "Southwest"),
  "OMSI" = list(lat = 45.5085, lng = -122.6650, neighborhood = "Central Eastside", section = "Southeast"),
  "Pittock Mansion" = list(lat = 45.5253, lng = -122.7161, neighborhood = "Forest Park", section = "Northwest"),
  "International Rose Test Garden" = list(lat = 45.5189, lng = -122.7064, neighborhood = "Washington Park", section = "Southwest")
)

# apply manual mappings for better accuracy
for (place_name in names(location_mappings)) {
  matches <- which(grepl(place_name, data$title, ignore.case = TRUE))
  if (length(matches) > 0) {
    mapping <- location_mappings[[place_name]]
    for (idx in matches) {
      if (is.na(data$lat[idx]) || data$lat[idx] == 45.516018) {  # default coords
        data$lat[idx] <- mapping$lat
        data$lng[idx] <- mapping$lng
        data$neighborhood[idx] <- mapping$neighborhood
        data$section[idx] <- mapping$section
        data$distance_mi[idx] <- calculate_distance(mapping$lat, mapping$lng)
      }
    }
  }
}

# final cleanup and validation
data <- data %>%
  mutate(
    # ensure all character fields are not na
    title = ifelse(is.na(title), "Unknown Place", title),
    note = ifelse(is.na(note), "", note),
    url = ifelse(is.na(url), "", url),
    tags = ifelse(is.na(tags), "", tags),
    when_available = ifelse(is.na(when_available), "Every day", when_available),
    seasonal_category = ifelse(is.na(seasonal_category), "Year-round", seasonal_category),
    frequency = ifelse(is.na(frequency), "Regular", frequency),
    activity_type = ifelse(is.na(activity_type), "Other", activity_type),
    location_text = ifelse(is.na(location_text), title, location_text),
    neighborhood = ifelse(is.na(neighborhood), "Portland Area", neighborhood),
    section = ifelse(is.na(section), "Central", section),

    # set default distance for remaining nas
    distance_mi = ifelse(is.na(distance_mi), 5.0, distance_mi),

    # Ensure processed_date is set
    processed_date = ifelse(is.na(processed_date), as.character(Sys.Date()), as.character(processed_date))
  )

# save the completed dataset
write_csv(data, "data/portland_places_complete.csv")

# generate completion report
cat("\nDataset completion summary:\n")
cat("Total rows:", nrow(data), "\n")
cat("Rows with coordinates:", sum(!is.na(data$lat) & !is.na(data$lng)), "\n")
cat("Rows with neighborhoods:", sum(!is.na(data$neighborhood) & data$neighborhood != ""), "\n")
cat("Rows with tags:", sum(!is.na(data$tags) & data$tags != ""), "\n")
cat("Rows with distances:", sum(!is.na(data$distance_mi)), "\n")

# check for remaining missing data
missing_coords <- sum(is.na(data$lat) | is.na(data$lng))
missing_neighborhoods <- sum(is.na(data$neighborhood) | data$neighborhood == "")
missing_tags <- sum(is.na(data$tags) | data$tags == "")

cat("\nRemaining missing data:\n")
cat("Missing coordinates:", missing_coords, "\n")
cat("Missing neighborhoods:", missing_neighborhoods, "\n")
cat("Missing tags:", missing_tags, "\n")

# activity type breakdown
cat("\nActivity type distribution:\n")
activity_counts <- table(data$activity_type)
for (activity in names(activity_counts)) {
  cat(paste0(activity, ": ", activity_counts[activity]), "\n")
}

cat("\nCompleted dataset saved as: data/portland_places_complete.csv\n")
