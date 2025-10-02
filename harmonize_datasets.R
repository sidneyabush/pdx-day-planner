#!/usr/bin/env Rscript

# Dataset Harmonization Script
# Combines existing Portland places data with new Reddit activities data

library(dplyr)
library(readr)

# Read existing processed data
existing_data <- read_csv("data/portland_places_processed.csv", show_col_types = FALSE)

# Read new Reddit data
reddit_data <- read_csv("data/Portland Things to Do - Reddit Edition - Adults.csv", show_col_types = FALSE)

# Clean up Reddit data column names (remove special characters)
names(reddit_data) <- make.names(names(reddit_data))

print("Original datasets loaded:")
print(paste("Existing data:", nrow(existing_data), "rows"))
print(paste("Reddit data:", nrow(reddit_data), "rows"))

# Function to determine seasonal category from row position
get_seasonal_category <- function(row_num) {
  if (row_num <= 110) return("Year-round")
  if (row_num <= 120) return("Occasional")
  if (row_num <= 139) return("Seasonal")
  return("Annual")
}

# Function to extract frequency from When column
get_frequency <- function(when_text) {
  if (is.na(when_text) || when_text == "") return("Regular")
  when_lower <- tolower(when_text)

  if (grepl("always|every day|daily", when_lower)) return("Daily")
  if (grepl("weekend|saturday|sunday", when_lower)) return("Weekends")
  if (grepl("monday|tuesday|wednesday|thursday|friday", when_lower)) return("Weekdays")
  if (grepl("once|annual|yearly", when_lower)) return("Annual")
  if (grepl("seasonal|summer|winter|spring|fall", when_lower)) return("Seasonal")
  if (grepl("occasional|sometimes", when_lower)) return("Occasional")

  return("Regular")
}

# Function to categorize activities from What column
categorize_activity <- function(what_text, notes_text = "") {
  if (is.na(what_text)) return("Other")

  what_lower <- tolower(paste(what_text, notes_text))

  if (grepl("museum|exhibit|art|gallery", what_lower)) return("Museums/Arts")
  if (grepl("hike|trail|park|nature|outdoor", what_lower)) return("Nature/Outdoor")
  if (grepl("restaurant|food|eat|drink|bar|brewery|cafe", what_lower)) return("Food/Drink")
  if (grepl("theater|movie|show|performance|cinema", what_lower)) return("Entertainment")
  if (grepl("shop|market|store", what_lower)) return("Shopping")
  if (grepl("trivia|game|arcade", what_lower)) return("Games/Trivia")
  if (grepl("festival|event|fair", what_lower)) return("Events/Festivals")
  if (grepl("class|learn|workshop|lesson", what_lower)) return("Learning/Classes")
  if (grepl("spa|relax|massage", what_lower)) return("Wellness")

  return("Activities")
}

# Process Reddit data
reddit_processed <- reddit_data %>%
  filter(!is.na(What.) & What. != "" & !grepl("^,", What.)) %>%  # Remove empty/invalid rows
  mutate(
    row_number = row_number(),
    # Map to existing schema
    title = What.,
    note = ifelse(!is.na(Notes) & Notes != "", Notes, ""),
    url = ifelse(!is.na(URL) & URL != "", URL, ""),
    tags = "",  # Will be populated based on activity type

    # New temporal columns
    when_available = ifelse(!is.na(When.) & When. != "", When., "Every day"),
    seasonal_category = sapply(row_number, get_seasonal_category),
    frequency = sapply(When., get_frequency),
    activity_type = mapply(categorize_activity, What., Notes),

    # Location info for later geocoding
    location_text = ifelse(!is.na(Where.) & Where. != "", Where., ""),

    # Source tracking
    source_list = "Reddit Portland Activities",
    processed_date = Sys.Date(),

    # Placeholder for coordinates (to be geocoded later)
    lat = NA_real_,
    lng = NA_real_,
    distance_mi = NA_real_,
    neighborhood = NA_character_,
    section = NA_character_,

    # Generate unique ID
    id = paste0("Reddit_", row_number)
  ) %>%
  select(title, note, url, tags, when_available, seasonal_category, frequency,
         activity_type, location_text, source_list, lat, lng, distance_mi,
         neighborhood, section, id, processed_date)

# Add new columns to existing data with default values
existing_enhanced <- existing_data %>%
  mutate(
    when_available = "Every day",  # Default for existing places
    seasonal_category = "Year-round",
    frequency = "Regular",
    activity_type = case_when(
      grepl("hike|trail|park", tolower(paste(title, tags, note)), ignore.case = TRUE) ~ "Nature/Outdoor",
      grepl("restaurant|food|cafe|bar", tolower(paste(title, tags, note)), ignore.case = TRUE) ~ "Food/Drink",
      grepl("theater|movie", tolower(paste(title, tags, note)), ignore.case = TRUE) ~ "Entertainment",
      grepl("museum|art", tolower(paste(title, tags, note)), ignore.case = TRUE) ~ "Museums/Arts",
      TRUE ~ "Other"
    ),
    location_text = paste(title, neighborhood, section) # Reconstruct location for consistency
  )

# Ensure both datasets have the same columns in the same order
common_cols <- intersect(names(existing_enhanced), names(reddit_processed))
existing_final <- existing_enhanced %>% select(all_of(common_cols))
reddit_final <- reddit_processed %>% select(all_of(common_cols))

# Add any missing columns with NA values
all_cols <- unique(c(names(existing_enhanced), names(reddit_processed)))
for (col in all_cols) {
  if (!col %in% names(existing_final)) existing_final[[col]] <- NA
  if (!col %in% names(reddit_final)) reddit_final[[col]] <- NA
}

# Reorder columns consistently
col_order <- c("title", "note", "url", "tags", "when_available", "seasonal_category",
               "frequency", "activity_type", "location_text", "source_list",
               "lat", "lng", "distance_mi", "neighborhood", "section", "id", "processed_date")

existing_final <- existing_final %>% select(all_of(intersect(col_order, names(existing_final))), everything())
reddit_final <- reddit_final %>% select(all_of(intersect(col_order, names(reddit_final))), everything())

# Combine datasets
combined_data <- bind_rows(existing_final, reddit_final)

print("\nCombined dataset summary:")
print(paste("Total rows:", nrow(combined_data)))
print("\nSeasonal category breakdown:")
print(table(combined_data$seasonal_category, useNA = "ifany"))
print("\nActivity type breakdown:")
print(table(combined_data$activity_type, useNA = "ifany"))
print("\nFrequency breakdown:")
print(table(combined_data$frequency, useNA = "ifany"))

# Save combined dataset
write_csv(combined_data, "data/portland_places_combined.csv")
print("\nCombined dataset saved as: data/portland_places_combined.csv")

# Create summary statistics
summary_stats <- combined_data %>%
  group_by(source_list, seasonal_category, activity_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(source_list, seasonal_category, activity_type)

write_csv(summary_stats, "data/harmonization_summary.csv")
print("Summary statistics saved as: data/harmonization_summary.csv")

print("\nHarmonization complete!")
print("New columns added:")
print("- when_available: Timing/schedule information")
print("- seasonal_category: Year-round, Seasonal, Occasional, Annual")
print("- frequency: Daily, Weekends, Weekdays, Regular, etc.")
print("- activity_type: Categorized activity types")
print("- location_text: Location information for geocoding")