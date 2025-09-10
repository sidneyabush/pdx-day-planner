# Add feature categorization to processed data
library(dplyr)

# Load current processed data
data <- readRDS('data/portland_places_processed.rds')

cat("Adding feature categorization...\n")

# Add feature column
data$feature <- NA_character_

# Categorize trails (from Hikes & Trail Runs list and trail-related tags)
trail_indices <- which(
  data$source_list == "Hikes & Trail Runs" |
  grepl("trail|hike|run|path|trek", data$title, ignore.case = TRUE) |
  grepl("trail|hike|run|path|trek", data$tags, ignore.case = TRUE)
)

data$feature[trail_indices] <- "Trail"
cat("Categorized", length(trail_indices), "places as Trail\n")

# Categorize museums (from Portland Museums list and museum-related tags)
museum_indices <- which(
  data$source_list == "Portland Museums" |
  grepl("museum|gallery|art center|exhibition", data$title, ignore.case = TRUE) |
  grepl("museum|gallery|art center|exhibition", data$tags, ignore.case = TRUE)
)

data$feature[museum_indices] <- "Museum"
cat("Categorized", length(museum_indices), "places as Museum\n")

# Show examples
cat("\nTrail examples:\n")
trail_examples <- data[!is.na(data$feature) & data$feature == "Trail", c("title", "source_list")]
print(head(trail_examples, 5))

cat("\nMuseum examples:\n") 
museum_examples <- data[!is.na(data$feature) & data$feature == "Museum", c("title", "source_list")]
print(head(museum_examples, 5))

# Save updated data
saveRDS(data, 'data/portland_places_processed.rds')

cat("\nFeature categorization complete!\n")
cat("Total places with features:", sum(!is.na(data$feature)), "\n")
cat("Features breakdown:\n")
print(table(data$feature, useNA = "ifany"))