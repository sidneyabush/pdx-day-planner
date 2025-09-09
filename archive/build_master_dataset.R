# build_master_dataset.R — Complete data pipeline for Portland saved places
# This script runs the full data processing pipeline and creates a master dataset
# Run this whenever you add new places to your Saved/ folder

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(readr); library(stringr); 
  library(purrr); library(tibble); library(stringi)
})

# Set working directory
setwd("/Users/sidneybush/Documents/GitHub/Portland_shiny_app")

# Source the processing functions
source("csv_saved_lists_to_kml.R")
source("tag_points_with_areas.R")

cat("🚀 Starting Portland Places Data Pipeline...\n")

# ---------------- STEP 1: Convert CSV to Geographic Formats ----------------
cat("\n📍 Step 1: Converting CSV files to geographic formats...\n")

# Ensure output directory exists
dir.create("out_lists_portland", showWarnings = FALSE)

# Get all CSV files from Saved folder
saved_files <- list.files("Saved", pattern = "\\.csv$", full.names = FALSE)
cat("Found", length(saved_files), "CSV files in Saved/\n")

# Convert CSV files to KML/GeoJSON
convert_saved_csv_folder_to_kml(
  in_dir = "Saved",
  out_dir = "out_lists_portland", 
  include_files = saved_files,  # Process all CSV files
  geocode_if_missing = TRUE,
  geocode_methods = c("google", "arcgis", "osm"),
  city_hint = "Portland, OR",
  cache_csv = "out_lists_portland/_geocode_cache.csv",
  pause_sec = 0.8,
  report_missing_dir = "out_lists_portland",
  url_prefer = "viewport"
)

cat("✅ Geographic conversion complete\n")

# ---------------- STEP 2: Tag with Neighborhoods & Sections ----------------
cat("\n🏘️ Step 2: Tagging places with neighborhoods and sections...\n")

# Ensure data directory and combo mapping exist
dir.create("data", showWarnings = FALSE)
combo_map_path <- "data/neighborhood_combo_map.csv"

# Create skeleton mapping if missing
if (!file.exists(combo_map_path)) {
  write_combo_skeleton("data/Neighborhood_Boundaries.geojson", combo_map_path)
  cat("Created neighborhood combo mapping file\n")
}

# Ensure Downtown mapping exists
norm_name <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  tolower(trimws(gsub("\\s+", " ", x)))
}

combo_map <- readr::read_csv(combo_map_path, show_col_types = FALSE)
if (!any(norm_name(combo_map$neighborhood) == "portland downtown")) {
  combo_map <- dplyr::bind_rows(
    combo_map,
    tibble(neighborhood = "PORTLAND DOWNTOWN", combo_label = "Downtown")
  ) |>
    dplyr::distinct(neighborhood, .keep_all = TRUE) |>
    dplyr::arrange(neighborhood)
  readr::write_csv(combo_map, combo_map_path)
  cat("Added mapping: PORTLAND DOWNTOWN → Downtown\n")
}

# Run the tagging process
tag_points_with_areas(
  in_dir = "out_lists_portland",
  out_dir = "out_lists_portland_tagged",
  neighborhoods_path = "data/Neighborhood_Boundaries.geojson",
  sextants_path = "data/Portland_Administrative_Sextants.geojson", 
  combo_map_csv = combo_map_path
)

cat("✅ Geographic tagging complete\n")

# ---------------- STEP 3: Create Master Dataset ----------------
cat("\n📊 Step 3: Building master dataset...\n")

# Load all tagged files
tagged_files <- list.files("out_lists_portland_tagged", pattern = "\\.geojson$", full.names = TRUE)

if (length(tagged_files) == 0) {
  stop("❌ No tagged files found. Check previous steps.")
}

# Read and combine all data
master_data <- tagged_files |>
  map_dfr(function(file) {
    tryCatch({
      data <- st_read(file, quiet = TRUE)
      data$source_file <- basename(tools::file_path_sans_ext(file))
      data
    }, error = function(e) {
      warning(paste("Could not read", file, ":", e$message))
      NULL
    })
  })

# Add processing metadata
master_data$processed_date <- Sys.Date()
master_data$processing_version <- "1.0"

# Ensure consistent column types
master_data$title <- as.character(master_data$title)
master_data$desc <- as.character(master_data$desc) 
master_data$address <- as.character(master_data$address)
master_data$url <- as.character(master_data$url)
master_data$neighborhood <- as.character(master_data$neighborhood)
master_data$section <- as.character(master_data$section)
master_data$combo_label <- as.character(master_data$combo_label)

# Add home distance (using fixed address)
HOME_LAT <- 45.5623
HOME_LNG <- -122.6754

compute_distance_mi <- function(home_lat, home_lng, pts) {
  if (nrow(pts) == 0) return(numeric(0))
  home_pt <- st_sfc(st_point(c(home_lng, home_lat)), crs = 4326)
  as.numeric(st_distance(st_transform(pts, 4326), home_pt)) / 1609.344
}

master_data$distance_from_home_mi <- compute_distance_mi(HOME_LAT, HOME_LNG, master_data)

# Create unique ID
master_data$master_id <- paste0("PDX_", seq_len(nrow(master_data)))

# Save master dataset
master_path <- "data/portland_places_master.geojson"
st_write(master_data, master_path, delete_dsn = TRUE, quiet = TRUE)

cat("✅ Master dataset saved to:", master_path, "\n")

# ---------------- STEP 4: Generate Summary Report ----------------
cat("\n📈 Step 4: Generating summary report...\n")

# Create summary statistics
summary_stats <- list(
  total_places = nrow(master_data),
  processing_date = Sys.Date(),
  source_files = unique(master_data$source_file),
  neighborhoods = length(unique(na.omit(master_data$neighborhood))),
  sections = length(unique(na.omit(master_data$section))),
  avg_distance_from_home = round(mean(master_data$distance_from_home_mi, na.rm = TRUE), 1),
  max_distance_from_home = round(max(master_data$distance_from_home_mi, na.rm = TRUE), 1)
)

# Count by source file
by_source <- master_data |>
  st_drop_geometry() |>
  count(source_file, sort = TRUE) |>
  rename(places = n)

# Count by neighborhood (top 10)
by_neighborhood <- master_data |>
  st_drop_geometry() |>
  filter(!is.na(neighborhood)) |>
  count(neighborhood, sort = TRUE) |>
  head(10) |>
  rename(places = n)

# Count by section
by_section <- master_data |>
  st_drop_geometry() |>
  filter(!is.na(section)) |>
  count(section, sort = TRUE) |>
  rename(places = n)

# Distance distribution
distance_summary <- master_data |>
  st_drop_geometry() |>
  filter(!is.na(distance_from_home_mi)) |>
  summarise(
    min_distance = round(min(distance_from_home_mi), 1),
    q25_distance = round(quantile(distance_from_home_mi, 0.25), 1),
    median_distance = round(median(distance_from_home_mi), 1),
    q75_distance = round(quantile(distance_from_home_mi, 0.75), 1),
    max_distance = round(max(distance_from_home_mi), 1)
  )

# Print summary report
cat("\n" %>% {rep(paste0(., "="), 50)} %>% paste(collapse = ""), "\n")
cat("📊 PORTLAND PLACES MASTER DATASET SUMMARY\n")
cat("=" %>% {rep(., 50)} %>% paste(collapse = ""), "\n\n")

cat("📍 OVERVIEW:\n")
cat("  Total Places:", summary_stats$total_places, "\n")
cat("  Processing Date:", as.character(summary_stats$processing_date), "\n")
cat("  Unique Neighborhoods:", summary_stats$neighborhoods, "\n")
cat("  City Sections:", summary_stats$sections, "\n\n")

cat("📂 BY SOURCE FILE:\n")
for(i in seq_len(nrow(by_source))) {
  cat("  ", by_source$source_file[i], ":", by_source$places[i], "places\n")
}
cat("\n")

cat("🏘️ TOP NEIGHBORHOODS:\n")
for(i in seq_len(min(5, nrow(by_neighborhood)))) {
  cat("  ", by_neighborhood$neighborhood[i], ":", by_neighborhood$places[i], "places\n")
}
cat("\n")

cat("🗺️ BY SECTION:\n")
for(i in seq_len(nrow(by_section))) {
  cat("  ", by_section$section[i], ":", by_section$places[i], "places\n")
}
cat("\n")

cat("📏 DISTANCE FROM HOME (3976 N Gantenbein Ave):\n")
cat("  Min:", distance_summary$min_distance, "miles\n")
cat("  25th percentile:", distance_summary$q25_distance, "miles\n") 
cat("  Median:", distance_summary$median_distance, "miles\n")
cat("  75th percentile:", distance_summary$q75_distance, "miles\n")
cat("  Max:", distance_summary$max_distance, "miles\n")
cat("  Average:", summary_stats$avg_distance_from_home, "miles\n\n")

cat("✅ DATA PIPELINE COMPLETE!\n")
cat("📁 Files created:\n")
cat("  - Master dataset: data/portland_places_master.geojson\n")
cat("  - Tagged individual files: out_lists_portland_tagged/*.geojson\n")
cat("  - Geocoding cache: out_lists_portland/_geocode_cache.csv\n\n")

cat("🚀 Ready to run day_off_app.R for your day planning!\n")
cat("=" %>% {rep(., 50)} %>% paste(collapse = ""), "\n")

# Return summary stats invisibly for potential use
invisible(list(
  summary = summary_stats,
  by_source = by_source,
  by_neighborhood = by_neighborhood, 
  by_section = by_section,
  distance_summary = distance_summary,
  master_data = master_data
))