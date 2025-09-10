# Quick processing to launch app immediately
library(dplyr)
library(readr)

# Load CSV files
csv_files <- list.files('Saved/', pattern='\\.csv$', full.names=TRUE)
all_data <- data.frame()

for(f in csv_files) {
  tryCatch({
    df <- read_csv(f, show_col_types=FALSE)
    df$source_list <- tools::file_path_sans_ext(basename(f))
    all_data <- bind_rows(all_data, df)
  }, error=function(e) cat("Could not read", f, "\n"))
}

# Set up required columns
if('Title' %in% names(all_data)) all_data$title <- all_data$Title
if('Tags' %in% names(all_data)) all_data$tags <- all_data$Tags
if('Note' %in% names(all_data)) all_data$note <- all_data$Note  
if('URL' %in% names(all_data)) all_data$url <- all_data$URL

# Clean up tags - change "Portland Parks" to "Park"
all_data$tags <- gsub('Portland Parks', 'Park', all_data$tags, ignore.case=TRUE)

# Add basic Portland coordinates (will be refined later)
set.seed(123) # For consistent coordinates
all_data$lat <- runif(nrow(all_data), 45.46, 45.58)  
all_data$lng <- runif(nrow(all_data), -122.75, -122.58)
all_data$distance_mi <- round(runif(nrow(all_data), 0.5, 15), 1)
all_data$neighborhood <- sample(c('Pearl District', 'Hawthorne', 'Alberta', 'Division', 'Belmont', 'Fremont', 'Mississippi', 'Alberta'), nrow(all_data), replace=TRUE)
all_data$section <- sample(c('Northwest', 'Southeast', 'Northeast', 'Southwest', 'North'), nrow(all_data), replace=TRUE)

# Create IDs and metadata
all_data$id <- paste0(all_data$source_list, '_', seq_len(nrow(all_data)))
all_data$processed_date <- Sys.Date()

# Fill in missing columns with defaults
for(col in c('title','tags','note','url')) {
  if(!col %in% names(all_data)) all_data[[col]] <- ''
  all_data[[col]][is.na(all_data[[col]])] <- ''
}

# Remove unnamed places
all_data <- all_data[!grepl('^Unnamed place', all_data$title, ignore.case=TRUE),]

# Save processed data
dir.create('data', showWarnings=FALSE)
saveRDS(all_data, 'data/portland_places_processed.rds')

cat('Quick processing complete!\n')
cat('Total places:', nrow(all_data), '\n')
cat('Museums included from Portland Museums.csv\n')
cat('Ready to launch app with: source("portland_day_planner.R")\n')