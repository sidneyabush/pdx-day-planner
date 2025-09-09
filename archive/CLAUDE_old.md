# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Portland Day Picker Shiny application that helps users discover and track places to visit in Portland, Oregon. The app uses geographic data processing to tag places with neighborhood and section information, and provides an interactive map interface for selecting destinations.

## Key Architecture Components

### Core Application (`app.R`)
- **Shiny Web Application**: Interactive map-based interface for place discovery
- **Data Pipeline**: Loads and processes GeoJSON files from `out_lists_portland_tagged/`
- **Geospatial Processing**: Uses `sf` package for spatial operations and distance calculations
- **Persistence**: Saves completed place IDs to `data/completed_ids.rds`
- **Home Location**: Click-to-set home coordinates for distance-based filtering
- **Filtering System**: Multi-criteria filtering by sections, neighborhoods, keywords, and bike distance

### Data Processing Scripts

#### `tag_points_with_areas.R`
- **Geographic Tagging**: Spatially joins places with Portland neighborhood boundaries and administrative sextants
- **Data Normalization**: Standardizes field names and handles missing columns across different data sources
- **Combo Mapping**: Uses `data/neighborhood_combo_map.csv` for normalized neighborhood naming
- **Deduplication**: Combines multiple data sources while preserving mapped labels and preferring GeoJSON over KML

#### `csv_saved_lists_to_kml.R` 
- **Geocoding Pipeline**: Converts CSV place lists to geographic formats (KML/GeoJSON)
- **URL Coordinate Extraction**: Parses Google Maps URLs for coordinates with Portland-area validation
- **Multi-Provider Geocoding**: Fallback chain through Google → ArcGIS → OpenStreetMap
- **Coordinate Validation**: Distance-based validation against Portland center (120km radius)
- **Caching System**: Maintains `_geocode_cache.csv` to avoid redundant API calls

### Data Structure

#### Input Data
- `out_lists_portland/`: Raw place data files
- `data/Neighborhood_Boundaries.geojson`: Portland neighborhood polygons  
- `data/Portland_Administrative_Sextants.geojson`: City administrative sections
- `data/neighborhood_combo_map.csv`: Neighborhood name normalization mapping

#### Processed Data
- `out_lists_portland_tagged/`: GeoJSON files with neighborhood/section tags
- `data/completed_ids.rds`: User's completed place tracking

## Running the Application

### Start Shiny App
```r
# From R console or RStudio
source("app.R")
```

### Data Processing Workflow
```r
# 1. Convert CSV lists to geographic formats
source("csv_saved_lists_to_kml.R")

# 2. Tag places with neighborhoods/sections  
source("tag_points_with_areas.R")

# 3. Run Shiny app (reads from out_lists_portland_tagged/)
source("app.R")
```

## Key Dependencies

- **Geospatial**: `sf`, `leaflet` for mapping and spatial operations
- **Data Processing**: `dplyr`, `purrr`, `stringr` for data manipulation
- **Web Framework**: `shiny` for the interactive application
- **File I/O**: `readr` for CSV handling, built-in functions for RDS persistence

## Development Notes

- **Hardcoded Paths**: The app uses absolute paths rooted at `/Users/sidneybush/Documents/GitHub/Portland_shiny_app` - update `proj_root` in `app.R` for different environments
- **Coordinate Systems**: All spatial data is transformed to WGS84 (EPSG:4326) for consistency
- **Distance Calculations**: Uses great circle distances converted to miles for bike filtering (≤10 miles)
- **Error Handling**: Graceful fallbacks for missing data files and failed geocoding attempts