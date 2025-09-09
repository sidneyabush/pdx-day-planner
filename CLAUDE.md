# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **simplified** Portland Day-Off Planner: a single-file Shiny application that reads Google Maps CSV exports directly and creates an interactive day planning tool. The philosophy is maximum simplicity with minimum complexity.

## Key Architecture

### Main Application (`portland_day_planner.R`) 🌟
- **Single File Solution**: Everything in one script - no complex pipeline
- **Direct CSV Processing**: Reads Google Maps exports directly from `Saved/` folder
- **Built-in Coordinate Extraction**: Parses Google Maps URLs for lat/lng
- **Automatic Section Detection**: Guesses Portland areas from place names
- **Smart Distance Calculation**: Uses Haversine formula for accurate distances
- **Interactive Filtering**: Activity categories + transportation modes + keywords

## Super Simple Workflow

1. **User exports Google Maps Saved Lists as CSV**
2. **Places CSV files in `Saved/` folder** 
3. **Runs: `source("portland_day_planner.R")`**
4. **App launches automatically with all data loaded**

## Key Features

### Home Base Configuration
- **Fixed Address**: 3976 N Gantenbein Avenue, Portland, OR 97227
- **Coordinates**: 45.5623, -122.6754
- **All distance calculations reference this point**
- **Easy to modify**: Just change `HOME_LAT`, `HOME_LNG`, `HOME_ADDRESS` variables

### Activity Categories (Emoji-Based)
Matches user's Google Maps emoji tags:
- ☕️ Coffee & Cafes
- 🍰 Sweet Treats  
- 📚 Bookstores
- 🍸 Drinks & Bars
- 🏷️ Thrift & Vintage
- 🚲 Outdoor & Sports
- And more...

### Transportation Distance Filtering
- 🚶 Walking: ≤2 miles
- 🚲 Biking: ≤10 miles  
- 🚗 Driving: ≤30 miles
- 🌍 Any Distance: No limit

### Portland Section Detection
Simple keyword matching for Portland areas:
- **Southwest**: Downtown, South Portland, etc.
- **Northwest**: Pearl, Nob Hill, Old Town
- **Southeast**: Hawthorne, Division, Richmond, etc.
- **Northeast**: Alberta, Fremont, Lloyd, etc.
- **North**: Mississippi, Boise, St Johns, etc.

## File Structure

```
Portland_shiny_app/
├── Saved/                     # Google Maps CSV exports (INPUT)
├── data/                      # Visited places tracking (AUTO-CREATED)
│   └── completed_places.rds  
├── portland_day_planner.R     # 🌟 MAIN APP (ONLY FILE NEEDED)
└── archive/                   # Complex old scripts (IGNORE)
```

## CSV Data Format Expected

From Google Maps Saved Lists exports:
- **Title**: Place name
- **Tags**: Emoji categories (☕️ Coffee, 📚 Bookstores, etc.)  
- **URL**: Google Maps link (contains @lat,lng coordinates)
- **Note**: Optional description
- **Comment**: Optional additional info

## Key Functions in the Script

### Data Processing
- `extract_coords_from_url()`: Parses Google Maps URLs for coordinates
- `calc_distance_miles()`: Haversine distance calculation from home
- `guess_portland_section()`: Simple keyword matching for Portland areas
- `matches_activity()`: Emoji tag matching for activity categories

### App Logic
- `load_csv_data()`: Reads and processes all CSV files from Saved/
- `load_completed()`/`save_completed()`: Persistent visited place tracking
- Reactive filtering based on activities, transport, and keywords
- Interactive map with home base, filtered places, visited markers

## Dependencies

**Required packages**: `shiny`, `leaflet`, `dplyr`, `readr`, `stringr`, `DT`

All standard CRAN packages - no complex geospatial dependencies.

## Development Notes

### Coordinate Extraction
- Handles multiple Google Maps URL formats (@lat,lng and !3d!4d patterns)
- Portland-area validation not enforced (keeps it simple)
- Falls back gracefully for URLs without coordinates

### Distance Calculation  
- Uses Haversine formula (great-circle distance)
- Earth radius: 3959 miles
- Returns rounded distances to 1 decimal place

### Section Detection
- Simple keyword matching against place titles and notes
- No complex geographic joins or boundary files needed
- Easily extensible by adding keywords to `PORTLAND_SECTIONS` list

### Visited Place Tracking
- Stores visited place IDs in `data/completed_places.rds`
- Persistent across app sessions
- Can reset or undo individual places

## Why This Simplified Approach?

1. **No data pipeline complexity** - works directly with CSV exports
2. **No external geographic files** - everything computed from text
3. **Single file deployment** - easy to share and modify
4. **Instant updates** - just replace CSV files and restart
5. **Minimal dependencies** - standard R packages only
6. **Fast startup** - no heavy data processing steps

## Archived Complex Approach

The `archive/` folder contains the previous complex approach with:
- Multi-step geocoding pipeline
- Geographic boundary file processing  
- Neighborhood polygon spatial joins
- Multiple processing scripts

This was functional but overly complex for the simple use case.

## Customization

**Activity Categories**: Add to `ACTIVITY_CATEGORIES` list
**Distance Limits**: Modify `TRANSPORT_MODES` values  
**Home Location**: Change `HOME_LAT`, `HOME_LNG`, `HOME_ADDRESS`
**Portland Areas**: Add keywords to `PORTLAND_SECTIONS` mapping