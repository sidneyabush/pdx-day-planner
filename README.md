# Portland Day-Off Planner
*A tool for exploring Portland's local spots*

My current app can be found and run [**here**](https://sidneyabush.shinyapps.io/portland-day-planner/), but you can also populate the app with your own saved lists from Google Maps by following the steps below.

## Workflow

1. **Export your Google Maps Saved Lists as CSV files**
2. **Place CSV files in the `Saved/` folder**
3. **Run the app:**
   ```r
   source("portland_day_planner.R")
   ```
## Current Features

### Mood-Based Filtering
- Coffee & Cafes
- Sweet Treats  
- Bookstores
- Drinks & Bars 
- Thrift & Vintage 

### Transportation Filtering
- **Walking**: 2 miles or less from home
- **Biking**: 10 miles or less from home
- **Driving**: 30 miles or less from home
- **Any Distance**: No limits

### Customizable Home Location
- Enter any address in the app interface
- All distances calculated from your chosen location
- Automatically geocodes addresses using OpenStreetMap

### Smart Suggestions
- Click activity types that match your mood
- Select how you want to get there
- Get randomized suggestions from unvisited places
- Track where you've been to avoid repeats

## File Structure

```
Portland_shiny_app/
├── Saved/                     # Your Google Maps CSV exports go here
├── data/                      # App creates this for tracking visited places
├── portland_day_planner.R     # THE ONLY SCRIPT YOU NEED
├── verified_distant_places.R  # System file for coordinate accuracy
└── archive/                   # Old complex scripts (ignore these)
```

## Customization 
### Add Activity Categories
Add to the `ACTIVITY_CATEGORIES` list in the script:
```r
"Art & Museums" = c("Art", "museum", "gallery")
```

### Adjust Distance Limits
Modify `TRANSPORT_MODES` in the script:
```r
"Biking" = 15,  # Change from 10 to 15 miles
```

## Dependencies

```r
install.packages(c("shiny", "leaflet", "dplyr", "readr", "stringr", "DT", "httr", "jsonlite"))
```

## Future feature additions (WIP)
- Weather-aware suggestions
- Time-of-day filtering
- Route planning for multi-stop days
- Export day plans
- Seasonal activities (e.g., holiday season, summer)

---

