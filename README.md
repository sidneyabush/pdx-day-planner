# Portland Day-Off Planner

The simplest way to discover perfect days off in Portland! Transform your Google Maps Saved Lists into a smart, mood-based exploration tool.

## Super Simple Workflow

1. **Export your Google Maps Saved Lists as CSV files**
2. **Place CSV files in the `Saved/` folder**
3. **Run the app:**
   ```r
   source("portland_day_planner.R")
   ```
4. **That's it!** The app handles everything automatically.

## Features

### Mood-Based Discovery
- **Coffee & Cafes** - Perfect morning spots
- **Sweet Treats** - Dessert adventures  
- **Bookstores** - Literary escapes
- **Drinks & Bars** - Evening vibes
- **Thrift & Vintage** - Treasure hunting
- And many more based on your emoji tags!

### Smart Transportation Filtering
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

## How It Works

The app automatically:
1. **Reads all CSV files** from your `Saved/` folder
2. **Extracts coordinates** from Google Maps URLs  
3. **Calculates distances** from your home
4. **Guesses Portland sections** from place names
5. **Creates an interactive map** with all your places
6. **Provides smart filtering** based on your mood and transportation

## Supported CSV Format

Your Google Maps Saved Lists should export with these columns:
- **Title** - Place name
- **Tags** - Your emoji categories (Coffee, Bookstores, etc.)
- **URL** - Google Maps link (contains coordinates)
- **Note** - Optional notes

## Adding New Places

1. **Add places to your Google Maps lists** (on phone/web)
2. **Re-export the updated lists as CSV**
3. **Replace files in `Saved/` folder**
4. **Restart the app** - it automatically loads the new data!

## Customization

### Using the Address Input
Simply enter your address in the "Your Address" field at the top of the app and click "Set Location". The app will automatically:
- Geocode your address using OpenStreetMap
- Update all distance calculations
- Center the map on your location

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

## Why This Approach?

- **No complex data processing** - works directly with your CSV exports
- **No external dependencies** - just standard R packages  
- **Automatic updates** - just replace CSV files and restart
- **One file to rule them all** - everything in a single script
- **Instant gratification** - from CSV to app in seconds!

## Making Your App Public

There are several ways to deploy your Shiny app so others can access it without installing R:

### Free Options

#### 1. shinyapps.io (Recommended for beginners)
```r
# Install deployment package
install.packages("rsconnect")

# Set up account at shinyapps.io, then:
rsconnect::setAccountInfo(name='your-account-name', 
                         token='your-token', 
                         secret='your-secret')

# Deploy the app
rsconnect::deployApp()
```

#### 2. Hugging Face Spaces
- Create a free account at huggingface.co
- Create a new Space with R/Shiny runtime
- Upload your files to the Space repository
- The app will automatically deploy

### Self-Hosted Options

#### 3. DigitalOcean App Platform
- Connect your GitHub repository
- Choose R buildpack
- Automatic deployment from GitHub

#### 4. Shiny Server (On your own server)
- Install Shiny Server on a Linux server
- Copy your app files to `/srv/shiny-server/`
- Access via your server's IP address

### Deployment Checklist

Before deploying:
- [ ] Test your app locally
- [ ] Ensure all dependencies are listed
- [ ] Add your CSV data files to the repository
- [ ] Consider data privacy (remove personal addresses if needed)
- [ ] Add usage instructions for new users

## Future Ideas

- Weather-aware suggestions
- Time-of-day filtering
- Route planning for multi-stop days
- Export day plans

---

*A practical tool for exploring Portland's local spots*