# 🌟 Portland Day-Off Planner

The simplest way to discover perfect days off in Portland! Transform your Google Maps Saved Lists into a smart, mood-based exploration tool.

## 🚀 Super Simple Workflow

1. **Export your Google Maps Saved Lists as CSV files**
2. **Place CSV files in the `Saved/` folder**
3. **Run the app:**
   ```r
   source("portland_day_planner.R")
   ```
4. **That's it!** The app handles everything automatically.

## ✨ Features

### 🎯 Mood-Based Discovery
- **☕️ Coffee & Cafes** - Perfect morning spots
- **🍰 Sweet Treats** - Dessert adventures  
- **📚 Bookstores** - Literary escapes
- **🍸 Drinks & Bars** - Evening vibes
- **🏷️ Thrift & Vintage** - Treasure hunting
- And many more based on your emoji tags!

### 🚶 Smart Transportation Filtering
- **Walking**: ≤2 miles from home
- **Biking**: ≤10 miles from home
- **Driving**: ≤30 miles from home
- **Any Distance**: No limits

### 🏠 Fixed Home Base
- **Address**: 3976 N Gantenbein Avenue, Portland, OR 97227
- All distances calculated from your home
- Easy to modify in the script if needed

### 🎲 Smart Suggestions
- Click activity types that match your mood
- Select how you want to get there
- Get randomized suggestions from unvisited places
- Track where you've been to avoid repeats

## 📁 File Structure

```
Portland_shiny_app/
├── Saved/                     # Your Google Maps CSV exports go here
├── data/                      # App creates this for tracking visited places
├── portland_day_planner.R     # 🌟 THE ONLY SCRIPT YOU NEED
└── archive/                   # Old complex scripts (ignore these)
```

## 🎯 How It Works

The app automatically:
1. **Reads all CSV files** from your `Saved/` folder
2. **Extracts coordinates** from Google Maps URLs  
3. **Calculates distances** from your home
4. **Guesses Portland sections** from place names
5. **Creates an interactive map** with all your places
6. **Provides smart filtering** based on your mood and transportation

## 📊 Supported CSV Format

Your Google Maps Saved Lists should export with these columns:
- **Title** - Place name
- **Tags** - Your emoji categories (☕️ Coffee, 📚 Bookstores, etc.)
- **URL** - Google Maps link (contains coordinates)
- **Note** - Optional notes

## 🔄 Adding New Places

1. **Add places to your Google Maps lists** (on phone/web)
2. **Re-export the updated lists as CSV**
3. **Replace files in `Saved/` folder**
4. **Restart the app** - it automatically loads the new data!

## ⚙️ Customization

### Change Home Address
Edit these lines in `portland_day_planner.R`:
```r
HOME_ADDRESS <- "Your address here"
HOME_LAT <- your_latitude
HOME_LNG <- your_longitude
```

### Add Activity Categories
Add to the `ACTIVITY_CATEGORIES` list in the script:
```r
"🎨 Art & Museums" = c("🎨 Art", "museum", "gallery")
```

### Adjust Distance Limits
Modify `TRANSPORT_MODES` in the script:
```r
"🚲 Biking" = 15,  # Change from 10 to 15 miles
```

## 🛠️ Dependencies

```r
install.packages(c("shiny", "leaflet", "dplyr", "readr", "stringr", "DT"))
```

## 🎉 Why This Approach?

- **No complex data processing** - works directly with your CSV exports
- **No external dependencies** - just standard R packages  
- **Automatic updates** - just replace CSV files and restart
- **One file to rule them all** - everything in a single script
- **Instant gratification** - from CSV to app in seconds!

## 📈 Future Ideas

- Weather-aware suggestions
- Time-of-day filtering
- Route planning for multi-stop days
- Export day plans

---

*Built for exploring Portland one perfect day at a time! 🌲*