# The Dream of the 90s is Alive in Portland
*A tool for exploring Portland's local spots*

My current app can be found [**here**](https://sidneyabush.shinyapps.io/portland-day-planner/), but you can also populate the app with your own saved lists from Google Maps by following the steps below.

## Workflow

1. **Export your Google Maps Saved Lists as CSV files**
2. **Place CSV files in the `Saved/` folder**
3. **Run the app:**
   ```r
   source("portland_day_planner.R")
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

---

