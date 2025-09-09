setwd("/Users/sidneybush/Documents/GitHub/Portland_shiny_app")
sys.source("csv_saved_lists_to_kml.R", envir = globalenv())

convert_saved_csv_folder_to_kml(
  in_dir  = "Saved",
  out_dir = "out_lists_portland",
  include_files = c(
    "Portlandia Food & Drink.csv",
    "Portland stuff.csv",
    "Movie theaters PDX.csv",
    "Hikes & Trail Runs.csv"
  ),
  geocode_if_missing = TRUE,
  geocode_methods    = c("google","arcgis","osm"),  # Google auto-skips if no key
  city_hint          = "Portland, OR",
  cache_csv          = "out_lists_portland/_geocode_cache.csv",
  pause_sec          = 0.8,
  report_missing_dir = "out_lists_portland",
  url_prefer         = "viewport",   # prefer @lat,lon
  overrides          = OVERRIDES     # edit lat/lon there to force exact pins if desired
)
