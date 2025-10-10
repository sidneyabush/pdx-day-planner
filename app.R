# Shinyapps.io deployment file
# Source the main application and capture the shiny app object
app <- source("portland_day_planner.R")$value

# Return the app object for shinyapps.io
app