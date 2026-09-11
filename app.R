# shinyapps.io deployment file
# source the main application and capture the shiny app object
app <- source("portland_day_planner.R")$value

# return the app object for shinyapps.io
app
