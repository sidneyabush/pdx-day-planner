library(rsconnect)

# Deploy the app
cat("Starting deployment to shinyapps.io...\n")

rsconnect::deployApp(
  appDir = ".",
  appName = "portland-day-planner",
  appTitle = "Portland Day-Off Planner",
  account = "sidneyabush",
  launch.browser = FALSE
)

cat("\n========================================\n")
cat("Deployment complete!\n")
cat("Your app is live at:\n")
cat("https://sidneyabush.shinyapps.io/portland-day-planner/\n")
cat("========================================\n")
