library(rsconnect)

# Set up shinyapps.io account
rsconnect::setAccountInfo(
  name='sidneyabush',
  token='502A6A14652712CF8D5CA8F0F35198E0',
  secret='AhV3KepZ6gxEUDa+4jWjOKyerjEh0b36QsqLRSPX'
)

cat("Account configured successfully!\n")
cat("Account name:", rsconnect::accounts()$name, "\n")
