data <- readRDS('data/portland_places_processed.rds')
urls <- data$url[!is.na(data$url) & data$url != ""]
cat("Sample URLs:\n")
for(i in 1:min(5, length(urls))) {
  cat(i, ":", urls[i], "\n")
}
cat("\nURL lengths:\n")
print(nchar(urls)[1:min(10, length(urls))])