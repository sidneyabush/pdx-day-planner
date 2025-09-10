library(stringr)

# Test with a sample URL to see if it contains coordinates
test_url <- 'https://www.google.com/maps/place/Less+and+more+coffee/data=!4m2!3m1!1s0x54950a0557af84a5:0xb27f6a7a45bcd13a'
cat('Testing URL:', test_url, '\n')

# The current CSV URLs are abbreviated - they don't contain coordinates
cat('This URL format does NOT contain embedded coordinates\n')

# However, let me check if we can derive full URLs or get coordinates another way
# The URLs in the CSV are place identifiers that could potentially be expanded

# Let's check what the pattern would look like if we had full URLs
full_url_example <- 'https://www.google.com/maps/place/Lei+Brilla/@45.5362022,-122.7177673,13.07z/data=!4m6!3m5!1s0x5495093c45cb0515:0x9f7ff5dddef26!8m2!3d45.5353428!4d-122.7012357'
cat('\nExample of FULL URL with coordinates:', full_url_example, '\n')

# Pattern 1: !3d!4d format (from the full URL)
matches1 <- str_match(full_url_example, '!3d(-?[0-9]+\\.?[0-9]*)!4d(-?[0-9]+\\.?[0-9]*)')
cat('Pattern !3d!4d matches:', !is.na(matches1[1]), '\n')
if(!is.na(matches1[1])) {
  cat('Lat:', matches1[2], 'Lng:', matches1[3], '\n')
}

# Pattern 2: @lat,lng format (from the full URL) 
matches2 <- str_match(full_url_example, '@(-?[0-9]+\\.?[0-9]*),(-?[0-9]+\\.?[0-9]*)')
cat('Pattern @lat,lng matches:', !is.na(matches2[1]), '\n')
if(!is.na(matches2[1])) {
  cat('Lat:', matches2[2], 'Lng:', matches2[3], '\n')
}

cat('\nThe problem: CSV contains abbreviated URLs, not full URLs with coordinates\n')
cat('CSV URL format: /data=!4m2!3m1!1s... (NO COORDINATES)\n')
cat('Full URL format: /@lat,lng,zoom/data=...!3dlat!4dlng (HAS COORDINATES)\n')