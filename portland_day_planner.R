suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(readr)
  library(stringr)
  library(DT)
  library(httr)
  library(jsonlite)
  library(sf)
  library(base64enc)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------- CONFIGURATION ----------------
DEFAULT_ADDRESS <- "Portland, OR"
DEFAULT_LAT <- 45.5152
DEFAULT_LNG <- -122.6784

# Activity categories that match your tags
ACTIVITY_CATEGORIES <- list(
  "☕️ Coffee & Cafes" = c("coffee", "cafe", "espresso"),
  "🍰 Sweet Treats" = c("dessert", "bakery", "sweet", "pastry", "donut", "bagel"),
  "🍃 Food & Restaurants" = c("restaurant", "food", "lunch", "dinner", "brunch", "cart", "vegan"),
  "🍸 Drinks & Bars" = c("bar", "brewery", "cocktails", "drinks", "wine", "pub", "beer"),
  "📚 Bookstores" = c("bookstore", "books", "library"),
  "💽 Music & Records" = c("music", "records", "vinyl", "record"),
  "🏷️ Thrift & Vintage" = c("vintage", "thrift", "antique", "used"),
  "📔 Stationery & Art" = c("stationery", "art supplies", "paper", "gallery", "art"),
  "🛴 Fun Stores" = c("toys", "games", "gifts", "shop"),
  "🚲 Outdoor & Sports" = c("outdoor", "sports", "hikes", "nature", "bike", "trail"),
  "🥕 Markets" = c("market", "farmers", "produce"),
  "🎭 Entertainment" = c("entertainment", "cinema", "theater", "museum", "movie"),
  "🖋️ Services & Creative" = c("tattoo", "salon", "spa")
)

# Activity modes
ACTIVITY_MODES <- list(
  "Coffee" = c("coffee", "cafe", "read", "draw", "relax", "people watch"),
  "Photography" = c("scenic", "view", "mural", "architecture", "historic", "nature", "bridge", "waterfront", "garden", "park", "museum"),
  "Walking Tour" = c("walk", "tour", "historic", "architecture"),
  "Bike Ride" = c("bike", "cycling", "trail", "path"),
  "Hiking" = c("hike", "hiking", "trail", "nature"),
  "Shopping" = c("shop", "shopping", "browse")
)

# Transportation modes  
TRANSPORT_MODES <- list(
  "🚶 Walking" = 2,      # miles
  "🚲 Biking" = 8,       # miles - one-way biking distance
  "🚗 Driving" = 30,     # miles
  "🚌 Public Transit" = 50  # miles
)

# Portland quadrant and neighborhood centers
PORTLAND_QUADRANTS <- list(
  "North"     = list(lat = 45.5758, lng = -122.6759, name = "North Portland"),
  "Northwest" = list(lat = 45.5392, lng = -122.6981, name = "Northwest Portland"),
  "Northeast" = list(lat = 45.5539, lng = -122.6398, name = "Northeast Portland"),
  "South"     = list(lat = 45.4900, lng = -122.6700, name = "South Portland"),
  "Southeast" = list(lat = 45.5059, lng = -122.6348, name = "Southeast Portland"),
  "Southwest" = list(lat = 45.4971, lng = -122.6906, name = "Southwest Portland")
)


NEIGHBORHOODS_BY_QUADRANT <- list(
  "Northwest" = list(
    "Pearl District" = list(lat = 45.5347, lng = -122.6906, name = "Pearl District"),
    "Nob Hill" = list(lat = 45.5314, lng = -122.6981, name = "Nob Hill"),
    "Old Town" = list(lat = 45.5247, lng = -122.6714, name = "Old Town"),
    "Downtown" = list(lat = 45.5152, lng = -122.6784, name = "Downtown"),
    "Kerns" = list(lat = 45.5237, lng = -122.6548, name = "Kerns"),
    "Overlook" = list(lat = 45.5459, lng = -122.6848, name = "Overlook")
  ),
  "Northeast" = list(
    "Alberta" = list(lat = 45.5587, lng = -122.6536, name = "Alberta"),
    "Fremont" = list(lat = 45.5481, lng = -122.6536, name = "Fremont"),
    "Lloyd District" = list(lat = 45.5314, lng = -122.6537, name = "Lloyd District"),
    "Beaumont-Wilshire" = list(lat = 45.5392, lng = -122.6298, name = "Beaumont-Wilshire"),
    "Rose City Park" = list(lat = 45.5426, lng = -122.6098, name = "Rose City Park"),
    "Alameda" = list(lat = 45.5459, lng = -122.6448, name = "Alameda"),
    "Concordia" = list(lat = 45.5548, lng = -122.6448, name = "Concordia"),
    "Sabin" = list(lat = 45.5548, lng = -122.6598, name = "Sabin"),
    "Boise" = list(lat = 45.5481, lng = -122.6748, name = "Boise"),
    "Humboldt" = list(lat = 45.5548, lng = -122.6748, name = "Humboldt"),
    "King" = list(lat = 45.5481, lng = -122.6648, name = "King"),
    "Eliot" = list(lat = 45.5392, lng = -122.6648, name = "Eliot"),
    "Irvington" = list(lat = 45.5481, lng = -122.6448, name = "Irvington")
  ),
  "Southeast" = list(
    "Hawthorne" = list(lat = 45.5122, lng = -122.6587, name = "Hawthorne"),
    "Division" = list(lat = 45.5059, lng = -122.6348, name = "Division"),
    "Belmont" = list(lat = 45.5165, lng = -122.6348, name = "Belmont"),
    "Richmond" = list(lat = 45.5059, lng = -122.6398, name = "Richmond"),
    "Sellwood" = list(lat = 45.4634, lng = -122.6348, name = "Sellwood"),
    "Woodstock" = list(lat = 45.4792, lng = -122.6348, name = "Woodstock"),
    "Laurelhurst" = list(lat = 45.5218, lng = -122.6298, name = "Laurelhurst"),
    "Mount Tabor" = list(lat = 45.5165, lng = -122.5998, name = "Mount Tabor"),
    "Eastmoreland" = list(lat = 45.4792, lng = -122.6348, name = "Eastmoreland"),
    "Brooklyn" = list(lat = 45.4976, lng = -122.6648, name = "Brooklyn"),
    "Hosford-Abernethy" = list(lat = 45.4976, lng = -122.6448, name = "Hosford-Abernethy"),
    "Ladd's Addition" = list(lat = 45.5059, lng = -122.6548, name = "Ladd's Addition"),
    "Buckman" = list(lat = 45.5165, lng = -122.6548, name = "Buckman"),
    "Sunnyside" = list(lat = 45.5122, lng = -122.6448, name = "Sunnyside"),
    "Creston-Kenilworth" = list(lat = 45.4792, lng = -122.6248, name = "Creston-Kenilworth"),
    "Foster-Powell" = list(lat = 45.4892, lng = -122.6148, name = "Foster-Powell"),
    "Lents" = list(lat = 45.4792, lng = -122.5748, name = "Lents"),
    "Jade District" = list(lat = 45.4634, lng = -122.5998, name = "Jade District"),
    "Powelhurst-Gilbert" = list(lat = 45.4892, lng = -122.5348, name = "Powelhurst-Gilbert")
  ),
  "Southwest" = list(
    "Hillsdale" = list(lat = 45.4792, lng = -122.6998, name = "Hillsdale"),
    "Burlingame" = list(lat = 45.4548, lng = -122.6898, name = "Burlingame"),
    "Multnomah" = list(lat = 45.4634, lng = -122.6698, name = "Multnomah"),
    "Westmoreland" = list(lat = 45.4634, lng = -122.6548, name = "Westmoreland")
  ),
  "North" = list(
    "Mississippi" = list(lat = 45.5587, lng = -122.6759, name = "Mississippi District"),
    "Kenton" = list(lat = 45.5835, lng = -122.6759, name = "Kenton"),
    "Arbor Lodge" = list(lat = 45.5748, lng = -122.6798, name = "Arbor Lodge"),
    "Piedmont" = list(lat = 45.5548, lng = -122.6898, name = "Piedmont"),
    "St. Johns" = list(lat = 45.5892, lng = -122.7598, name = "St. Johns"),
    "University Park" = list(lat = 45.5748, lng = -122.7148, name = "University Park"),
    "Portland Meadows" = list(lat = 45.6048, lng = -122.6998, name = "Portland Meadows"),
    "Cathedral Park" = list(lat = 45.5892, lng = -122.7448, name = "Cathedral Park")
  ),
  "South" = list(
    "South Portland" = list(lat = 45.4900, lng = -122.6700, name = "South Portland"),
    "Lair Hill" = list(lat = 45.5059, lng = -122.6748, name = "Lair Hill"),
    "South Waterfront" = list(lat = 45.4976, lng = -122.6714, name = "South Waterfront"),
    "Johns Landing" = list(lat = 45.4634, lng = -122.6748, name = "Johns Landing")
  )
)

# Context filters
CONTEXT_FILTERS <- list(
  "☔ Rainy Weather" = list(
    exclude_activities = c("Hiking", "Photography"),
    exclude_transport = c("🚶 Walking", "🚲 Biking"),
    prefer_venues = c("📚 Bookstores", "☕ Coffee & Cafes", "🎭 Entertainment"),
    max_distance = 15
  ),
  "🌞 Nice Weather" = list(),
  "💰 Budget Friendly" = list(
    exclude_venues = c("🍸 Drinks & Bars", "🍃 Food & Restaurants")
  ),
  "🚶 No Car" = list(
    max_distance = 5
  )
)

# ---------- Tag canonicalization & supercats ----------
canonicalize_tags <- function(x) {
  if (is.null(x)) return("")
  x <- tolower(x)
  x <- gsub("\\btheatre(s)?\\b", "theater\\1", x)
  x <- gsub("\\bmuseums\\b", "museum", x)
  x <- gsub("\\bgalleries\\b", "gallery", x)
  x <- gsub("\\btrails\\b", "trail", x)
  x <- gsub("\\bparks\\b", "park", x)
  stringr::str_squish(x)
}

derive_supercats <- function(x) {
  x <- tolower(x %||% "")
  add <- character(length(x))
  is_ent <- grepl("\\b(museum|gallery|theater|cinema|film|show|performance|movie theater)\\b", x)
  is_nat <- grepl("\\b(park|garden|trail|hike|nature|forest|arboretum|outdoor)\\b", x)
  add[is_ent & is_nat] <- "entertainment; nature"
  add[is_ent & !is_nat] <- "entertainment"
  add[!is_ent & is_nat] <- "nature"
  add
}

add_token <- function(tags, token, mask) {
  tags <- ifelse(is.na(tags), "", tags)
  need <- mask & !grepl(paste0("\\b", token, "\\b"), tags, ignore.case = TRUE)
  ifelse(need & nzchar(tags), paste(tags, token, sep = "; "),
         ifelse(need, token, tags))
}

# ---------- Backfill tags from other columns (museums/theaters/parks/trails) ----------
backfill_core_tags <- function(df) {
  if (!nrow(df)) return(df)
  if (!("tags" %in% names(df))) df$tags <- ""
  df$tags <- canonicalize_tags(df$tags)
  
  lc <- function(col) tolower(as.character(df[[col]] %||% ""))
  anytext <- paste(
    tolower(as.character(df$title %||% df$name %||% "")),
    lc("amenity"), lc("tourism"), lc("leisure"),
    lc("category"), lc("feature"), lc("note"), lc("notes"),
    lc("description"), lc("type"), lc("kinds"), lc("subcategory"),
    sep = " | "
  )
  
  looks_theater <- grepl("\\b(theater|theatre|cinema|movie|screening|playhouse|performing arts)\\b", anytext)
  looks_park    <- grepl("\\b(park|garden|arboretum|nature)\\b", anytext)
  looks_museum  <- grepl("\\b(museum)\\b", anytext)
  looks_trail   <- grepl("\\b(trail|greenway|rail trail|singletrack|loop|path)\\b", anytext)
  
  df$tags <- add_token(df$tags, "theater", looks_theater)
  df$tags <- add_token(df$tags, "park",    looks_park)
  df$tags <- add_token(df$tags, "museum",  looks_museum)
  df$tags <- add_token(df$tags, "trail",   looks_trail)
  
  df$tags <- canonicalize_tags(df$tags)
  supers <- derive_supercats(df$tags)
  df$tags <- ifelse(nzchar(supers), paste(df$tags, supers, sep = "; "), df$tags)
  df$tags <- stringr::str_squish(df$tags)
  df
}

# ---------- Address / distance helpers ----------
home_is_set <- function(addr, lat, lng) {
  !is.null(addr) && nzchar(addr) && !is.na(lat) && !is.na(lng)
}

calc_distance_miles <- function(lat1, lng1, lat2, lng2) {
  if (any(is.na(c(lat1, lng1, lat2, lng2)))) return(NA_real_)
  R <- 3959
  lat1 <- lat1 * pi/180; lat2 <- lat2 * pi/180
  dlat <- (lat2 - lat1); dlng <- (lng2 - lng1) * pi/180
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlng/2)^2
  round(R * 2 * atan2(sqrt(a), sqrt(1 - a)), 2)
}

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

is_within_radius <- function(lat1,lng1,lat2,lng2,radius_miles=1) {
  d <- calc_distance_miles(lat1,lng1,lat2,lng2)
  !is.na(d) && d <= radius_miles
}

# Single-hop "on the way back" check vs home
is_on_way_back <- function(home_lat, home_lng, v1_lat, v1_lng, cand_lat, cand_lng, detour_factor = 1.25) {
  if (any(is.na(c(home_lat, home_lng, v1_lat, v1_lng, cand_lat, cand_lng)))) return(FALSE)
  d_home_v1   <- calc_distance_miles(home_lat, home_lng, v1_lat, v1_lng)
  d_home_cand <- calc_distance_miles(home_lat, home_lng, cand_lat, cand_lng)
  d_v1_cand   <- calc_distance_miles(v1_lat, v1_lng, cand_lat, cand_lng)
  towards_home <- (!is.na(d_home_cand) && !is.na(d_home_v1) && d_home_cand <= d_home_v1)
  if (!towards_home) return(FALSE)
  baseline <- d_home_v1
  with_cand <- d_v1_cand + d_home_cand
  !any(is.na(c(baseline, with_cand))) && (with_cand <= detour_factor * baseline)
}

# Chain validator: up to 3 stops must be en route (short hops and/or trending toward home)
chain_ok <- function(home_lat, home_lng, pts, hop_radius = 1.0, detour_factor = 1.35) {
  # pts is a data.frame with columns lat, lng (order = planned order)
  if (nrow(pts) == 0) return(FALSE)
  if (nrow(pts) == 1) return(TRUE)
  
  # Hops must be close OR trending toward home
  for (i in 2:nrow(pts)) {
    p_prev <- pts[i-1,]; p_cur <- pts[i,]
    hop_ok <- is_within_radius(p_prev$lat, p_prev$lng, p_cur$lat, p_cur$lng, hop_radius)
    if (!hop_ok && !is.na(home_lat) && !is.na(home_lng)) {
      hop_ok <- is_on_way_back(home_lat, home_lng, p_prev$lat, p_prev$lng, p_cur$lat, p_cur$lng, detour_factor = 1.25)
    }
    if (!hop_ok) return(FALSE)
  }
  
  # Total path vs direct home->last
  if (!is.na(home_lat) && !is.na(home_lng)) {
    # path = home -> p1 -> p2 -> p3 -> home
    path_len <- 0
    # Start at home
    path_len <- path_len + calc_distance_miles(home_lat, home_lng, pts$lat[1], pts$lng[1])
    # Through each hop
    for (i in 2:nrow(pts)) {
      path_len <- path_len + calc_distance_miles(pts$lat[i-1], pts$lng[i-1], pts$lat[i], pts$lng[i])
    }
    # Back home
    path_len <- path_len + calc_distance_miles(pts$lat[nrow(pts)], pts$lng[nrow(pts)], home_lat, home_lng)
    
    direct <- calc_distance_miles(home_lat, home_lng, pts$lat[nrow(pts)], pts$lng[nrow(pts)]) * 2  # out-and-back baseline
    if (!is.na(path_len) && !is.na(direct)) {
      return(path_len <= detour_factor * direct)
    }
  }
  TRUE
}

# ---------- Natural-language suffix for "Surprise Me" card ----------
action_suffix_from_tags <- function(tags_text) {
  t <- tolower(tags_text %||% "")
  pick <- function(...) any(grepl(paste0("(", paste(list(...), collapse="|"), ")"), t))
  
  if (pick("mural","bridge","view","scenic","architecture","historic","photo","photos")) return("take some film photos, and soak in the atmosphere")
  if (pick("bookstore","library")) return("browse for a new book, and settle in with a coffee")
  if (pick("coffee","cafe")) return("try a new drink, and spend some time reading or drawing")
  if (pick("museum","gallery","art")) return("explore the exhibits, and do some sketching")
  if (pick("trail","hike","park","garden","arboretum","nature","forest")) return("take a peaceful walk, and connect with nature")
  if (pick("thrift","vintage","antique","market")) return("hunt for unique treasures, and enjoy the browsing")
  if (pick("record","vinyl","music")) return("dig through the records, and discover some new sounds")
  if (pick("brewery","bar","cocktail","wine","pub","beer")) return("sample their drinks, and enjoy the atmosphere")
  if (pick("bakery","dessert","sweet","donut","pastry","bagel")) return("indulge in their specialties, and savor the moment")
  if (pick("restaurant","food","lunch","dinner","brunch","cart")) return("grab a bite")
  "explore what they have to offer"
}

# ---------- Transit selection by distance ----------
transit_by_distance <- function(d_mi, context = NULL) {
  if (is.na(d_mi)) return("🚌 Public Transit")
  if (d_mi <= 2) return("🚶 Walking")
  
  # Check if driving should be suggested based on context
  allow_driving <- !is.null(context) && (
    "☔ Rainy Weather" %in% context
  )
  
  if (d_mi <= 8) return("🚲 Biking")  # One-way biking distance
  if (d_mi <= 30 && allow_driving) return("🚗 Driving")
  "🚌 Public Transit"  # Default to transit if driving not allowed
}

# Convert transit mode to action verb
transit_to_verb <- function(transit_mode) {
  if (is.na(transit_mode) || transit_mode == "") return("Go to")
  
  if (grepl("Walking", transit_mode)) return("Walk to")
  if (grepl("Biking", transit_mode)) return("Bike to") 
  if (grepl("Driving", transit_mode)) return("Drive to")
  if (grepl("Public Transit", transit_mode)) return("Take transit to")
  
  return("Go to")  # fallback
}

# Simple activity description for single stops
simple_activity_from_tags <- function(tags_text, title_text = "") {
  t <- tolower(tags_text %||% "")
  title_lower <- tolower(title_text %||% "")
  pick <- function(...) any(grepl(paste0("(", paste(list(...), collapse="|"), ")"), t))
  pick_title <- function(...) any(grepl(paste0("(", paste(list(...), collapse="|"), ")"), title_lower))
  
  # Coffee & drinks - MUST come first to prevent being caught as restaurants
  # First check title for coffee places
  if (pick_title("coffee|cafe|espresso|cappuccino|americano|mocha|macchiato|latte")) return("to grab coffee")
  # Then check tags for coffee
  if (pick("coffee","cafe","espresso","latte","cappuccino","americano","mocha","macchiato")) return(sample(c(
    "for coffee and to people watch", "to grab coffee", "for a coffee break", 
    "to relax with coffee"
  ), 1))
  
  # Food & drinks - general restaurants (after coffee detection)
  # First check title for obvious restaurant names (removed "cafe" to avoid conflict with coffee)
  if (pick_title("restaurant|kitchen|bistro|tavern|grill|diner|eatery|food|pizza|burger|sandwich|taco|ramen|pho|bagel|wrap|burrito|bowl|sub|deli|dining|brasserie|steakhouse")) return(sample(c(
    "to grab something to eat", "for a meal", "to grab a bite", "for dinner", "for lunch"
  ), 1))
  # Then check tags
  if (pick("restaurant","dining","eatery","grill","bistro","tavern","kitchen","food","meal","menu","cuisine","chef","bagel","sandwich","wrap","burrito","bowl")) return(sample(c(
    "to grab something to eat", "for a meal", "to grab a bite", "for dinner", "for lunch"
  ), 1))

  # Specialized stores - must be very specific first
  if (pick("bike","cycling","bicycle")) return(sample(c(
    "to look at bikes", "to get bike gear", "to ask about repairs", "to browse cycling equipment"
  ), 1))
  if (pick("hardware","tool","repair")) return(sample(c(
    "to find supplies", "to get what you need", "to solve your project", "to browse tools"
  ), 1))
  if (pick("game","toy","hobby")) return(sample(c(
    "to browse games", "to find something fun", "to check out the selection", "to discover new games"
  ), 1))
  if (pick("art supplies","craft","creative")) return(sample(c(
    "to get inspired", "to find art supplies", "to browse materials", "to fuel your creativity"
  ), 1))
  if (pick("tattoo","piercing")) return(sample(c(
    "to browse designs", "to check out their work", "to get inspired", "to see the art"
  ), 1))
  if (pick("salon","spa","beauty")) return(sample(c(
    "to treat yourself", "for some self-care", "to get pampered", "to relax"
  ), 1))
  
  # General shopping & retail - comes after food detection
  if (pick("store","shop","retail","market","mall","outlet")) return(sample(c(
    "to browse", "to shop", "to see what they have", "to check out their selection"
  ), 1))
  if (pick("bookstore","library")) return(sample(c(
    "to find your next read", "to browse books", "to discover new authors", "to explore the stacks", "to find a good book"
  ), 1))
  if (pick("thrift","vintage","antique")) return(sample(c(
    "to hunt for treasures", "to browse vintage finds", "to search for unique items", "to thrift shop", "to find hidden gems"
  ), 1))
  if (pick("record","vinyl","music")) return(sample(c(
    "to dig for records", "to browse vinyl", "to discover new music", "to hunt for rare finds", "to find some good tunes"
  ), 1))
  if (pick("brunch","breakfast")) return(sample(c(
    "for brunch", "for breakfast", "to start the day"
  ), 1))
  if (pick("cart","truck","stand","street food")) return(sample(c(
    "to try street food", "for a quick bite", "to grab something tasty"
  ), 1))
  
  # Drinks & nightlife - social activities
  if (pick("brewery","distillery")) return(sample(c(
    "to try the beer", "for drinks with friends", "to taste local brews", "for happy hour"
  ), 1))
  if (pick("bar","pub","cocktail","wine")) return(sample(c(
    "for drinks", "to grab a cocktail", "to try the wine", "to meet friends"
  ), 1))
  
  # Sweets & treats - indulgent activities (MUST come before restaurant check)
  if (pick("patisserie","pastry","bakery","donut","cupcake","cake","sweet","dessert")) return(sample(c(
    "for a sweet treat", "to grab something sweet", "to treat yourself", "for dessert"
  ), 1))
  if (pick("ice cream","gelato","sorbet")) return(sample(c(
    "for ice cream", "to cool down with a treat", "for something cold and sweet"
  ), 1))
  
  # Nature & outdoors - active detailed activities
  if (pick("trail","hike")) return(sample(c(
    "to go hiking", "for a hike", "to hit the trails", "to explore nature", "to get some exercise"
  ), 1))
  if (pick("park","garden","arboretum")) return(sample(c(
    "to walk around", "to enjoy nature", "for a stroll", "to relax outdoors", "to get some fresh air"
  ), 1))
  if (pick("nature","forest","outdoor")) return(sample(c(
    "to enjoy nature", "to get outside", "for fresh air", "to disconnect from the city"
  ), 1))
  
  # Cultural & entertainment - engaging activities
  if (pick("museum","gallery")) return(sample(c(
    "to explore the exhibits", "to see the art", "to learn something new", "to take in the culture", "to admire the collection"
  ), 1))
  if (pick("theater","cinema","movie","film","screening")) return(sample(c(
    "to see a movie", "to catch a film", "to watch a movie", "for a movie night"
  ), 1))
  
  
  # Sightseeing - photo and exploration activities  
  if (pick("mural","street art")) return(sample(c(
    "to see the street art", "to check out the murals", "for photos", "to admire the artwork"
  ), 1))
  if (pick("bridge","view","scenic","lookout")) return(sample(c(
    "for the view", "to see the sights", "for photos", "to take in the scenery"
  ), 1))
  if (pick("architecture","historic","landmark")) return(sample(c(
    "to see the architecture", "for the history", "to learn about the area", "to check out the landmark"
  ), 1))
  
  
  # Additional title-based fallback detection for non-restaurant venues
  if (pick_title("coffee|cafe|espresso|cappuccino|americano|mocha|macchiato|latte")) return("to grab coffee")
  if (pick_title("bar|pub|brewery|distillery|cocktail|wine")) return("for drinks") 
  if (pick_title("bakery|donut|pastry|dessert|sweet|cake|cupcake")) return("for a sweet treat")
  if (pick_title("gallery|museum")) return("to see the exhibits")
  if (pick_title("theater|cinema|movie")) return("to see a movie")
  if (pick_title("bookstore|library")) return("to browse books")
  if (pick_title("market|shop|store|boutique")) return("to browse")
  
  # Final fallback - be more specific than "check it out"
  return(sample(c(
    "to browse", "to see what they offer", "to discover something new", "to see what's there"
  ), 1))
}

# ---------- Portland time helper ----------
pdx_time_string <- function() {
  tryCatch({
    portland_time <- as.POSIXct(Sys.time(), tz = "America/Los_Angeles")
    format(portland_time, "%I:%M %p PST")
  }, error = function(e) {
    format(Sys.time(), "%I:%M %p")
  })
}

# ---------- Day planning functions ----------
generate_day_plan <- function(places, context = NULL, activities = NULL, modes = NULL, time_filter = NULL) {
  if (is.null(places) || nrow(places) == 0) return(list())
  
  # Simple plan generation - return top place with context
  top_place <- places[1, , drop = FALSE]
  
  plan <- list(
    title = if (!is.null(context)) paste("Perfect for", context) else "Great Choice",
    description = paste("Check out", top_place$title),
    type = "suggestion",
    estimated_time = "1-2 hours",
    places = top_place
  )
  
  list(plan)
}

generate_surprise_adventure <- function(available_places, time_available = NULL, context = NULL, 
                                       home_lat = NULL, home_lng = NULL, home_addr = NULL) {
  if (is.null(available_places) || nrow(available_places) == 0) return(list())
  
  # Filter to places with coordinates
  valid_places <- available_places[!is.na(available_places$lat) & !is.na(available_places$lng), , drop = FALSE]
  if (nrow(valid_places) == 0) return(list())
  
  # Determine number of locations (1-3)
  num_locations <- sample(1:3, 1)
  
  # Calculate distances for proper venue arrangement
  if (!is.null(home_lat) && !is.null(home_lng)) {
    if (!"distance_mi" %in% names(valid_places) || any(is.na(valid_places$distance_mi))) {
      valid_places$distance_mi <- mapply(function(lat, lng) {
        calc_distance_miles(home_lat, home_lng, lat, lng)
      }, valid_places$lat, valid_places$lng)
    }
  }
  
  # Categorize places by time of day preference
  categorize_venue_time <- function(tags) {
    tags_lower <- tolower(tags %||% "")
    if (grepl("coffee|cafe|bakery|breakfast|brunch", tags_lower)) return("morning")
    if (grepl("lunch|restaurant|food|museum|gallery|park|bookstore|shopping", tags_lower)) return("afternoon") 
    if (grepl("bar|brewery|cocktail|wine|pub|beer|dinner|nightlife", tags_lower)) return("evening")
    return("anytime")  # Default
  }
  
  valid_places$time_category <- sapply(valid_places$tags, categorize_venue_time)
  
  # Add venue type categorization for diversity (reuse function from guided multi-stop)
  categorize_venue_type_surprise <- function(tags, title = "") {
    tags_lower <- tolower(tags %||% "")
    title_lower <- tolower(title %||% "")
    
    # Check tags first - COFFEE MUST COME FIRST to avoid being caught as restaurant
    if (grepl("coffee|cafe|espresso|latte|cappuccino|americano|mocha|macchiato", tags_lower)) return("coffee")
    if (grepl("bakery|donut|pastry|dessert|sweet|cake|cupcake|ice cream|gelato", tags_lower)) return("dessert")
    if (grepl("restaurant|dining|eatery|kitchen|grill|bistro|tavern|food|cuisine|chef|menu|meal|eat|lunch|dinner|brunch|breakfast|subs|sandwich|pizza|burger|noodle|taco|ramen|pho", tags_lower)) return("restaurant")
    if (grepl("juice|smoothie|drink", tags_lower)) return("beverage")
    if (grepl("bar|brewery|pub|beer|cocktail|wine", tags_lower)) return("bar")
    
    # Title-based fallback for common food indicators - COFFEE MUST COME FIRST
    if (grepl("coffee|cafe|espresso|cappuccino|americano|mocha|macchiato|latte", title_lower)) return("coffee")
    if (grepl("bakery|donut|pastry|dessert|sweet|cake|cupcake|ice cream|gelato", title_lower)) return("dessert")
    if (grepl("restaurant|kitchen|bistro|tavern|grill|diner|subs|sandwich|pizza|burger|noodle|taco|ramen|pho", title_lower)) return("restaurant") 
    if (grepl("juice|smoothie", title_lower)) return("beverage")
    if (grepl("bar|pub|brewery|distillery", title_lower)) return("bar")
    
    return("other")
  }
  
  valid_places$venue_type <- mapply(categorize_venue_type_surprise, valid_places$tags, valid_places$title, SIMPLIFY = TRUE)
  
  # Select places with proper time progression
  selected_places <- if (num_locations == 1) {
    # Single location: any time is fine
    valid_places[sample(nrow(valid_places), 1), , drop = FALSE]
  } else if (num_locations == 2) {
    # Two locations: try to get morning/afternoon first, then evening
    morning_afternoon <- valid_places[valid_places$time_category %in% c("morning", "afternoon", "anytime"), , drop = FALSE]
    evening <- valid_places[valid_places$time_category %in% c("evening", "anytime"), , drop = FALSE]
    
    if (nrow(morning_afternoon) > 0 && nrow(evening) > 0) {
      first <- morning_afternoon[sample(nrow(morning_afternoon), 1), , drop = FALSE]
      # Exclude the first place and same venue type from evening options
      evening_filtered <- evening[evening$id != first$id & evening$venue_type != first$venue_type, , drop = FALSE]
      if (nrow(evening_filtered) > 0) {
        second <- evening_filtered[sample(nrow(evening_filtered), 1), , drop = FALSE]
        rbind(first, second)
      } else {
        # Fallback: at least avoid same ID, even if venue types match
        evening_filtered_id_only <- evening[evening$id != first$id, , drop = FALSE]
        if (nrow(evening_filtered_id_only) > 0) {
          second <- evening_filtered_id_only[sample(nrow(evening_filtered_id_only), 1), , drop = FALSE]
          rbind(first, second)
        } else {
          valid_places[sample(nrow(valid_places), 2), , drop = FALSE]
        }
      }
    } else {
      valid_places[sample(nrow(valid_places), min(2, nrow(valid_places))), , drop = FALSE]
    }
  } else {
    # Three locations: morning -> afternoon -> evening progression
    morning <- valid_places[valid_places$time_category %in% c("morning", "anytime"), , drop = FALSE]
    afternoon <- valid_places[valid_places$time_category %in% c("afternoon", "anytime"), , drop = FALSE] 
    evening <- valid_places[valid_places$time_category %in% c("evening", "anytime"), , drop = FALSE]
    
    selected <- data.frame()
    used_ids <- character(0)
    used_venue_types <- character(0)
    
    # Try to get one from each time period with venue type diversity
    if (nrow(morning) > 0) {
      first <- morning[sample(nrow(morning), 1), , drop = FALSE]
      selected <- rbind(selected, first)
      used_ids <- c(used_ids, first$id)
      used_venue_types <- c(used_venue_types, first$venue_type)
    }
    
    if (nrow(afternoon) > 0) {
      # Filter by both ID and venue type
      afternoon_filtered <- afternoon[!afternoon$id %in% used_ids & !afternoon$venue_type %in% used_venue_types, , drop = FALSE]
      if (nrow(afternoon_filtered) > 0) {
        second <- afternoon_filtered[sample(nrow(afternoon_filtered), 1), , drop = FALSE]
        selected <- rbind(selected, second)
        used_ids <- c(used_ids, second$id)
        used_venue_types <- c(used_venue_types, second$venue_type)
      } else {
        # Fallback: at least avoid same ID
        afternoon_filtered_id_only <- afternoon[!afternoon$id %in% used_ids, , drop = FALSE]
        if (nrow(afternoon_filtered_id_only) > 0) {
          second <- afternoon_filtered_id_only[sample(nrow(afternoon_filtered_id_only), 1), , drop = FALSE]
          selected <- rbind(selected, second)
          used_ids <- c(used_ids, second$id)
          used_venue_types <- c(used_venue_types, second$venue_type)
        }
      }
    }
    
    if (nrow(evening) > 0) {
      # Filter by both ID and venue type
      evening_filtered <- evening[!evening$id %in% used_ids & !evening$venue_type %in% used_venue_types, , drop = FALSE]
      if (nrow(evening_filtered) > 0) {
        third <- evening_filtered[sample(nrow(evening_filtered), 1), , drop = FALSE]
        selected <- rbind(selected, third)
      } else {
        # Fallback: at least avoid same ID
        evening_filtered_id_only <- evening[!evening$id %in% used_ids, , drop = FALSE]
        if (nrow(evening_filtered_id_only) > 0) {
          third <- evening_filtered_id_only[sample(nrow(evening_filtered_id_only), 1), , drop = FALSE]
          selected <- rbind(selected, third)
        }
      }
    }
    
    # If we didn't get 3 places, fill with random ones
    if (nrow(selected) < 3) {
      remaining <- valid_places[!valid_places$id %in% used_ids, , drop = FALSE]
      if (nrow(remaining) > 0) {
        needed <- min(3 - nrow(selected), nrow(remaining))
        additional <- remaining[sample(nrow(remaining), needed), , drop = FALSE]
        selected <- rbind(selected, additional)
      }
    }
    
    selected
  }
  
  # Arrange venues by distance for logical journey flow (surprise adventures)
  # Close venues should be first or last, not in the middle
  if (num_locations > 2 && !is.null(home_lat) && !is.null(home_lng) && nrow(selected_places) > 2) {
    # Ensure distances are calculated
    selected_places$distance_mi <- mapply(function(lat, lng) {
      calc_distance_miles(home_lat, home_lng, lat, lng)
    }, selected_places$lat, selected_places$lng)
    
    # Sort by distance and rearrange for optimal journey
    selected_places <- selected_places[order(selected_places$distance_mi, na.last = TRUE), , drop = FALSE]
    
    # For 3+ stops: keep closest first, furthest in middle, second closest last
    # This creates a logical "out and back" pattern
    if (nrow(selected_places) >= 3) {
      reordered <- selected_places[1, , drop = FALSE]  # Start with closest
      
      # Add middle venues (furthest ones)
      middle_count <- nrow(selected_places) - 2
      if (middle_count > 0) {
        middle_venues <- selected_places[(2:(1+middle_count)), , drop = FALSE]
        # Reverse middle venues so furthest is first in the middle section
        middle_venues <- middle_venues[nrow(middle_venues):1, , drop = FALSE]
        reordered <- rbind(reordered, middle_venues)
      }
      
      # End with second closest (if different from first)
      if (nrow(selected_places) > 1) {
        last_venue <- selected_places[nrow(selected_places), , drop = FALSE]
        reordered <- rbind(reordered, last_venue)
      }
      
      selected_places <- reordered
    }
  }
  
  # Arrange venues by distance for logical journey flow
  # Close venues should be first or last, not in the middle
  if (!is.null(home_lat) && !is.null(home_lng) && nrow(selected_places) > 2) {
    # Ensure distances are calculated
    selected_places$distance_mi <- mapply(function(lat, lng) {
      calc_distance_miles(home_lat, home_lng, lat, lng)
    }, selected_places$lat, selected_places$lng)
    
    # Sort by distance and rearrange for optimal journey
    selected_places <- selected_places[order(selected_places$distance_mi, na.last = TRUE), , drop = FALSE]
    
    # For 3+ stops: keep closest first, furthest in middle, second closest last
    # This creates a logical "out and back" pattern
    if (nrow(selected_places) >= 3) {
      reordered <- selected_places[1, , drop = FALSE]  # Start with closest
      
      # Add middle venues (furthest ones)
      middle_count <- nrow(selected_places) - 2
      if (middle_count > 0) {
        middle_venues <- selected_places[(2:(1+middle_count)), , drop = FALSE]
        # Reverse middle venues so furthest is first in the middle section
        middle_venues <- middle_venues[nrow(middle_venues):1, , drop = FALSE]
        reordered <- rbind(reordered, middle_venues)
      }
      
      # End with second closest (if different from first)
      if (nrow(selected_places) > 1) {
        last_venue <- selected_places[nrow(selected_places), , drop = FALSE]
        reordered <- rbind(reordered, last_venue)
      }
      
      selected_places <- reordered
    }
  }
  
  # Calculate distances and determine overall transit mode
  if (!is.null(home_lat) && !is.null(home_lng) && !is.na(home_lat) && !is.na(home_lng)) {
    selected_places$distance_mi <- mapply(function(lat, lng) {
      calc_distance_miles(home_lat, home_lng, lat, lng)
    }, selected_places$lat, selected_places$lng)
    
    # Use the furthest distance to determine transit mode
    max_distance <- max(selected_places$distance_mi, na.rm = TRUE)
    if (!is.na(max_distance)) {
      main_transit <- transit_by_distance(max_distance, context)
    } else {
      main_transit <- "🚌 Public Transit"
    }
  } else {
    main_transit <- "🚌 Public Transit"
  }
  
  # Generate activities based on place tags using the improved function
  activities <- c()
  for (i in 1:nrow(selected_places)) {
    place <- selected_places[i, ]
    activity <- simple_activity_from_tags(place$tags, place$title)
    activities <- c(activities, activity)
  }
  
  # Create adventure description using transit verb
  transit_verb <- tolower(transit_to_verb(main_transit))
  
  if (num_locations == 1) {
    description <- paste(stringr::str_to_sentence(paste(transit_verb, selected_places$title[1], activities[1])))
  } else if (num_locations == 2) {
    description <- paste(stringr::str_to_sentence(paste(transit_verb, selected_places$title[1], activities[1])), 
                        "then head to", selected_places$title[2], activities[2])
  } else {
    description <- paste(stringr::str_to_sentence(paste(transit_verb, selected_places$title[1], activities[1])),
                        "then", selected_places$title[2], activities[2], 
                        "and finish at", selected_places$title[3], activities[3])
  }
  
  # Estimate time based on number of locations
  time_estimate <- switch(as.character(num_locations),
                         "1" = "1-2 hours",
                         "2" = "2-3 hours", 
                         "3" = "3-4 hours")
  
  adventure <- list(
    title = paste(num_locations, "Stop Adventure"),
    description = description,
    type = "adventure",
    estimated_time = time_estimate,
    transit = main_transit,
    neighborhood = selected_places$neighborhood[1] %||% "Portland",
    places = selected_places,
    activities = activities,
    num_locations = num_locations
  )
  
  list(adventure)
}

# Generate activity suggestions based on place tags
generate_activity_from_tags <- function(tags) {
  if (is.na(tags) || tags == "") return("explore")
  
  tags_lower <- tolower(tags)
  
  # Coffee/cafe activities
  if (grepl("coffee|cafe", tags_lower)) {
    return(sample(c("grab a coffee", "draw your surroundings", "read a book", "people watch"), 1))
  }
  
  # Food activities  
  if (grepl("restaurant|food|lunch|dinner|brunch", tags_lower)) {
    return(sample(c("grab a bite", "try the local cuisine", "enjoy a meal", "sample the menu"), 1))
  }
  
  # Drinks/bar activities
  if (grepl("bar|brewery|cocktail|drinks", tags_lower)) {
    return(sample(c("grab a drink", "try a local beer", "enjoy happy hour", "sample cocktails"), 1))
  }
  
  # Shopping activities
  if (grepl("bookstore|books|book shop", tags_lower)) {
    return(sample(c("browse for books", "find your next read", "explore the stacks", "discover new authors"), 1))
  }
  
  if (grepl("vintage|thrift|antique", tags_lower)) {
    return(sample(c("hunt for treasures", "browse vintage finds", "search for unique items", "thrift shop"), 1))
  }
  
  if (grepl("record|music|vinyl", tags_lower)) {
    return(sample(c("dig for records", "browse vinyl", "discover new music", "hunt for rare finds"), 1))
  }
  
  # Entertainment activities
  if (grepl("museum|gallery|art", tags_lower)) {
    return(sample(c("explore the exhibits", "admire the art", "learn something new", "take in the culture"), 1))
  }
  
  if (grepl("theater|cinema|movie", tags_lower)) {
    return(sample(c("catch a show", "watch a film", "enjoy the performance", "see what's playing"), 1))
  }
  
  # Outdoor activities
  if (grepl("park|nature|trail|hike", tags_lower)) {
    return(sample(c("take a walk", "enjoy nature", "get some fresh air", "explore the grounds"), 1))
  }
  
  # Default activities
  return(sample(c("explore", "try something new", "discover something new"), 1))
}

# Generate contextually appropriate activities for multi-stop plans
generate_contextual_activities <- function(places) {
  if (is.null(places) || nrow(places) == 0) return(character(0))
  
  activities <- character(nrow(places))
  
  # Define activity types by venue characteristics
  quiet_activities <- c("to read", "to work", "to catch up with a friend", "to relax")
  social_activities <- c("to people watch", "to draw your surroundings", "to take photos", "to soak in the atmosphere")
  focused_activities <- c("to browse", "to hunt for treasures", "to explore the collection", "to discover something new")
  
  for (i in seq_len(nrow(places))) {
    tags_lower <- tolower(places$tags[i] %||% "")
    
    # Bakeries and patisseries FIRST (before restaurants) - sweet treats
    if (grepl("patisserie|pastry|bakery|donut|cupcake|sweet|dessert", tags_lower)) {
      activities[i] <- sample(c("for a sweet treat", "to treat yourself", "for something sweet"), 1)
    }
    # Coffee shops - specific coffee activities
    else if (grepl("coffee|cafe|espresso|latte", tags_lower)) {
      activities[i] <- sample(c("for coffee", "to grab coffee", "for a coffee break", "to draw your surroundings"), 1)
    }
    # Libraries - quiet, focused activities
    else if (grepl("library", tags_lower)) {
      activities[i] <- sample(quiet_activities, 1)
    }
    # Bookstores, record shops - focused browsing activities  
    else if (grepl("bookstore|book|record|vinyl|music", tags_lower)) {
      activities[i] <- sample(focused_activities, 1)
    }
    # Museums, galleries - contemplative activities
    else if (grepl("museum|gallery", tags_lower)) {
      activities[i] <- sample(c("to explore the exhibits", "to see the art", "to learn something new", "to admire the collection"), 1)
    }
    # Cinemas and theaters - entertainment
    else if (grepl("cinema|theater|theatre|movie|film", tags_lower)) {
      activities[i] <- sample(c("to see a movie", "to catch a film", "to watch a movie"), 1)
    }
    # Restaurants - food activities (more specific pattern)
    else if (grepl("restaurant|dining|eatery|grill|bistro|tavern", tags_lower)) {
      activities[i] <- sample(c("to grab a bite"), 1)
    }
    # Bars, breweries - drink activities  
    else if (grepl("bar|brewery|pub|beer|cocktail|wine", tags_lower)) {
      activities[i] <- sample(c("for drinks"), 1)
    }
    # Markets, shops - browsing activities
    else if (grepl("market|shop|store|thrift|vintage", tags_lower)) {
      activities[i] <- sample(social_activities, 1)
    }
    # Parks, outdoor spaces - nature activities
    else if (grepl("park|garden|nature|outdoor|trail", tags_lower)) {
      activities[i] <- sample(c("to walk around", "to enjoy nature", "for a stroll", "to relax outdoors"), 1)
    }
    # Fallback to simple activity assignment
    else {
      activities[i] <- simple_activity_from_tags(places$tags[i], places$title[i])
    }
  }
  
  return(activities)
}

# Generate guided multi-stop plans (similar to surprise adventure but with user filters)
generate_guided_multi_stop <- function(available_places, num_stops, context = NULL, 
                                      home_lat = NULL, home_lng = NULL, home_addr = NULL) {
  if (is.null(available_places) || nrow(available_places) == 0 || num_stops < 2) return(NULL)
  
  # Filter to places with coordinates
  valid_places <- available_places[!is.na(available_places$lat) & !is.na(available_places$lng), , drop = FALSE]
  if (nrow(valid_places) < num_stops) return(NULL)
  
  # Calculate distances for proper venue arrangement
  if (!is.null(home_lat) && !is.null(home_lng)) {
    if (!"distance_mi" %in% names(valid_places) || any(is.na(valid_places$distance_mi))) {
      valid_places$distance_mi <- mapply(function(lat, lng) {
        calc_distance_miles(home_lat, home_lng, lat, lng)
      }, valid_places$lat, valid_places$lng)
    }
  }
  
  # Use the same time categorization as surprise adventures
  categorize_venue_time <- function(tags) {
    tags_lower <- tolower(tags %||% "")
    if (grepl("coffee|cafe|bakery|breakfast|brunch", tags_lower)) return("morning")
    if (grepl("lunch|restaurant|food|museum|gallery|park|bookstore|shopping", tags_lower)) return("afternoon") 
    if (grepl("bar|brewery|cocktail|wine|pub|beer|dinner|nightlife", tags_lower)) return("evening")
    return("anytime")
  }
  
  valid_places$time_category <- sapply(valid_places$tags, categorize_venue_time)
  
  # Add venue type categorization for diversity enforcement (expanded patterns + title fallback)
  categorize_venue_type <- function(tags, title = "") {
    tags_lower <- tolower(tags %||% "")
    title_lower <- tolower(title %||% "")
    
    # Check tags first - COFFEE MUST COME FIRST to avoid being caught as restaurant
    if (grepl("coffee|cafe|espresso|latte|cappuccino|americano|mocha|macchiato", tags_lower)) return("coffee")
    if (grepl("bakery|donut|pastry|dessert|sweet|cake|cupcake|ice cream|gelato", tags_lower)) return("dessert")
    if (grepl("restaurant|dining|eatery|kitchen|grill|bistro|tavern|food|cuisine|chef|menu|meal|eat|lunch|dinner|brunch|breakfast|subs|sandwich|pizza|burger|noodle|taco|ramen|pho", tags_lower)) return("restaurant")
    if (grepl("juice|smoothie|drink", tags_lower)) return("beverage")
    if (grepl("bar|brewery|pub|beer|cocktail|wine", tags_lower)) return("bar")
    
    # Title-based fallback for common food indicators - COFFEE MUST COME FIRST
    if (grepl("coffee|cafe|espresso|cappuccino|americano|mocha|macchiato|latte", title_lower)) return("coffee")
    if (grepl("bakery|donut|pastry|dessert|sweet|cake|cupcake|ice cream|gelato", title_lower)) return("dessert")
    if (grepl("restaurant|kitchen|bistro|tavern|grill|diner|subs|sandwich|pizza|burger|noodle|taco|ramen|pho", title_lower)) return("restaurant") 
    if (grepl("juice|smoothie", title_lower)) return("beverage")
    if (grepl("bar|pub|brewery|distillery", title_lower)) return("bar")
    
    return("other")
  }
  
  valid_places$venue_type <- mapply(categorize_venue_type, valid_places$tags, valid_places$title, SIMPLIFY = TRUE)
  
  # Select places with proper time progression AND venue diversity
  selected_places <- if (num_stops == 2) {
    # Two locations: morning/afternoon first, then evening
    morning_afternoon <- valid_places[valid_places$time_category %in% c("morning", "afternoon", "anytime"), , drop = FALSE]
    evening <- valid_places[valid_places$time_category %in% c("evening", "anytime"), , drop = FALSE]
    
    if (nrow(morning_afternoon) > 0 && nrow(evening) > 0) {
      first <- morning_afternoon[sample(nrow(morning_afternoon), 1), , drop = FALSE]
      # Filter by ID and venue type to avoid duplicates
      evening_filtered <- evening[evening$id != first$id & evening$venue_type != first$venue_type, , drop = FALSE]
      if (nrow(evening_filtered) > 0) {
        second <- evening_filtered[sample(nrow(evening_filtered), 1), , drop = FALSE]
        rbind(first, second)
      } else {
        # Fallback: at least avoid same ID, even if venue types match
        evening_filtered_id_only <- evening[evening$id != first$id, , drop = FALSE]
        if (nrow(evening_filtered_id_only) > 0) {
          second <- evening_filtered_id_only[sample(nrow(evening_filtered_id_only), 1), , drop = FALSE]
          rbind(first, second)
        } else {
          valid_places[sample(nrow(valid_places), min(2, nrow(valid_places))), , drop = FALSE]
        }
      }
    } else {
      valid_places[sample(nrow(valid_places), min(2, nrow(valid_places))), , drop = FALSE]
    }
  } else {
    # Three locations: morning -> afternoon -> evening progression
    morning <- valid_places[valid_places$time_category %in% c("morning", "anytime"), , drop = FALSE]
    afternoon <- valid_places[valid_places$time_category %in% c("afternoon", "anytime"), , drop = FALSE]
    evening <- valid_places[valid_places$time_category %in% c("evening", "anytime"), , drop = FALSE]
    
    selected <- data.frame()
    used_ids <- character(0)
    used_venue_types <- character(0)
    
    # Try to get one from each category with venue type diversity
    if (nrow(morning) > 0) {
      first <- morning[sample(nrow(morning), 1), , drop = FALSE]
      selected <- rbind(selected, first)
      used_ids <- c(used_ids, first$id)
      used_venue_types <- c(used_venue_types, first$venue_type)
    }
    
    if (nrow(afternoon) > 0) {
      # Filter by both ID and venue type
      afternoon_filtered <- afternoon[!afternoon$id %in% used_ids & !afternoon$venue_type %in% used_venue_types, , drop = FALSE]
      if (nrow(afternoon_filtered) > 0) {
        second <- afternoon_filtered[sample(nrow(afternoon_filtered), 1), , drop = FALSE]
        selected <- rbind(selected, second)
        used_ids <- c(used_ids, second$id)
        used_venue_types <- c(used_venue_types, second$venue_type)
      } else {
        # Fallback: at least avoid same ID
        afternoon_filtered_id_only <- afternoon[!afternoon$id %in% used_ids, , drop = FALSE]
        if (nrow(afternoon_filtered_id_only) > 0) {
          second <- afternoon_filtered_id_only[sample(nrow(afternoon_filtered_id_only), 1), , drop = FALSE]
          selected <- rbind(selected, second)
          used_ids <- c(used_ids, second$id)
          used_venue_types <- c(used_venue_types, second$venue_type)
        }
      }
    }
    
    if (nrow(evening) > 0) {
      # Filter by both ID and venue type
      evening_filtered <- evening[!evening$id %in% used_ids & !evening$venue_type %in% used_venue_types, , drop = FALSE]
      if (nrow(evening_filtered) > 0) {
        third <- evening_filtered[sample(nrow(evening_filtered), 1), , drop = FALSE]
        selected <- rbind(selected, third)
        used_ids <- c(used_ids, third$id)
        used_venue_types <- c(used_venue_types, third$venue_type)
      } else {
        # Fallback: at least avoid same ID
        evening_filtered_id_only <- evening[!evening$id %in% used_ids, , drop = FALSE]
        if (nrow(evening_filtered_id_only) > 0) {
          third <- evening_filtered_id_only[sample(nrow(evening_filtered_id_only), 1), , drop = FALSE]
          selected <- rbind(selected, third)
          used_ids <- c(used_ids, third$id)
          used_venue_types <- c(used_venue_types, third$venue_type)
        }
      }
    }
    
    # Fill remaining slots if needed with venue diversity
    if (nrow(selected) < num_stops) {
      # First try to avoid duplicate venue types
      remaining_diverse <- valid_places[!valid_places$id %in% used_ids & !valid_places$venue_type %in% used_venue_types, , drop = FALSE]
      if (nrow(remaining_diverse) > 0) {
        needed <- min(num_stops - nrow(selected), nrow(remaining_diverse))
        additional <- remaining_diverse[sample(nrow(remaining_diverse), needed), , drop = FALSE]
        selected <- rbind(selected, additional)
        used_ids <- c(used_ids, additional$id)
        used_venue_types <- c(used_venue_types, additional$venue_type)
      }
      
      # If still need more, fall back to just avoiding same ID
      if (nrow(selected) < num_stops) {
        remaining_id_only <- valid_places[!valid_places$id %in% used_ids, , drop = FALSE]
        if (nrow(remaining_id_only) > 0) {
          needed <- min(num_stops - nrow(selected), nrow(remaining_id_only))
          additional <- remaining_id_only[sample(nrow(remaining_id_only), needed), , drop = FALSE]
          selected <- rbind(selected, additional)
        }
      }
    }
    
    selected
  }
  
  # Calculate distances and determine transit mode
  if (!is.null(home_lat) && !is.null(home_lng) && !is.na(home_lat) && !is.na(home_lng)) {
    selected_places$distance_mi <- mapply(function(lat, lng) {
      calc_distance_miles(home_lat, home_lng, lat, lng)
    }, selected_places$lat, selected_places$lng)
    
    max_distance <- max(selected_places$distance_mi, na.rm = TRUE)
    main_transit <- if (!is.na(max_distance)) transit_by_distance(max_distance, context) else "🚌 Public Transit"
  } else {
    main_transit <- "🚌 Public Transit"
  }
  
  # Generate contextually appropriate activities for each stop
  activities <- generate_contextual_activities(selected_places)
  
  # Create guided plan description
  transit_verb <- tolower(transit_to_verb(main_transit))
  
  if (num_stops == 2) {
    description <- paste(stringr::str_to_sentence(paste(transit_verb, selected_places$title[1], activities[1])), 
                        "then head to", selected_places$title[2], activities[2])
  } else {
    description <- paste(stringr::str_to_sentence(paste(transit_verb, selected_places$title[1], activities[1])),
                        "then", selected_places$title[2], activities[2], 
                        "and finish at", selected_places$title[3], activities[3])
  }
  
  # Return adventure structure
  list(
    title = paste(num_stops, "Stop Guided Plan"),
    description = description,
    type = "guided_plan",
    estimated_time = paste(num_stops * 90, "minutes"),
    transit = main_transit,
    places = selected_places,
    activities = activities,
    num_locations = num_stops,
    neighborhood = paste(unique(selected_places$neighborhood[!is.na(selected_places$neighborhood)]), collapse = ", ")
  )
}

# ======= DATA & MAP SETUP (paste this after your helpers, before ui <- fluidPage) =======

# --- App dir + header image  --------------------------
get_app_dir <- function() {
  tryCatch(normalizePath(dirname(sys.frame(1)$ofile)),
           error = function(e) normalizePath(getwd()))
}
APP_DIR <- get_app_dir()

HEADER_IMG <- tryCatch({
  img_path <- file.path(APP_DIR, "www:", "pdx_header.png")  # <- no colon
  if (file.exists(img_path)) base64enc::dataURI(file = img_path, mime = "image/png") else NULL
}, error = function(e) NULL)


# --- Load processed places --------------------------------------------------
processed_file <- "data/portland_places_processed.rds"
if (!file.exists(processed_file)) stop("❌ Missing processed data. Run process_data.R first.")
places <- readRDS(processed_file)
if (!is.data.frame(places) || nrow(places) == 0) stop("❌ Invalid processed data.")

# ensure lat/lng numeric and drop junk titles
if ("lat" %in% names(places)) places$lat <- suppressWarnings(as.numeric(places$lat))
if ("lng" %in% names(places)) places$lng <- suppressWarnings(as.numeric(places$lng))
if ("title" %in% names(places)) {
  places <- places[!grepl("^Unnamed place", places$title, ignore.case = TRUE), ]
  places <- places[!is.na(places$title) & nzchar(trimws(places$title)), ]
}


# --- Polygon helpers + load neighborhood/sextant boundaries ----------------
load_map_file <- function(paths) { for (p in paths) if (file.exists(p)) return(p); NULL }
safe_read_sf <- function(path) {
  if (is.null(path)) return(NULL)
  tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NULL)
}
pick_name_col <- function(sfobj, cands) {
  cands <- cands[cands %in% names(sfobj)]
  if (length(cands)) cands[[1]] else NULL
}

sextants_path <- load_map_file(c(
  "archive/Portland_Administrative_Sextants.geojson",
  "data/Portland_Administrative_Sextants.geojson"
))
sections_boundaries <- safe_read_sf(sextants_path)
SEC_NAME_COL <- if (!is.null(sections_boundaries)) pick_name_col(sections_boundaries,
                                                                 c("Sextant","SEXTANT","PREFIX","NAME")) else NULL

neighborhood_path <- load_map_file(c(
  "archive/Neighborhood_Boundaries.geojson",
  "archive/neighborhoods.geojson",
  "data/Neighborhood_Boundaries.geojson"
))
neighborhood_boundaries <- safe_read_sf(neighborhood_path)
NEI_NAME_COL <- if (!is.null(neighborhood_boundaries)) pick_name_col(neighborhood_boundaries,
                                                                     c("MAPLABEL","NAME","Label","Neighborhood","NEIGHBORHD","neigh","label")) else NULL

# --- Sextant name normalization & small label helpers ----------------------
normalize_sextant <- function(x) {
  x <- trimws(as.character(x)); if (!length(x)) return(character(0))
  alias <- c(
    "SW"="Southwest","S.W."="Southwest","South West"="Southwest",
    "SE"="Southeast","S.E."="Southeast","South East"="Southeast",
    "NW"="Northwest","N.W."="Northwest","North West"="Northwest",
    "NE"="Northeast","N.E."="Northeast","North East"="Northeast",
    "N" ="North",
    "S"="South"
  )
  out <- ifelse(!is.na(alias[x]), alias[x], x)
  proper <- c("North","South","Northeast","Northwest","Southeast","Southwest")
  out <- ifelse(out %in% proper, out, tools::toTitleCase(gsub("\\s+"," ", out)))
  out
}

neighborhood_to_quadrant <- function(neigh) {
  if (is.null(neigh) || !nzchar(neigh)) return(NA_character_)
  tg <- tolower(trimws(neigh))
  for (q in names(NEIGHBORHOODS_BY_QUADRANT)) {
    if (tg %in% tolower(names(NEIGHBORHOODS_BY_QUADRANT[[q]]))) return(q)
  }
  NA_character_
}

neigh_display_vec <- function(geo, txt) ifelse(!is.na(geo) & nzchar(geo), geo, txt)
label_with_neigh <- function(title, neigh) {
  if (!is.na(neigh) && nzchar(neigh)) paste0(title, " (", neigh, ")") else title
}

# --- Point-in-polygon enrichment ------------------------------------------
geo_enrich_places <- function(df, neighborhoods_sf, nei_col, sextants_sf, sec_col) {
  df$neighborhood_geo <- NA_character_
  df$section_geo <- NA_character_
  if (nrow(df) == 0) return(df)
  pts <- tryCatch(sf::st_as_sf(df, coords = c("lng","lat"), crs = 4326, remove = FALSE),
                  error = function(e) NULL)
  if (!is.null(pts) && !is.null(neighborhoods_sf) && !is.null(nei_col)) {
    nb <- neighborhoods_sf
    if (!is.na(sf::st_crs(nb)) && sf::st_crs(nb)$epsg != 4326) nb <- sf::st_transform(nb, 4326)
    joined_nb <- suppressWarnings(sf::st_join(pts, nb[, nei_col, drop = FALSE], join = sf::st_within, left = TRUE))
    df$neighborhood_geo <- as.character(joined_nb[[nei_col]])
  }
  if (!is.null(pts) && !is.null(sextants_sf) && !is.null(sec_col)) {
    sx <- sextants_sf
    if (!is.na(sf::st_crs(sx)) && sf::st_crs(sx)$epsg != 4326) sx <- sf::st_transform(sx, 4326)
    joined_sx <- suppressWarnings(sf::st_join(pts, sx[, sec_col, drop = FALSE], join = sf::st_within, left = TRUE))
    raw_sec <- as.character(joined_sx[[sec_col]])
    df$section_geo <- normalize_sextant(raw_sec)
  }
  df
}

# apply polygon labels
places <- geo_enrich_places(places, neighborhood_boundaries, NEI_NAME_COL,
                            sections_boundaries, SEC_NAME_COL)

# --- Backfill tags (museum/theater/park/trail) + supercats ----------------
places <- backfill_core_tags(places)

# --- Apply automatic list-specific tags (AFTER backfill) -------------------
apply_list_specific_tags <- function(df) {
  if (!("source_list" %in% names(df)) || !("tags" %in% names(df))) return(df)
  
  # Initialize tags column if missing
  df$tags <- ifelse(is.na(df$tags), "", as.character(df$tags))
  
  # Helper function to add tag if not already present (case insensitive check)
  add_tag_if_missing <- function(existing_tags, new_tag) {
    if (is.na(existing_tags) || existing_tags == "") return(new_tag)
    
    # Split existing tags and normalize
    existing_split <- trimws(strsplit(existing_tags, ";")[[1]])
    existing_lower <- tolower(existing_split)
    
    # Check if new tag already exists (case insensitive)
    if (!tolower(new_tag) %in% existing_lower) {
      return(paste(existing_tags, new_tag, sep = "; "))
    }
    
    return(existing_tags)
  }
  
  # Helper function to standardize duplicate tags (e.g., "Trail" and "trail")
  standardize_tags <- function(tags_string) {
    if (is.na(tags_string) || tags_string == "") return(tags_string)
    
    # Split, trim, and remove empty tags
    tags_split <- trimws(strsplit(tags_string, ";")[[1]])
    tags_split <- tags_split[tags_split != ""]
    
    # Create a standardized version lookup
    standardized <- character(length(tags_split))
    
    for (i in seq_along(tags_split)) {
      tag <- tags_split[i]
      # Standardize all tags to lowercase
      if (tolower(tag) %in% c("trail", "trails")) {
        standardized[i] <- "trail"
      } else if (tolower(tag) %in% c("park", "parks")) {
        standardized[i] <- "park"
      } else if (tolower(tag) %in% c("museum", "museums")) {
        standardized[i] <- "museum"
      } else if (tolower(tag) %in% c("nature")) {
        standardized[i] <- "nature"
      } else {
        # Convert all other tags to lowercase as well
        standardized[i] <- tolower(tag)
      }
    }
    
    # Remove duplicates (case insensitive) while preserving order
    unique_tags <- character(0)
    for (tag in standardized) {
      if (!tolower(tag) %in% tolower(unique_tags)) {
        unique_tags <- c(unique_tags, tag)
      }
    }
    
    return(paste(unique_tags, collapse = "; "))
  }
  
  # Outdoor museums list (places that should get nature tag)
  outdoor_museums <- c(
    "pittock mansion",
    "hoyt arboretum", 
    "japanese garden",
    "portland japanese garden",
    "lan su chinese garden",
    "world forestry center",
    "oregon rail heritage center"
  )
  
  # Add "museum" tag for Portland Museums list
  museum_mask <- grepl("Portland.*Museums|Museums.*Portland", df$source_list, ignore.case = TRUE)
  if (any(museum_mask)) {
    for (i in which(museum_mask)) {
      df$tags[i] <- add_tag_if_missing(df$tags[i], "museum")
      
      # Add nature tag for outdoor museums
      title_lower <- tolower(df$title[i])
      if (any(sapply(outdoor_museums, function(om) grepl(om, title_lower, fixed = TRUE)))) {
        df$tags[i] <- add_tag_if_missing(df$tags[i], "nature")
      }
    }
  }
  
  # Add "trail" and "nature" tags for Hikes & Trail Runs list
  trail_mask <- grepl("Hikes.*Trail.*Runs|Trail.*Runs", df$source_list, ignore.case = TRUE)
  if (any(trail_mask)) {
    for (i in which(trail_mask)) {
      df$tags[i] <- add_tag_if_missing(df$tags[i], "trail")
      df$tags[i] <- add_tag_if_missing(df$tags[i], "nature")
    }
  }
  
  # Add "park" and "nature" tags for Portland Parks list
  park_mask <- grepl("Portland.*Parks|Parks.*Portland", df$source_list, ignore.case = TRUE)
  if (any(park_mask)) {
    for (i in which(park_mask)) {
      df$tags[i] <- add_tag_if_missing(df$tags[i], "park")
      df$tags[i] <- add_tag_if_missing(df$tags[i], "nature")
    }
  }
  
  # Standardize all tags to remove duplicates and fix case issues
  df$tags <- sapply(df$tags, standardize_tags)
  
  return(df)
}

# Apply the list-specific tags AFTER backfill
places <- apply_list_specific_tags(places)

# --- Activity & mode matchers + tag cleaner (used later) -------------------
matches_activity <- function(tags_text, category_terms) {
  if (is.na(tags_text) || tags_text == "") return(FALSE)
  tags_lower <- tolower(tags_text)
  any(vapply(category_terms, function(term) {
    clean_term <- gsub("[^A-Za-z ]", "", term) |> stringr::str_trim() |> tolower()
    if (clean_term != "") stringr::str_detect(tags_lower, stringr::fixed(clean_term)) else FALSE
  }, logical(1)))
}
matches_activity_mode <- function(title, tags, note, mode_terms) {
  if (is.na(title)) title <- ""; if (is.na(tags)) tags <- ""; if (is.na(note)) note <- ""
  combined_text <- tolower(paste(title, tags, note))
  any(vapply(mode_terms, function(term) {
    clean_term <- gsub("[^A-Za-z ]", "", term) |> stringr::str_trim() |> tolower()
    stringr::str_detect(combined_text, stringr::fixed(clean_term))
  }, logical(1)))
}
clean_tags <- function(tags_text) {
  if (is.na(tags_text) || tags_text == "") return("")
  cleaned <- gsub("[^A-Za-z0-9 .,!?()-]", " ", tags_text)
  cleaned <- stringr::str_squish(cleaned)
  words <- strsplit(cleaned, " ")[[1]]
  words <- words[nchar(words) > 2 | words %in% c("&", "or", "of")]
  paste(words, collapse = " ")
}

# --- Geocoding + simple visited persistence -------------------------------
geocode_address <- function(address) {
  tryCatch({
    if (is.null(address) || stringr::str_trim(address) == "") {
      return(list(success = FALSE, error = "Please enter an address"))
    }
    encoded <- utils::URLencode(stringr::str_trim(address))
    url <- paste0("https://nominatim.openstreetmap.org/search?q=", encoded, "&format=json&limit=1")
    response <- httr::GET(url, httr::user_agent("Portland Day Planner App"))
    if (httr::status_code(response) != 200) return(list(success = FALSE, error = "Geocoding unavailable. Try later."))
    content <- httr::content(response, "text", encoding = "UTF-8")
    result <- jsonlite::fromJSON(content)
    if (length(result) > 0 && nrow(result) > 0) {
      return(list(
        success = TRUE,
        lat = as.numeric(result$lat[1]),
        lng = as.numeric(result$lon[1]),
        formatted_address = result$display_name[1]
      ))
    }
    list(success = FALSE, error = "Address not found. Try a different format.")
  }, error = function(e) list(success = FALSE, error = paste("Error:", e$message)))
}

if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b
if (!exists("load_completed", mode = "function")) {
  load_completed <- function() {
    f <- "data/completed_places.rds"
    if (file.exists(f)) { tryCatch(readRDS(f), error = function(e) character(0)) } else { character(0) }
  }
}
if (!exists("save_completed", mode = "function")) {
  save_completed <- function(ids) {
    dir.create("data", showWarnings = FALSE, recursive = TRUE)
    tryCatch(saveRDS(ids, "data/completed_places.rds"), error = function(e) invisible(NULL))
    invisible(TRUE)
  }
}


ui <- fluidPage(
  # Floating weather/time stack in top-right of app
  div(class="app-weather-corner", uiOutput("weather_ui")),
  
  tags$head(
    tags$style(HTML("
  @import url('https://fonts.googleapis.com/css2?family=Work+Sans:wght@300;400;500;600;700&family=Poppins:wght@300;400;500;600;700&display=swap');
  :root{ 
    --bg:#fefcfb; --bg-grad:#fcf8f6; --card:#ffffff; --border:#f4e8e1; --text:#2d2926; --muted:#8b7765; 
    --accent:#e8a083; --accent-600:#e08a63; --accent-50:#fef7f4; --accent-hover:#dc8965;
    --secondary:#f2c2a7; --secondary-600:#ee9f7a; --secondary-50:#fef9f6;
    --tertiary:#f7d4c4; --tertiary-600:#f3b8a0; --tertiary-50:#fefaf8;
    --success:#7fb069; --danger:#d67e7e; --warning:#e6c79c; 
    --shadow-soft:0 4px 20px rgba(164,180,148,0.08); --shadow-medium:0 8px 32px rgba(164,180,148,0.12);
    --radius-sm:8px; --radius-md:12px; --radius-lg:16px; --radius-xl:24px;
  }
  * { box-sizing: border-box; }
  body { 
    font-family:'Work Sans',system-ui,sans-serif !important; 
    background:var(--bg); 
    color:var(--text); margin:0; min-height:100vh; line-height:1.6;
  }
  
  /* Weather/time stack fixed in top-right (floats while scrolling) */
  .app-weather-corner {
    position:fixed; top:80px; right:20px; z-index:1000;
    display:flex; flex-direction:column; gap:8px;
  }

  /* Header card */
  .header { 
    background:var(--card); color:var(--text); padding:20px; margin:-15px -15px 0 -15px;
    box-shadow:var(--shadow-soft); border-radius:0 0 var(--radius-xl) var(--radius-xl); 
    border-bottom:1px solid var(--border);
  }

  /* Modern header layout */
  .header-grid { display:flex; flex-direction:column; gap:12px; align-items:center; }
  .header-content-row { display:grid; grid-template-columns: 1fr 1fr; gap:24px; width:80%; max-width:900px; align-items:start; margin:0 auto; }
  .header-image-section { display:flex; justify-content:center; }
  .header-suggestion-section { display:flex; align-items:center; }
  
  .image-container {
    position:relative; width:350px; height:280px; border-radius:var(--radius-xl); 
    overflow:hidden; box-shadow:var(--shadow-medium); 
    transition:transform 0.3s ease, box-shadow 0.3s ease;
  }
  .image-container:hover { transform:translateY(-4px); box-shadow:0 16px 48px rgba(0,0,0,0.15); }
  
  .header-photo { 
    width:100%; height:100%; object-fit:cover; display:block;
  }

  /* Overlay weather displays - compact and modern (NO emojis) */
  .time-display {
    background:rgba(255,255,255,0.95); backdrop-filter:blur(12px);
    border:1px solid rgba(255,255,255,0.6); border-radius:var(--radius-md);
    padding:12px 16px; transition:all 0.3s ease; min-width:140px;
    box-shadow:0 4px 20px rgba(0,0,0,0.1);
  }
  .time-display:hover { transform:translateY(-2px); box-shadow:0 6px 24px rgba(0,0,0,0.15); }
  .time-label {
    font-size:10px; text-transform:uppercase; letter-spacing:0.8px;
    color:var(--muted); font-weight:600; margin-bottom:2px;
  }
  .time-value {
    font-size:13px; font-weight:600; color:var(--text);
    font-family:'Poppins',sans-serif; line-height:1.2;
  }

  .weather-display {
    background:rgba(255,255,255,0.95); backdrop-filter:blur(12px);
    border:1px solid rgba(255,255,255,0.6); border-radius:var(--radius-md);
    padding:12px 16px; transition:all 0.3s ease; min-width:180px;
    box-shadow:0 4px 20px rgba(0,0,0,0.1);
  }
  .weather-display:hover { transform:translateY(-2px); box-shadow:0 6px 24px rgba(0,0,0,0.15); }
  .weather-main { display:flex; justify-content:space-between; align-items:center; margin-bottom:8px; }
  .weather-condition { font-size:12px; font-weight:600; color:var(--text); text-transform:capitalize; font-family:'Poppins',sans-serif; }
  .weather-temp { font-size:16px; font-weight:700; color:var(--text); font-family:'Poppins',sans-serif; }
  .weather-details { display:flex; gap:8px; flex-wrap:wrap; }
  .weather-detail {
    font-size:9px; color:var(--muted); font-weight:500;
    background:rgba(255,255,255,0.6); padding:2px 6px;
    border-radius:8px; text-transform:uppercase; letter-spacing:0.5px;
  }

  /* Mobile responsive */
  @media (max-width: 768px){
    .header { padding:16px; }
    .header-grid { gap:12px; }
    .header-content-row { grid-template-columns: 1fr; gap:16px; }
    .image-container { width:100%; max-width:300px; height:240px; }
    .hero-card { height:240px; padding:24px; }
    .header-controls { margin-top:12px; }
  }

  .header h1 { 
    font-family:'Poppins',sans-serif; font-weight:700; margin:0 0 16px 0; 
    font-size:3rem; letter-spacing:-0.02em; color:var(--text);
  }
  .homebase-line { color:var(--muted); margin:0 0 16px 0; font-size:1.2rem; font-weight:400; }

  .header-controls { margin-top:24px; }
  .cta-row { display:flex; justify-content:center; margin-bottom:20px; }
  .btn-big { 
    font-family:'Poppins',sans-serif; font-weight:600; padding:16px 32px; 
    border-radius:var(--radius-md); border:none;
    background:#e8a083 !important; color:white; box-shadow:var(--shadow-soft); 
    transition:all 0.3s cubic-bezier(0.4,0,0.2,1); cursor:pointer;
    text-decoration:none; display:inline-block; position:relative; overflow:hidden;
  }
  .btn-big::before {
    content:''; position:absolute; top:0; left:-100%; width:100%; height:100%;
    background:linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
    transition:left 0.6s; z-index:1; pointer-events:none;
  }
  .btn-big:hover { transform:translateY(-3px); box-shadow:0 8px 25px rgba(232,160,131,0.4); background:#e08a63 !important; }
  .btn-big:hover::before { left:100%; }
  .btn-big:active { transform:translateY(-1px); }
  .btn-surprise .btn-icon{ display:inline-block; margin-right:8px; position:relative; width:16px; height:16px; vertical-align:middle; }
  .btn-surprise .btn-icon::before, .btn-surprise .btn-icon::after{
    content:''; position:absolute; width:4px; height:4px; background:currentColor; border-radius:50%;
    animation:pulse 1.5s infinite; pointer-events:none;
  }
  .btn-surprise .btn-icon::before { top:0; left:0; animation-delay:0s; }
  .btn-surprise .btn-icon::after { bottom:0; right:0; animation-delay:0.75s; }
  @keyframes pulse { 0%,100%{opacity:.3; transform:scale(.8);} 50%{opacity:1; transform:scale(1.2);} }

  /* Address box */
  .address-box { margin-top:20px; padding:20px; border:1px solid var(--border); border-radius:var(--radius-md); background:var(--card); box-shadow:var(--shadow-soft); transition:all 0.3s ease; }
  .address-box:hover { box-shadow:var(--shadow-medium); }
  .address-grid { display:grid; grid-template-columns: 1fr auto; gap:16px; align-items:center; }
  .address-status { font-size:13px; color:var(--muted); margin-top:8px; font-weight:500; }

  /* Control panel */
  .control-panel { background:var(--card); border-radius:var(--radius-lg); padding:28px; border:1px solid var(--border); box-shadow:var(--shadow-soft); transition:all 0.3s ease; position:relative; }
  .control-panel:hover { box-shadow:var(--shadow-medium); }
  .control-panel h4, .control-panel h5 { color:var(--text); font-weight:600; margin:8px 0 12px 0; font-size:1.05em; letter-spacing:-0.01em; font-family:'Poppins',sans-serif; }

  /* Activity / transport chips */
  .activity-btn{ margin:6px 4px; padding:12px 16px; border-radius:var(--radius-md); border:1px solid var(--border); background:var(--card); color:var(--text); cursor:pointer; font-size:13px; font-weight:500; transition:all .3s cubic-bezier(.4,0,.2,1); display:inline-block; position:relative; overflow:hidden; }
  .activity-btn::before{ content:''; position:absolute; top:0; left:-100%; width:100%; height:100%; background:linear-gradient(90deg, transparent, rgba(108,123,92,0.1), transparent); transition:left .5s; z-index:0; pointer-events:none; }
  .activity-btn:hover{ border-color:var(--accent); transform:translateY(-2px); box-shadow:0 4px 12px rgba(232,160,131,0.2); background:var(--accent-50); }
  .activity-btn:hover::before{ left:100%; }
  .activity-btn.active{ background:var(--accent); color:white; border-color:var(--accent); box-shadow:0 6px 16px rgba(232,160,131,0.4); transform:translateY(-1px); }

  .transport-btn{ margin:6px 4px; padding:14px 18px; border-radius:var(--radius-md); border:1px solid var(--border); background:var(--card); cursor:pointer; font-weight:600; transition:all .3s cubic-bezier(.4,0,.2,1); color:var(--text); position:relative; overflow:hidden; }
  .transport-btn::before{ content:''; position:absolute; top:0; left:-100%; width:100%; height:100%; background:linear-gradient(90deg, transparent, rgba(155,139,122,0.1), transparent); transition:left .5s; z-index:0; pointer-events:none; }
  .transport-btn:hover{ border-color:var(--secondary); transform:translateY(-2px); box-shadow:0 4px 12px rgba(155,139,122,0.2); background:var(--secondary-50); }
  .transport-btn:hover::before{ left:100%; }
  .transport-btn.active{ background:var(--secondary-50); border-color:var(--secondary); color:var(--secondary-600); box-shadow:0 4px 12px rgba(155,139,122,0.25); transform:translateY(-1px); }

  /* Hero card */
  .hero-suggestion{ margin-bottom:32px; }
  .hero-card{
    background:var(--accent-50); color:var(--text); border-radius:var(--radius-xl); padding:32px; 
    box-shadow:var(--shadow-medium); position:relative; overflow:hidden; transition:all .4s cubic-bezier(.4,0,.2,1);
    border:none; min-height:400px; height:auto; display:flex; flex-direction:column; justify-content:center;
  }
  .hero-card::before{ content:''; position:absolute; top:0; left:0; right:0; bottom:0; background:linear-gradient(45deg, transparent 0%, rgba(255,255,255,0.1) 50%, transparent 100%); transform:translateX(-100%); transition:transform .6s; pointer-events:none; }
  .hero-card:hover::before{ transform:translateX(100%); }
  .hero-card:hover{ transform:translateY(-4px) scale(1.02); box-shadow:0 12px 40px rgba(14,165,233,0.25); }
  .hero-card h2{ font-family:'Poppins',sans-serif; font-size:3rem; font-weight:700; margin:0 0 20px 0; line-height:1.2; }
  .hero-card .description{ font-size:1.8rem; font-weight:400; opacity:0.95; margin-bottom:24px; line-height:1.4; }
  .hero-card .details{ display:flex; gap:24px; flex-wrap:wrap; margin-top:auto; }
  .hero-card .detail-chip{ background:rgba(255,255,255,0.8); backdrop-filter:blur(10px); padding:8px 16px; border-radius:20px; font-size:14px; font-weight:500; color:var(--text); }

  .hero-empty{ background:var(--card); border:2px dashed var(--border); border-radius:var(--radius-xl); padding:60px 40px; text-align:center; color:var(--muted); min-height:200px; display:flex; flex-direction:column; justify-content:center; }
  .hero-empty h2{ font-family:'Poppins',sans-serif; font-size:1.8rem; margin-bottom:12px; color:var(--text); }

  /* Data table + map */
  .dataTables_wrapper{ background:var(--card); border-radius:var(--radius-lg); padding:24px; box-shadow:var(--shadow-soft); border:1px solid var(--border); transition:all .3s ease; }
  .dataTables_wrapper:hover{ box-shadow:var(--shadow-medium); }
  .leaflet-container{ border-radius:var(--radius-lg); box-shadow:var(--shadow-medium); transition:all .3s ease; }
  .leaflet-container:hover{ box-shadow:0 12px 40px rgba(0,0,0,0.15); }

  /* Location control panel */
  .location-control-panel { 
    background:var(--card); border-radius:var(--radius-lg); padding:24px; 
    box-shadow:var(--shadow-soft); border:1px solid var(--border); 
    transition:all .3s ease; height: fit-content;
  }
  .location-control-panel:hover { box-shadow:var(--shadow-medium); }

  /* Forms */
  .form-control, .selectize-input, input[type=text], select{
    border:1px solid var(--border) !important; border-radius:var(--radius-sm) !important;
    padding:12px 16px !important; font-size:14px !important; transition:all .3s ease !important; background:var(--card) !important;
  }
  .form-control:focus, .selectize-input.focus, input[type=text]:focus, select:focus{
    border-color:var(--accent) !important; box-shadow:0 0 0 3px rgba(246,139,139,0.1) !important; outline:none !important;
  }
  .btn-primary, .btn-outline-primary{ border-radius:var(--radius-sm) !important; font-weight:500 !important; padding:12px 20px !important; transition:all .3s ease !important; border:1px solid var(--accent) !important; }
  .btn-primary{ background:var(--accent) !important; color:white !important; }
  .btn-primary:hover{ transform:translateY(-2px) !important; box-shadow:0 6px 16px rgba(246,139,139,0.3) !important; }
  .btn-outline-primary{ background:var(--card) !important; color:var(--accent) !important; }
  .btn-outline-primary:hover{ background:var(--accent-50) !important; transform:translateY(-1px) !important; }


  /* Disabled transport button styles */
  .transport-btn.disabled, .transport-btn:disabled {
    opacity: 0.4 !important;
    color: #999 !important;
    background-color: #f5f5f5 !important;
    cursor: not-allowed !important;
    pointer-events: none !important;
  }

  @media (max-width:1100px){
    .header-grid { grid-template-columns: 1fr; }
    .header-left { width:100%; align-items:center; }
    .header-photo { width:100%; max-width:420px; height:auto; }
    .control-panel { padding:20px; }
    .hero-card { min-height:350px; padding:28px; }
    .hero-card h2 { font-size:2.6rem; }
    .hero-card .description { font-size:1.5rem; }
    .adventure-stop { font-size: 1.1rem !important; padding: 12px 16px !important; }
  }
")),
  ),
  
  # ======= HEADER =======
  div(class = "header",
      div(class = "header-grid",
          # Title + current starting location string (server fills)
          div(class = "header-title-section", style = "text-align: center; width: 100%;",
              h1("The Dream of the 90s is Alive in Portland"),
              uiOutput("home_info_ui")
          ),
          # Image + Hero Suggestion
          div(class = "header-content-row",
              # Left: image (height matches hero card)
              div(class="header-image-section",
                  div(class="image-container",
                      if (!is.null(HEADER_IMG))
                        tags$img(src = HEADER_IMG, alt = "Portland", class = "header-photo")
                  )
              ),
              # Right: suggestion card
              div(class="header-suggestion-section",
                  div(class="hero-suggestion",
                      uiOutput("hero_suggestion_display")
                  )
              )
          ),
      )
  ),
  
  # ======= STARTING LOCATION & MAP SECTION =======
  fluidRow(
    column(
      4,
      div(class = "location-control-panel",
          h4("Starting Location"),
          p("Choose one option below or click anywhere on the map", style = "color: var(--muted); font-size: 1.3rem; margin-bottom: 16px; font-weight: 600;"),
          
          # Address Option
          div(style = "margin-bottom: 16px; padding: 12px; border: 1px solid var(--border); border-radius: 8px;",
              h5("Option 1: Specific Address", style = "margin: 0 0 8px 0; color: var(--primary); font-size: 1.4rem;"),
              div(class="address-grid",
                  textInput("home_address", "", placeholder = "Enter your address (e.g., 123 Main St, Portland, OR)", value = "", width = "100%"),
                  actionButton("geocode_address", "Set Address", class = "btn-outline-primary", style = "min-width: 140px;")
              )
          ),
          
          # Area Option  
          div(style = "margin-bottom: 16px; padding: 12px; border: 1px solid var(--border); border-radius: 8px;",
              h5("Option 2: Portland Area", style = "margin: 0 0 8px 0; color: var(--primary); font-size: 1.4rem;"),
              div(style = "margin-bottom: 12px;",
                  tags$label("Choose a quadrant:", style = "font-size: 1.2rem; font-weight: normal; color: var(--text-secondary); margin-bottom: 4px; display: block;"),
                  selectInput("selected_quadrant", NULL, 
                             choices = c("Choose a quadrant..." = "", names(PORTLAND_QUADRANTS)), 
                             width = "100%")
              ),
              conditionalPanel(
                condition = "input.selected_quadrant != '' && input.selected_quadrant != null",
                div(style = "margin-bottom: 12px;",
                    tags$label("Optionally, narrow to a neighborhood:", style = "font-size: 0.9rem; font-weight: normal; color: var(--text-secondary); margin-bottom: 4px; display: block;"),
                    selectInput("selected_neighborhood", NULL, 
                               choices = c("Use entire quadrant" = ""), 
                               width = "100%")
                )
              ),
              div(style = "text-align: center; margin-bottom: 12px;",
                  actionButton("set_area", "Set Area", class = "btn-outline-primary", style = "min-width: 140px;")
              ),
              # Confirm starting location button (shown when location is set but not locked)
              conditionalPanel(
                condition = "output.map_instructions",
                div(style = "text-align: center; border-top: 1px solid var(--border); padding-top: 12px; margin-top: 12px;",
                    uiOutput("map_instructions")
                )
              )
          ),
          div(id = "address_status", class="address-status"),
          
          br(),
          div(style = "text-align: center; margin-top: 16px;",
              actionButton("random_inspiration", HTML("<span class='btn-icon'></span>Surprise Me"),
                           class = "btn-big btn-surprise",
                           style = "font-size: 1.3rem; padding: 20px 40px; font-weight: 700; width: 100%;")
          )
      )
    ),
    
    column(
      8,
      div(class="map-container", leafletOutput("map", height = 600)),
      div(id = "map_status", class="address-status", style = "margin-top: 8px;")
    )
  ),
  
  # ======= SUGGESTION CONTROLS =======
  fluidRow(
    column(
      12,
      div(class = "control-panel", style = "margin-top: 24px;",
          fluidRow(
            # Left narrow column: Where to explore
            column(3,
                   # Location Filters  
                   div(style = "margin-bottom: 16px; padding: 12px; border: 1px solid var(--border); border-radius: 8px;",
                       h5("Where to explore?", style = "margin: 0 0 8px 0; color: var(--primary); font-size: 1.3rem; font-weight: 600;"),
                       div(style = "margin-bottom: 12px;",
                           uiOutput("section_selector")
                       ),
                       div(style = "margin-bottom: 0;",
                           uiOutput("neighborhood_selector")  
                       )
                   )
            ),
            # Middle column: What kinds of places and transportation
            column(6,
                   # What kinds of places 
                   div(style = "margin-bottom: 16px; padding: 12px; border: 1px solid var(--border); border-radius: 8px;",
                       h5("What kinds of places?", style = "margin: 0 0 8px 0; color: var(--primary); font-size: 1.3rem; font-weight: 600;"),
                       div(id = "activity_buttons",
                           lapply(names(ACTIVITY_CATEGORIES), function(cat) {
                             actionButton(paste0("act_", gsub("[^A-Za-z0-9]", "", cat)), cat, class = "activity-btn")
                           })
                       )
                   ),
                   # Transportation - directly below What kinds of places
                   div(style = "margin-bottom: 16px; padding: 12px; border: 1px solid var(--border); border-radius: 8px;",
                       h5("How ya getting there?", style = "margin: 0 0 8px 0; color: var(--primary); font-size: 1.3rem; font-weight: 600;"),
                       div(id = "transport_buttons",
                           lapply(names(TRANSPORT_MODES), function(mode) {
                             actionButton(paste0("trans_", gsub("[^A-Za-z0-9]", "", mode)), mode, class = "transport-btn")
                           })
                       )
                   )
            ),
            # Right column: What's the mood, Number of stops, and Guided Plan
            column(3,
                   # Context Filter
                   div(style = "margin-bottom: 16px; padding: 12px; border: 1px solid var(--border); border-radius: 8px;",
                       h5("What's the mood?", style = "margin: 0 0 8px 0; color: var(--primary); font-size: 1.3rem; font-weight: 600;"),
                       selectizeInput("context_filter", "", choices = names(CONTEXT_FILTERS), selected = NULL, multiple = TRUE,
                                      options = list(placeholder = 'Any context'), width = "100%")
                   ),
                   # Number of stops
                   div(style = "margin-bottom: 16px; padding: 12px; border: 1px solid var(--border); border-radius: 8px;",
                       h5("Number of stops", style = "margin: 0 0 8px 0; color: var(--primary); font-size: 1.3rem; font-weight: 600;"),
                       selectInput("num_stops", NULL, 
                                  choices = c("1 stop" = 1, "2 stops" = 2, "3 stops" = 3), 
                                  selected = 1, width = "100%")
                   ),
                   # Guided Plan Button
                   div(style = "margin-bottom: 16px; padding: 12px; border: 1px solid var(--border); border-radius: 8px; height: fit-content;",
                       div(style = "text-align: center;",
                           actionButton("suggest_place", "Guided Plan", class = "btn-primary", style = "width: 100%; padding: 16px 20px; font-size: 1.4rem; font-weight: 600;")
                       )
                   ),
                   # Clear Selections Button
                   div(style = "margin-bottom: 16px; text-align: center;",
                       actionButton("clear_exploration", "Clear Selections", class = "btn-outline-secondary", style = "width: 100%; padding: 8px 16px; font-size: 1rem;")
                   )
            )
          ),
          # Visited Places row
          fluidRow(
            column(3), # Empty column to align with left column
            column(9,
                   # Visited Places
                   div(style = "margin-bottom: 16px; padding: 12px; border: 1px solid var(--border); border-radius: 8px;",
                       h5("Places Visited", style = "margin: 0 0 8px 0; color: var(--primary); font-size: 1.3rem; font-weight: 600;"),
                       verbatimTextOutput("visited_count"),
                       uiOutput("visited_preview")
                   )
            )
          )
      )
    )
  ),
  
  # ======= DATA TABLE =======
  fluidRow(
    column(
      12,
      div(class="data-container", style = "margin-top: 24px;",
          h5("All Available Places"),
          DT::dataTableOutput("places_table")
      )
    )
  ),
  
  # JS toggles
  tags$script(HTML("
    $(document).on('click', '.activity-btn', function() {
      $(this).toggleClass('active');
      var a1 = $('#activity_buttons .activity-btn.active').map(function(){return $(this).text();}).get();
      Shiny.setInputValue('selected_activities', a1, {priority: 'event'});

      var a2 = [];
      Shiny.setInputValue('selected_activity_modes', a2, {priority: 'event'});
    });

    $(document).on('click', '.transport-btn', function() {
      if ($(this).hasClass('active')) {
        $(this).removeClass('active');
        Shiny.setInputValue('selected_transport', '', {priority: 'event'});
      } else {
        $('.transport-btn').removeClass('active');
        $(this).addClass('active');
        Shiny.setInputValue('selected_transport', $(this).text(), {priority: 'event'});
      }
    });

    $(document).on('change', '#context_filter', function() {
      var contexts = $(this).val();
      $('#transport_buttons .transport-btn').show();

      if (contexts && contexts.includes('☔ Rainy Weather')) {
        // Hide outdoor activities
        // Hide walking and biking transport
        $('#transport_buttons .transport-btn').each(function(){
          var t = $(this).text();
          if (t.includes('Walking') || t.includes('Biking')) {
            $(this).hide().removeClass('active');
          }
        });
        var active_transport = $('#transport_buttons .transport-btn.active:visible').map(function(){return $(this).text();}).get();
        Shiny.setInputValue('selected_transport', active_transport.join(', '), {priority: 'event'});
      }
      
      if (contexts && contexts.includes('🚶 No Car')) {
        // Disable driving option
        $('#transport_buttons .transport-btn').each(function(){
          var t = $(this).text();
          if (t.includes('Driving')) {
            $(this).prop('disabled', true).addClass('disabled').removeClass('active');
          }
        });
        var active_transport = $('#transport_buttons .transport-btn.active:not(.disabled)').map(function(){return $(this).text();}).get();
        Shiny.setInputValue('selected_transport', active_transport.join(', '), {priority: 'event'});
      } else {
        // Re-enable driving option when No Car is not selected
        $('#transport_buttons .transport-btn').each(function(){
          var t = $(this).text();
          if (t.includes('Driving')) {
            $(this).prop('disabled', false).removeClass('disabled');
          }
        });
      }
    });
    
    // Handle clear exploration selections message from R
    Shiny.addCustomMessageHandler('clearExplorationButtons', function(message) {
      // Clear activity button selections
      $('#activity_buttons .activity-btn').removeClass('active');
      Shiny.setInputValue('selected_activities', [], {priority: 'event'});
      
      // Clear transport button selections
      $('#transport_buttons .transport-btn').removeClass('active');
      Shiny.setInputValue('selected_transport', '', {priority: 'event'});
    });
  "))
)

# ---- Coordinate helper ----
has_coords <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(rep(FALSE, 0))
  if (!all(c("lat","lng") %in% names(df))) return(rep(FALSE, nrow(df)))
  is.finite(df$lat) & is.finite(df$lng)
}

# --- Weather & time helpers (outside server) ---
get_weather_forecast <- function() {
  tryCatch({
    url <- "https://wttr.in/Portland,OR?format=%C|%t|%h|%w&u"
    response <- readLines(url, warn = FALSE)
    parts <- strsplit(response[1], "\\|")[[1]]
    if (length(parts) >= 4) {
      condition <- parts[1]
      temp <- parts[2]
      humidity <- parts[3]
      wind <- parts[4]
      is_rainy <- grepl("rain|drizzle|shower|storm", tolower(condition))
      return(list(condition=condition, temperature=temp, humidity=humidity, wind=wind, is_rainy=is_rainy, success=TRUE))
    }
    list(success=FALSE)
  }, error=function(e) list(success=FALSE))
}


# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  # ---- Weather/time (no emojis) ----
  current_weather <- reactive({
    invalidateLater(15 * 60 * 1000, session)  # refresh theme + data every 15 min
    get_weather_forecast() %||% list(success = FALSE)
  })
  output$weather_ui <- renderUI({
    invalidateLater(60 * 1000, session)       # tick the clock each minute
    w <- current_weather()
    time_str <- pdx_time_string()
    if (isTRUE(w$success)) {
      tagList(
        div(class="time-display",
            div(class="time-label", "Current Time"),
            div(class="time-value", time_str)
        ),
        div(class="weather-display",
            div(class="weather-main",
                div(class="weather-condition", w$condition),
                div(class="weather-temp", w$temperature)
            ),
            div(class="weather-details",
                span(class="weather-detail", paste("Humidity:", w$humidity)),
                span(class="weather-detail", paste("Wind:", w$wind))
            )
        )
      )
    } else {
      div(class="time-display",
          div(class="time-label", "Current Time"),
          div(class="time-value", time_str)
      )
    }
  })
  
  # ---- State ----
  values <- reactiveValues(
    completed = load_completed(),
    suggested = NULL,
    inspiration_text = NULL,
    home_lat = NA_real_,
    home_lng = NA_real_,
    home_address = "",
    preview_address = NULL,
    map_clicked = FALSE,
    starting_location_locked = FALSE,
    pending_nb = NULL           # stores desired neighborhood during quadrant refresh
  )
  
  
  neighborhood_to_quadrant <- function(neigh) {
    if (is.null(neigh) || !nzchar(neigh)) return(NA_character_)
    target <- tolower(trimws(neigh))
    
    # First check hardcoded neighborhoods
    for (q in names(NEIGHBORHOODS_BY_QUADRANT)) {
      if (target %in% tolower(names(NEIGHBORHOODS_BY_QUADRANT[[q]]))) return(q)
    }
    
    # If not found, check geographic boundaries
    if (!is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL) &&
        !is.null(sections_boundaries) && !is.null(SEC_NAME_COL)) {
      # Try exact match first
      nb_matches <- neighborhood_boundaries[tolower(as.character(neighborhood_boundaries[[NEI_NAME_COL]])) == target, , drop = FALSE]
      
      # If no exact match, try partial match (in case of slight naming differences)
      if (nrow(nb_matches) == 0) {
        all_nb_names <- tolower(as.character(neighborhood_boundaries[[NEI_NAME_COL]]))
        partial_matches <- which(grepl(target, all_nb_names, fixed = TRUE) | grepl(all_nb_names, target, fixed = TRUE))
        if (length(partial_matches) > 0) {
          nb_matches <- neighborhood_boundaries[partial_matches[1], , drop = FALSE] # Take first match
        }
      }
      
      if (nrow(nb_matches) > 0) {
        # Find which quadrant this neighborhood intersects with
        for (q in names(PORTLAND_QUADRANTS)) {
          q_norm <- normalize_sextant(q)
          sel_secs <- sections_boundaries[sections_boundaries[[SEC_NAME_COL]] %in% q_norm, , drop = FALSE]
          if (nrow(sel_secs) > 0) {
            intersects <- safe_st_intersects_rows(nb_matches, sel_secs)
            if (length(intersects) > 0) return(q)
          }
        }
      }
    }
    
    NA_character_
  }
  
  # Get neighborhood info (coordinates and name) from either hardcoded data or geographic boundaries
  get_neighborhood_info <- function(neighborhood, quadrant) {
    if (is.null(neighborhood) || !nzchar(neighborhood)) return(NULL)
    
    # First try hardcoded neighborhoods
    if (!is.null(quadrant) && quadrant %in% names(NEIGHBORHOODS_BY_QUADRANT)) {
      tryCatch({
        quadrant_neighborhoods <- NEIGHBORHOODS_BY_QUADRANT[[quadrant]]
        if (!is.null(quadrant_neighborhoods) && neighborhood %in% names(quadrant_neighborhoods)) {
          hardcoded_info <- quadrant_neighborhoods[[neighborhood]]
          if (!is.null(hardcoded_info)) return(hardcoded_info)
        }
      }, error = function(e) {
        # Continue to geographic boundary approach
      })
    }
    
    # If not found, calculate from geographic boundaries
    if (!is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL)) {
      # Try exact match first (case insensitive)
      nb_matches <- neighborhood_boundaries[tolower(as.character(neighborhood_boundaries[[NEI_NAME_COL]])) == tolower(neighborhood), , drop = FALSE]
      
      # If no exact match, try partial match
      if (nrow(nb_matches) == 0) {
        all_nb_names <- as.character(neighborhood_boundaries[[NEI_NAME_COL]])
        target_lower <- tolower(neighborhood)
        partial_matches <- which(grepl(target_lower, tolower(all_nb_names), fixed = TRUE) | grepl(tolower(all_nb_names), target_lower, fixed = TRUE))
        if (length(partial_matches) > 0) {
          nb_matches <- neighborhood_boundaries[partial_matches[1], , drop = FALSE] # Take first match
        }
      }
      
      if (nrow(nb_matches) > 0) {
        # Calculate centroid of the neighborhood
        bbox <- sf::st_bbox(nb_matches)
        lat <- (bbox$ymin + bbox$ymax) / 2
        lng <- (bbox$xmin + bbox$xmax) / 2
        actual_name <- as.character(nb_matches[[NEI_NAME_COL]])[1]
        return(list(lat = lat, lng = lng, name = actual_name))
      }
    }
    
    NULL
  }
  strip_portland <- function(x) sub("\\s*portland\\s*$", "", tolower(trimws(x %||% "")))
  normalize_quadrant_input <- function(x) normalize_sextant(tools::toTitleCase(strip_portland(x)))
  
  set_starting_selects <- function(q, nb = NULL) {
    if (is.null(q) || !nzchar(q) || is.na(q)) return(invisible())
    q_norm <- normalize_sextant(q)
    
    # Prime the neighborhood selection to survive the quadrant observer refresh
    if (!is.null(nb) && nzchar(nb)) {
      n_choices <- names(NEIGHBORHOODS_BY_QUADRANT[[q_norm]] %||% list())
      hit <- n_choices[tolower(n_choices) == tolower(nb)]
      values$pending_nb <- if (length(hit)) hit[[1]] else ""
    } else {
      values$pending_nb <- ""
    }
    
    # Trigger the quadrant observer; it will apply pending_nb as selected
    updateSelectInput(
      session, "selected_quadrant",
      choices = c("Choose a quadrant..." = "", names(PORTLAND_QUADRANTS)),
      selected = q_norm
    )
  }
  
  
  # Header: home info
  output$home_info_ui <- renderUI({
    if (!is.null(values$home_address) && nzchar(values$home_address)) {
      tags$p(sprintf("starting from: %s", values$home_address), class = "homebase-line")
    } else NULL
  })
  
  # Map instructions based on state
  output$map_instructions <- renderUI({
    if (!values$starting_location_locked) {
      div(
        if (!is.na(values$home_lat) && !is.na(values$home_lng)) {
          div(
            p(paste("Preview location set. Click below to confirm:"), 
              style = "color: var(--muted); font-size: 0.9rem; margin-bottom: 8px; text-align: center;"),
            actionButton("lock_starting_location", "✓ Confirm Starting Location", 
                         class = "btn-success", 
                         style = "width: 100%; font-weight: 500;")
          )
        }
      )
    } else {
      div(
        p("✅ Starting location confirmed!", 
          style = "color: var(--success); font-size: 1.3rem; margin-bottom: 8px; text-align: center; font-weight: 700;"),
        actionButton("reset_starting_location", "Change Starting Location", 
                     class = "btn-outline-secondary", 
                     style = "width: 100%; font-size: 1.1rem; font-weight: 600;")
      )
    }
  })
  
  # Output for conditional panel to show/hide map confirmation button
  output$map_clicked <- reactive({
    values$map_clicked
  })
  outputOptions(output, "map_clicked", suspendWhenHidden = FALSE)
  
  # Geocoding
  observeEvent(input$geocode_address, {
    if (is.null(input$home_address) || input$home_address == "") {
      output$address_status <- renderText("Please enter an address."); return()
    }
    output$address_status <- renderText("Geocoding address...")
    result <- geocode_address(input$home_address)
    if (isTRUE(result$success)) {
      values$home_lat <- result$lat; values$home_lng <- result$lng; values$home_address <- result$formatted_address
      values$starting_location_locked <- TRUE
      values$map_clicked <- FALSE  # Clear any map click preview
      output$address_status <- renderText(paste("Location set:", result$formatted_address))
      showNotification("Address successfully geocoded! Map now in exploration mode.", type = "message")
    } else {
      output$address_status <- renderText(paste("Error:", result$error %||% "Geocoding failed"))
      showNotification(paste("Geocoding failed:", result$error %||% "Unknown error"), type = "error")
    }
  })
  
  # Update neighborhood choices when quadrant changes (for starting location)
  observeEvent(input$selected_quadrant, {
    q <- input$selected_quadrant %||% ""
    if (!nzchar(q)) {
      updateSelectInput(session, "selected_neighborhood",
                        choices = c("Use entire quadrant" = ""), selected = "")
      return()
    }
    
    neighborhoods <- character(0)
    
    # First try hardcoded neighborhoods (this is reliable)
    if (q %in% names(NEIGHBORHOODS_BY_QUADRANT)) {
      neighborhoods <- names(NEIGHBORHOODS_BY_QUADRANT[[q]] %||% list())
    }
    
    # If hardcoded list is empty, try geographic boundary approach as fallback
    if (length(neighborhoods) == 0 && 
        !is.null(sections_boundaries) && !is.null(SEC_NAME_COL) &&
        !is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL) &&
        nrow(sections_boundaries) > 0 && nrow(neighborhood_boundaries) > 0) {
      
      tryCatch({
        normalized_q <- normalize_sextant(q)
        if (!is.na(normalized_q) && nzchar(normalized_q)) {
          sel_secs <- sections_boundaries[sections_boundaries[[SEC_NAME_COL]] %in% normalized_q, , drop = FALSE]
          if (nrow(sel_secs) > 0) {
            rows_to_draw <- safe_st_intersects_rows(neighborhood_boundaries, sel_secs)
            if (length(rows_to_draw) > 0) {
              neighborhoods <- neighborhood_boundaries[rows_to_draw, ][[NEI_NAME_COL]] |> 
                as.character() |> 
                unique() |> 
                sort()
              # Filter out any NA or empty values
              neighborhoods <- neighborhoods[!is.na(neighborhoods) & nzchar(neighborhoods)]
            }
          }
        }
      }, error = function(e) {
        # Geographic approach also failed, neighborhoods remains empty
        neighborhoods <- character(0)
      })
    }
    
    choices <- c("Use entire quadrant" = "", neighborhoods)
    
    # Prefer a programmatically requested neighborhood if present
    sel <- values$pending_nb %||% isolate(input$selected_neighborhood) %||% ""
    if (!nzchar(sel) || !(sel %in% neighborhoods)) sel <- ""
    
    updateSelectInput(session, "selected_neighborhood", choices = choices, selected = sel)
    values$pending_nb <- NULL  # clear once applied
  })
  
  
  # Handle area selection (quadrant or neighborhood)
  observeEvent(input$set_area, {
    if (is.null(input$selected_quadrant) || input$selected_quadrant == "") {
      output$address_status <- renderText("Please select a quadrant first."); 
      return()
    }
    
    # Check if a specific neighborhood is selected
    if (!is.null(input$selected_neighborhood) && nzchar(input$selected_neighborhood)) {
      tryCatch({
        neighborhood_info <- get_neighborhood_info(input$selected_neighborhood, input$selected_quadrant)
        if (!is.null(neighborhood_info)) {
          values$home_lat <- neighborhood_info$lat
          values$home_lng <- neighborhood_info$lng
          values$home_address <- neighborhood_info$name
          values$starting_location_locked <- TRUE
          values$map_clicked <- FALSE  # Clear any map click preview
          output$address_status <- renderText(paste("Starting from:", neighborhood_info$name))
          showNotification(paste("Starting location set to", neighborhood_info$name, "- Map now in exploration mode"), type = "message")
        } else {
          output$address_status <- renderText("Neighborhood not found.")
          return()
        }
        
        # Sync explore selectors
        updateSelectizeInput(session, "section_filter",
                             selected = normalize_sextant(input$selected_quadrant), server = TRUE)
        updateSelectizeInput(session, "neighborhood_filter",
                             selected = input$selected_neighborhood, server = TRUE)
      }, error = function(e) {
        output$address_status <- renderText(paste("Error setting neighborhood:", e$message))
        showNotification(paste("Error:", e$message), type = "error")
        return()
      })
      
    } else {
      # Use quadrant center - add error handling here too
      tryCatch({
        if (!(input$selected_quadrant %in% names(PORTLAND_QUADRANTS))) {
          output$address_status <- renderText("Invalid quadrant selection.")
          return()
        }
        quadrant_info <- PORTLAND_QUADRANTS[[input$selected_quadrant]]
        if (is.null(quadrant_info)) {
          output$address_status <- renderText("Quadrant information not found.")
          return()
        }
        
        values$home_lat <- quadrant_info$lat
        values$home_lng <- quadrant_info$lng
        values$home_address <- quadrant_info$name
        values$starting_location_locked <- TRUE
        values$map_clicked <- FALSE  # Clear any map click preview
        output$address_status <- renderText(paste("Starting from:", quadrant_info$name))
        showNotification(paste("Starting location set to", quadrant_info$name, "- Map now in exploration mode"), type = "message")
        
        # Sync explore selectors
        updateSelectizeInput(session, "section_filter",
                             selected = normalize_sextant(input$selected_quadrant), server = TRUE)
        updateSelectizeInput(session, "neighborhood_filter",
                             selected = character(0), server = TRUE)
      }, error = function(e) {
        output$address_status <- renderText(paste("Error setting quadrant:", e$message))
        showNotification(paste("Error:", e$message), type = "error")
        return()
      })
    }
  })
  
  
  
  # --- Replace your confirm_map_location observer with this ---
  observeEvent(input$confirm_map_location, {
    # Validate location data before confirming
    if (is.na(values$home_lat) || is.na(values$home_lng) || 
        is.null(values$preview_address) || !nzchar(values$preview_address)) {
      showNotification("Cannot confirm location. Please click on the map or set an address first.", type = "error")
      return()
    }
    
    tryCatch({
      values$home_address <- values$preview_address
      
      # Try neighborhood first
      prev <- values$preview_address %||% ""
      q_guess <- neighborhood_to_quadrant(prev)
      
      if (!is.na(q_guess)) {
        # Sync both Explore and Starting pickers
        updateSelectizeInput(session, "section_filter",
                             selected = normalize_sextant(q_guess), server = TRUE)
        updateSelectizeInput(session, "neighborhood_filter",
                             selected = prev, server = TRUE)
        set_starting_selects(q_guess, prev)
      } else {
        # Maybe it was a quadrant-like string ("Northwest Portland")
        q_guess2 <- normalize_quadrant_input(prev)
        if (q_guess2 %in% names(PORTLAND_QUADRANTS)) {
          updateSelectizeInput(session, "section_filter",
                               selected = q_guess2, server = TRUE)
          updateSelectizeInput(session, "neighborhood_filter",
                               selected = character(0), server = TRUE)
          set_starting_selects(q_guess2, NULL)
        } else {
          # Unknown area → clear neighborhood in Starting panel
          set_starting_selects("", NULL)
        }
      }
      
      values$starting_location_locked <- TRUE
      values$map_clicked <- FALSE
      output$address_status <- renderText(paste("Starting from:", values$home_address))
      showNotification(paste("Starting location set to", values$home_address,
                             "- Map now in exploration mode"), type = "success")
    }, error = function(e) {
      showNotification(paste("Error confirming map location:", e$message), type = "error")
    })
  })
  
  # Manual lock starting location
  observeEvent(input$lock_starting_location, {
    # Comprehensive validation before locking
    if (is.na(values$home_lat) || is.na(values$home_lng) || 
        is.null(values$home_address) || !nzchar(values$home_address)) {
      showNotification("Cannot confirm starting location. Please set your location first using 'Set Address' or 'Set Area'.", type = "error")
      return()
    }
    
    # Additional validation for reasonable coordinates
    if (is.na(as.numeric(values$home_lat)) || is.na(as.numeric(values$home_lng))) {
      showNotification("Invalid coordinates detected. Please set your location again.", type = "error")
      return()
    }
    
    tryCatch({
      values$starting_location_locked <- TRUE
      showNotification("Starting location locked! Map now in exploration mode.", type = "success")
    }, error = function(e) {
      showNotification(paste("Error confirming location:", e$message), type = "error")
    })
  })
  
  # Reset starting location
  observeEvent(input$reset_starting_location, {
    values$starting_location_locked <- FALSE
    values$home_lat <- NA_real_
    values$home_lng <- NA_real_
    values$home_address <- ""
    output$address_status <- renderText("")
    showNotification("Starting location reset. Click map to set new location.", type = "message")
  })
  
  # Distances only when home is set
  places_with_distances <- reactive({
    df <- places
    if (home_is_set(values$home_address, values$home_lat, values$home_lng)) {
      df$distance_mi <- mapply(function(lat, lng) calc_distance_miles(values$home_lat, values$home_lng, lat, lng), df$lat, df$lng)
    } else df$distance_mi <- NA_real_
    df
  })
  
  # Sextant helpers + selectors
  get_sextant_choices <- function(places, sections_boundaries, SEC_NAME_COL) {
    choices <- NULL
    if (!is.null(sections_boundaries) && !is.null(SEC_NAME_COL) && SEC_NAME_COL %in% names(sections_boundaries)) {
      choices <- sections_boundaries[[SEC_NAME_COL]] |> as.character()
    } else if ("section" %in% names(places)) {
      choices <- as.character(places$section)
    }
    if (is.null(choices) || !length(choices)) choices <- c("North","South","Northeast","Northwest","Southeast","Southwest")
    sort(unique(normalize_sextant(choices)))
  }
  safe_st_intersects_rows <- function(neigh_sf, sect_sf) {
    if (is.null(neigh_sf) || is.null(sect_sf) || nrow(sect_sf) == 0) return(integer(0))
    if (!is.na(sf::st_crs(sect_sf)) && !is.na(sf::st_crs(neigh_sf)) && sf::st_crs(sect_sf) != sf::st_crs(neigh_sf)) {
      neigh_sf <- sf::st_transform(neigh_sf, sf::st_crs(sect_sf))
    }
    suppressWarnings({ idx <- sf::st_intersects(neigh_sf, sect_sf, sparse = FALSE) })
    which(rowSums(idx) > 0)
  }
  output$section_selector <- renderUI({
    sextant_choices <- get_sextant_choices(places, sections_boundaries, SEC_NAME_COL)
    selectizeInput("section_filter", "", choices = sextant_choices, selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Any quadrant"))
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (is.null(click$id) || is.null(click$group)) return(NULL)
    
    # ---------- SET LOCATION MODE (not locked): update Starting Location only ----------
    if (!isTRUE(values$starting_location_locked)) {
      
      if (identical(click$group, "sextants")) {
        q <- normalize_sextant(sub("^sextant::", "", click$id))
        if (!(q %in% names(PORTLAND_QUADRANTS))) return(NULL)
        
        # Update the quadrant dropdown directly
        updateSelectInput(session, "selected_quadrant", selected = q)
        # Clear neighborhood selection when quadrant changes
        updateSelectInput(session, "selected_neighborhood", selected = "")
        
        info <- PORTLAND_QUADRANTS[[q]]
        values$home_lat <- info$lat; values$home_lng <- info$lng
        values$preview_address <- info$name
        values$map_clicked <- TRUE
        output$address_status <- renderText(paste("Preview:", info$name, "(click '✓ Confirm Map Location' to lock)"))
        showNotification(paste("Starting location preview:", info$name), type = "default")
        return(invisible(NULL))
      }
      
      
      return(invisible(NULL))
    }
    
    # ---------- EXPLORE MODE (locked): your existing explore toggles ----------
    if (identical(click$group, "sextants")) {
      raw_name <- sub("^sextant::", "", click$id)
      sec_name <- normalize_sextant(raw_name)
      sx_choices <- get_sextant_choices(places, sections_boundaries, SEC_NAME_COL)
      if (!sec_name %in% sx_choices) { 
        showNotification(paste("Unrecognized Quadrant:", raw_name), type = "warning", duration = 3) 
        return(NULL) 
      }
      cur <- isolate(input$section_filter); if (is.null(cur)) cur <- character(0)
      cur <- normalize_sextant(cur)
      was_selected <- sec_name %in% cur
      if (was_selected) {
        cur <- setdiff(cur, sec_name)
        showNotification(paste("✅ REMOVED", sec_name), type = "warning", duration = 2)
      } else {
        cur <- unique(c(cur, sec_name))
        showNotification(paste("✅ ADDED", sec_name), type = "default", duration = 2)
      }
      updateSelectizeInput(session, "section_filter", choices = sx_choices, selected = cur, server = TRUE)
      updateSelectizeInput(session, "neighborhood_filter", choices = character(0), selected = character(0), server = TRUE)
      return(invisible(NULL))
    }
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$section_filter, {
    updateSelectizeInput(session, "neighborhood_filter", selected = character(0))
  }, ignoreInit = TRUE)
  
  # Neighborhood selector based on selected Sextants
  output$neighborhood_selector <- renderUI({
    # Add additional safety checks
    if (is.null(input$section_filter) || !length(input$section_filter) ||
        is.null(sections_boundaries) || is.null(SEC_NAME_COL) ||
        is.null(neighborhood_boundaries) || is.null(NEI_NAME_COL) ||
        nrow(sections_boundaries) == 0 || nrow(neighborhood_boundaries) == 0) {
      return(selectizeInput("neighborhood_filter", "", choices = character(0), selected = NULL, multiple = TRUE,
                            options = list(placeholder = "Select quadrant first")))
    }
    
    # Additional safety in filtering
    tryCatch({
      normalized_sections <- normalize_sextant(input$section_filter)
      if (length(normalized_sections) == 0 || any(is.na(normalized_sections))) {
        return(selectizeInput("neighborhood_filter", "", choices = character(0), selected = NULL, multiple = TRUE,
                              options = list(placeholder = "Invalid quadrant selection")))
      }
      
      sel_secs <- sections_boundaries[sections_boundaries[[SEC_NAME_COL]] %in% normalized_sections, , drop = FALSE]
      if (nrow(sel_secs) == 0) {
        return(selectizeInput("neighborhood_filter", "", choices = character(0), selected = NULL, multiple = TRUE,
                              options = list(placeholder = "No quadrant boundaries found")))
      }
      
      rows_to_draw <- safe_st_intersects_rows(neighborhood_boundaries, sel_secs)
      available_neighborhoods <- if (length(rows_to_draw) > 0) {
        neighborhood_boundaries[rows_to_draw, ][[NEI_NAME_COL]] |> as.character() |> unique() |> sort()
      } else character(0)
      
      # Filter out any NA or empty neighborhood names
      available_neighborhoods <- available_neighborhoods[!is.na(available_neighborhoods) & nzchar(available_neighborhoods)]
      
      selectizeInput("neighborhood_filter", "", choices = available_neighborhoods, selected = NULL, multiple = TRUE,
                     options = list(placeholder = if (length(available_neighborhoods)) "Any neighborhood" else "No neighborhoods found"))
    }, error = function(e) {
      # Fallback UI in case of any errors
      selectizeInput("neighborhood_filter", "", choices = character(0), selected = NULL, multiple = TRUE,
                     options = list(placeholder = "Error loading neighborhoods"))
    })
  })
  
  # Filtering pipeline
  filtered_places <- reactive({
    df <- places_with_distances()
    df$neigh_disp <- neigh_display_vec(df$neighborhood_geo, df$neighborhood)
    
    # Quadrants
    if (!is.null(input$section_filter) && length(input$section_filter) > 0 && ("section_geo" %in% names(df) || "section" %in% names(df))) {
      wanted <- normalize_sextant(input$section_filter)
      if ("section_geo" %in% names(df)) {
        df <- df[!is.na(df$section_geo) & df$section_geo %in% wanted, , drop = FALSE]
      } else {
        df$section_norm <- normalize_sextant(df$section)
        df <- df[!is.na(df$section_norm) & df$section_norm %in% wanted, , drop = FALSE]
      }
    }
    # Neighborhoods
    if (!is.null(input$neighborhood_filter) && length(input$neighborhood_filter) > 0) {
      df <- df[!is.na(df$neigh_disp) & df$neigh_disp %in% input$neighborhood_filter, , drop = FALSE]
    }
    # Activity categories
    if (!is.null(input$selected_activities) && length(input$selected_activities) > 0) {
      activity_match <- rep(FALSE, nrow(df))
      for (activity in input$selected_activities) {
        if (activity %in% names(ACTIVITY_CATEGORIES)) {
          matches <- sapply(df$tags, function(tags) matches_activity(tags, ACTIVITY_CATEGORIES[[activity]]))
          activity_match <- activity_match | matches
        }
      }
      df <- df[activity_match, , drop = FALSE]
    }
    # Activity modes
    if (!is.null(input$selected_activity_modes) && length(input$selected_activity_modes) > 0) {
      mode_match <- rep(FALSE, nrow(df))
      for (mode in input$selected_activity_modes) {
        if (mode %in% names(ACTIVITY_MODES)) {
          matches <- mapply(function(title, tags, note) {
            matches_activity_mode(title, tags, note, ACTIVITY_MODES[[mode]])
          }, df$title, df$tags, df$note)
          mode_match <- mode_match | matches
        }
      }
      df <- df[mode_match, , drop = FALSE]
    }
    # Context filters
    if (!is.null(input$context_filter) && length(input$context_filter) > 0) {
      for (context_name in input$context_filter) {
        context <- CONTEXT_FILTERS[[context_name]]; if (is.null(context)) next
        if ("exclude_activities" %in% names(context)) {
          for (ex in context$exclude_activities) if (ex %in% names(ACTIVITY_MODES)) {
            terms <- ACTIVITY_MODES[[ex]]
            exm <- mapply(function(title, tags, note) matches_activity_mode(title, tags, note, terms),
                          df$title, df$tags, df$note)
            df <- df[!exm, , drop = FALSE]
          }
        }
        if ("exclude_venues" %in% names(context)) {
          for (ex in context$exclude_venues) if (ex %in% names(ACTIVITY_CATEGORIES)) {
            terms <- ACTIVITY_CATEGORIES[[ex]]
            exm <- sapply(df$tags, function(tags) matches_activity(tags, terms))
            df <- df[!exm, , drop = FALSE]
          }
        }
        if (home_is_set(values$home_address, values$home_lat, values$home_lng) &&
            "max_distance" %in% names(context)) {
          df <- df[!is.na(df$distance_mi) & df$distance_mi <= context$max_distance, , drop = FALSE]
        }
        if ("exclude_transport" %in% names(context) && 
            home_is_set(values$home_address, values$home_lat, values$home_lng)) {
          excluded_modes <- context$exclude_transport
          if ("🚶 Walking" %in% excluded_modes && "🚲 Biking" %in% excluded_modes) {
            # For rainy weather: exclude places that would primarily be reached by walking or biking
            # Keep very close places (under 0.5 mi - short dash to car) or longer distance places
            df <- df[is.na(df$distance_mi) | df$distance_mi <= 0.5 | df$distance_mi >= 3, , drop = FALSE]
          }
        }
      }
    }
    # Transport constraints
    if (home_is_set(values$home_address, values$home_lat, values$home_lng) &&
        !is.null(input$selected_transport) && nzchar(input$selected_transport)) {
      transport_text <- input$selected_transport
      for (mode in names(TRANSPORT_MODES)) {
        if (stringr::str_detect(transport_text, stringr::fixed(stringr::str_sub(mode, 1, 10)))) {
          max_dist <- TRANSPORT_MODES[[mode]]
          if (max_dist < 999) df <- df[!is.na(df$distance_mi) & df$distance_mi <= max_dist, , drop = FALSE]
          break
        }
      }
    }
    
    # Permanent filter: exclude bike shops
    bike_shop_patterns <- c("bike", "cycling", "bicycle", "cycle")
    is_bike_shop <- sapply(df$tags, function(tags) {
      tags_lower <- tolower(tags %||% "")
      any(sapply(bike_shop_patterns, function(pattern) grepl(pattern, tags_lower)))
    })
    df <- df[!is_bike_shop, , drop = FALSE]
    
    df
  })
  
  available_places <- reactive({
    df <- filtered_places()
    df[!(df$id %in% values$completed), , drop = FALSE]
  })
  
  # Clear exploration selections (but keep starting location)
  observeEvent(input$clear_exploration, {
    # Clear exploration filters
    updateSelectizeInput(session, "context_filter", selected = NULL)
    updateSelectizeInput(session, "section_filter", selected = character(0))
    updateSelectizeInput(session, "neighborhood_filter", selected = character(0))
    updateSelectInput(session, "num_stops", selected = 1)
    
    # Clear activity and transport button selections via JavaScript
    session$sendCustomMessage("clearExplorationButtons", list())
    
    # Clear any current suggestions
    values$suggested <- NULL
    values$inspiration_text <- NULL
    
    showNotification("Exploration selections cleared", type = "message", duration = 2)
  })
  
  # Suggestions
  observeEvent(input$suggest_place, {
    # Check for stay home conditions  
    should_stay_home <- !is.null(input$context_filter) && 
                        length(input$context_filter) > 0 && 
                        "☔ Rainy Weather" %in% input$context_filter
    
    if (should_stay_home) {
      # Generate stay-at-home suggestions
      stay_home_activities <- c(
        "paint",
        "finish a book",
        "find a new movie to watch",
        "draw", 
        "craft",
        "start a new puzzle",
        "scan film negatives"
      )
      
      chosen_activity <- sample(stay_home_activities, 1)
      mood <- "☔ Rainy Weather"
      
      values$suggested <- NULL  # No map location for staying home
      values$inspiration_text <- list(
        title = "Stay Home & Cozy",
        description = paste("Stay at home to", chosen_activity),
        type = "stay_home",
        estimated_time = "As long as you want",
        transit = "🏠 Stay Home",
        mood = mood
      )
      return()
    }
    
    # Check if address is set - multiple safety checks
    if (!home_is_set(values$home_address, values$home_lat, values$home_lng) ||
        is.na(values$home_lat) || is.na(values$home_lng) ||
        is.null(values$home_address) || !nzchar(values$home_address)) {
      showNotification("Please set your starting location first! Use 'Set Address' or 'Set Area' to choose where you're starting from.", type = "warning")
      return()
    }
    
    # Additional safety check for valid coordinates
    if (is.na(as.numeric(values$home_lat)) || is.na(as.numeric(values$home_lng))) {
      showNotification("Invalid starting location coordinates. Please set your location again.", type = "error")
      return()
    }
    
    tryCatch({
      candidates <- available_places()
      if (nrow(candidates) == 0) { showNotification("No unvisited places match your criteria!", type = "warning"); values$suggested <- NULL; return() }
    
    # Get user-selected number of stops
    num_stops <- as.numeric(input$num_stops %||% 1)
    
    if (num_stops == 1) {
      # Single stop: use original logic
      context_for_plan <- if (length(input$context_filter) > 0) input$context_filter[1] else NULL
      plans <- generate_day_plan(candidates, context_for_plan, input$selected_activities, character(0), NULL)
      if (length(plans) > 0 && !is.null(plans[[1]])) {
        plan <- plans[[1]]
        suggested_place <- plan$places[1, , drop = FALSE]
        
        # Calculate distance and determine transit mode
        if (!is.na(suggested_place$distance_mi[1])) {
          transit_mode <- transit_by_distance(suggested_place$distance_mi[1], input$context_filter)
        } else {
          transit_mode <- "🚌 Public Transit"
        }
        
        values$suggested <- suggested_place
        values$inspiration_text <- list(
          title = plan$title, 
          description = paste(transit_to_verb(transit_mode), suggested_place$title[1], simple_activity_from_tags(suggested_place$tags[1], suggested_place$title[1])), 
          type = plan$type, 
          estimated_time = plan$estimated_time,
          transit = transit_mode
        )
      } else {
        chosen_place <- candidates[sample(nrow(candidates), 1), , drop = FALSE]
        
        # Calculate distance and determine transit mode
        if (!is.na(chosen_place$distance_mi[1])) {
          transit_mode <- transit_by_distance(chosen_place$distance_mi[1], input$context_filter)
        } else {
          transit_mode <- "🚌 Public Transit"
        }
        
        values$suggested <- chosen_place
        values$inspiration_text <- list(
          title = "Great Choice",
          description = paste(transit_to_verb(transit_mode), chosen_place$title[1], simple_activity_from_tags(chosen_place$tags[1], chosen_place$title[1])),
          type = "suggestion",
          estimated_time = "1-2 hours",
          transit = transit_mode
        )
      }
    } else {
      # Multi-stop: use adventure generation with specified number of stops
      context_for_adventure <- if (length(input$context_filter) > 0) input$context_filter[1] else NULL
      
      # Create a custom adventure generation that respects the number of stops
      adventure <- generate_guided_multi_stop(
        available_places = candidates,
        num_stops = num_stops,
        context = context_for_adventure,
        home_lat = values$home_lat, home_lng = values$home_lng, home_addr = values$home_address
      )
      
      if (!is.null(adventure)) {
        values$suggested <- adventure$places[1, , drop = FALSE]  # First location for map display
        values$inspiration_text <- list(
          title = adventure$title,
          description = adventure$description,
          type = "guided_plan",
          estimated_time = adventure$estimated_time,
          transit = adventure$transit,
          neighborhood = adventure$neighborhood,
          full_adventure = adventure  # Store full adventure details
        )
      } else {
        showNotification("Unable to generate multi-stop plan with your criteria. Try adjusting filters.", type = "warning")
        values$suggested <- NULL
        values$inspiration_text <- NULL
      }
    }
    }, error = function(e) {
      showNotification(paste("Error generating guided plan:", e$message, "Please try setting your location again."), type = "error")
      values$suggested <- NULL
      values$inspiration_text <- NULL
      return()
    })
  })
  
  observeEvent(input$random_inspiration, {
    # Check if address is set - multiple safety checks
    if (!home_is_set(values$home_address, values$home_lat, values$home_lng) ||
        is.na(values$home_lat) || is.na(values$home_lng) ||
        is.null(values$home_address) || !nzchar(values$home_address)) {
      showNotification("Please set your starting location first! Use 'Set Address' or 'Set Area' to choose where you're starting from.", type = "warning")
      return()
    }
    
    # Additional safety check for valid coordinates
    if (is.na(as.numeric(values$home_lat)) || is.na(as.numeric(values$home_lng))) {
      showNotification("Invalid starting location coordinates. Please set your location again.", type = "error")
      return()
    }
    
    tryCatch({
      all_available <- places[!(places$id %in% values$completed), , drop = FALSE]
      if (nrow(all_available) == 0) { 
        showNotification("You've visited everywhere! Time for new places.", type = "warning")
        values$suggested <- NULL
        return() 
      }
      
      context_for_adventure <- if (length(input$context_filter) > 0) input$context_filter[1] else NULL
      adventures <- generate_surprise_adventure(
        available_places = all_available,
        time_available = NULL,
        context = context_for_adventure,
        home_lat = values$home_lat, home_lng = values$home_lng, home_addr = values$home_address
      )
      
      if (length(adventures) > 0 && !is.null(adventures[[1]])) {
        adventure <- adventures[[1]]
        
        # The new adventure generator always includes proper transit mode, locations, and activities
        values$suggested <- adventure$places[1, , drop = FALSE]  # First location for map display
        values$inspiration_text <- list(
          title = adventure$title,
          description = adventure$description,
          type = adventure$type,
          estimated_time = adventure$estimated_time,
          transit = adventure$transit,
          neighborhood = adventure$neighborhood,
          full_adventure = adventure  # Store full adventure details
        )
      } else {
        showNotification("Unable to generate adventure - try adjusting your filters or check available places.", type = "warning")
        values$suggested <- NULL
        values$inspiration_text <- NULL
      }
    }, error = function(e) {
      showNotification(paste("Error generating surprise adventure:", e$message, "Please try setting your location again."), type = "error")
      values$suggested <- NULL
      values$inspiration_text <- NULL
      return()
    })
  })
  
  # Visited toggles
  observeEvent(input$mark_visited, {
    if (is.null(values$suggested)) { showNotification("Please suggest a place first!", type = "message") }
    else {
      values$completed <- unique(c(values$completed, values$suggested$id))
      save_completed(values$completed)
      showNotification(paste("✅", values$suggested$title, "marked as visited!"), type = "success")
    }
  })
  observeEvent(input$unmark, {
    if (is.null(values$suggested)) { showNotification("Please suggest a place first!", type = "message") }
    else {
      values$completed <- setdiff(values$completed, values$suggested$id)
      save_completed(values$completed)
      showNotification(paste("↩️", values$suggested$title, "unmarked!"), type = "success")
    }
  })
  observeEvent(input$mark_complete, {
    if (is.null(values$suggested)) { 
      showNotification("No suggestion to mark as complete!", type = "warning") 
    } else {
      values$completed <- unique(c(values$completed, values$suggested$id))
      save_completed(values$completed)
      showNotification(paste("✅", values$suggested$title, "marked as complete!"), type = "success")
      # Clear the suggestion after marking complete
      values$suggested <- NULL
      values$inspiration_text <- NULL
    }
  })
  observeEvent(input$reset_visited, {
    values$completed <- character(0); save_completed(values$completed)
    showNotification("♻️ All visited places reset!", type = "info")
  })
  
  # ---- MAP: panes so markers are above polygons (clickable) ----
  output$map <- renderLeaflet({
    center_lat <- if (!is.na(values$home_lat)) values$home_lat else DEFAULT_LAT
    center_lng <- if (!is.na(values$home_lng)) values$home_lng else DEFAULT_LNG
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMapPane("polygons", zIndex = 410) %>%
      addMapPane("markers",  zIndex = 420) %>%
      setView(lng = center_lng, lat = center_lat, zoom = 11) %>%
      htmlwidgets::onRender("function(el,x){this.getContainer().style.cursor='crosshair';}")
  })
  
  
  # Layers
  observe({
    filtered <- filtered_places()
    visited <- places[places$id %in% values$completed, , drop = FALSE]
    suggested <- values$suggested
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    
    # Store current zoom/center to maintain view during updates
    current_zoom <- input$map_zoom %||% 11
    current_center_lat <- input$map_center$lat %||% DEFAULT_LAT
    current_center_lng <- input$map_center$lng %||% DEFAULT_LNG
    
    # Only allow polygon interaction when exploration mode is on
    clickable_now <- isTRUE(values$starting_location_locked)
    pointer_ev <- if (clickable_now) "auto" else "none"
    
    # Only show data points when location is locked (not in location setting mode)
    show_data_points <- isTRUE(values$starting_location_locked)
    
    # Check if we have active suggestions
    has_suggestions <- show_data_points && !is.null(suggested) && nrow(suggested) > 0 && has_coords(suggested)[1]
    
    # ---- Sextant polygons ----
    if (!is.null(sections_boundaries) && !is.null(SEC_NAME_COL) && SEC_NAME_COL %in% names(sections_boundaries)) {
      secs_all <- sections_boundaries
      sx_names_raw <- as.character(secs_all[[SEC_NAME_COL]]); sx_names <- normalize_sextant(sx_names_raw)
      
      # Check both exploration filter AND starting location selection
      explore_selected <- input$section_filter; explore_selected <- if (is.null(explore_selected)) character(0) else normalize_sextant(explore_selected)
      starting_selected <- input$selected_quadrant; starting_selected <- if (is.null(starting_selected) || starting_selected == "") character(0) else normalize_sextant(starting_selected)
      
      # Combine both selections for highlighting
      all_selected <- unique(c(explore_selected, starting_selected))
      
      # In exploration mode (location locked), show all quadrants for selection
      # In location setting mode, only show selected quadrants to reduce clutter
      if (isTRUE(values$starting_location_locked)) {
        # Exploration mode - show all quadrants but highlight selected ones
        quadrants_to_show <- sx_names
      } else {
        # Location setting mode - only show selected quadrants if any are selected
        quadrants_to_show <- if (length(all_selected) > 0) all_selected else sx_names
      }
      
      section_colors <- c("Southwest"="#ff6b6b","Northwest"="#4ecdc4","Southeast"="#45b7d1","Northeast"="#96ceb4","North"="#feca57","South"="#fd79a8","Other"="#a29bfe")
      for (i in seq_along(sx_names)) {
        sx <- sx_names[i]
        # Only draw quadrants that should be shown
        if (!(sx %in% quadrants_to_show)) next
        
        is_selected <- sx %in% all_selected
        base_color <- section_colors[[sx]] %||% section_colors[["Other"]]
        proxy <- proxy %>% addPolygons(
          data = secs_all[i, ],
          fillColor = base_color, fillOpacity = if (is_selected) 0.08 else 0.05,
          color = base_color, weight = if (is_selected) 2.5 else 1.5, opacity = if (is_selected) 0.9 else 0.7,
          group = "sextants", layerId = paste0("sextant::", sx),
          options = pathOptions(pane = "polygons", interactive = TRUE, clickable = TRUE, pointerEvents = "auto"),
          highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.25, bringToFront = FALSE),
          popup = paste0("<b>", sx, "</b>")
        )
      }
    }
    
    # ---- Neighborhood polygons (only when a Sextant is selected and no suggestions active) ----
    # During location setting: only show neighborhoods if starting quadrant is selected
    # During exploration: show neighborhoods for both starting and exploration quadrants
    # Hide neighborhoods when suggestions are active for clean display
    if (!has_suggestions) {
      explore_sx <- input$section_filter
      starting_sx <- input$selected_quadrant
      
      if (isTRUE(values$starting_location_locked)) {
        # Exploration mode - show neighborhoods ONLY for selected exploration quadrants (not starting location)
        all_sx <- if (!is.null(explore_sx) && length(explore_sx) > 0) explore_sx else character(0)
      } else {
        # Location setting mode - only show neighborhoods if starting quadrant is selected
        all_sx <- if (!is.null(starting_sx) && starting_sx != "") starting_sx else character(0)
      }
      
      if (length(all_sx) > 0 &&
          !is.null(neighborhood_boundaries) && !is.null(NEI_NAME_COL) &&
          !is.null(sections_boundaries) && !is.null(SEC_NAME_COL)) {
      # Get selected quadrant boundaries and filter neighborhoods to only those within selected quadrants
      sel_secs <- sections_boundaries[sections_boundaries[[SEC_NAME_COL]] %in% normalize_sextant(all_sx), , drop = FALSE]
      
      # Only show neighborhoods that are within the selected quadrants
      if (nrow(sel_secs) > 0) {
        # Filter neighborhoods to only those that intersect with selected quadrants
        rows_to_draw <- safe_st_intersects_rows(neighborhood_boundaries, sel_secs)
        if (length(rows_to_draw) > 0) {
          neigh_to_draw <- neighborhood_boundaries[rows_to_draw, , drop = FALSE]
        } else {
          neigh_to_draw <- neighborhood_boundaries[0, , drop = FALSE] # Empty
        }
      } else {
        neigh_to_draw <- neighborhood_boundaries[0, , drop = FALSE] # Empty
      }
      
      if (nrow(neigh_to_draw) > 0) {
        nb_names <- as.character(neigh_to_draw[[NEI_NAME_COL]])
        
        # Check both exploration AND starting location neighborhood selections
        explore_nb <- input$neighborhood_filter; if (is.null(explore_nb)) explore_nb <- character(0)
        starting_nb <- input$selected_neighborhood; starting_nb <- if (is.null(starting_nb) || starting_nb == "") character(0) else starting_nb
        all_selected_nb <- unique(c(explore_nb, starting_nb))
        
        for (i in seq_len(nrow(neigh_to_draw))) {
          nb_name <- nb_names[i]; is_selected <- nb_name %in% all_selected_nb
          proxy <- proxy %>% addPolygons(
            data = neigh_to_draw[i, ],
            fillColor = if (is_selected) "#10b981" else "#ccfbf1",
            fillOpacity = if (is_selected) 0.06 else 0.01,
            color = if (is_selected) "#10b981" else "#0ea5a8",
            weight = if (is_selected) 2 else 1.2,
            opacity = if (is_selected) 1 else 0.6,
            group = "neighborhoods", layerId = paste0("neigh::", nb_name),
            label = nb_name,
            options = pathOptions(pane = "overlayPane", interactive = TRUE, clickable = FALSE, pointerEvents = "auto")
          )
        }
      }
      }
    }
    
    # Home marker
    if (home_is_set(values$home_address, values$home_lat, values$home_lng)) {
      proxy <- proxy %>% addCircleMarkers(
        lng = values$home_lng, lat = values$home_lat, label = "Home", popup = values$home_address,
        radius = 8, color = "#16a34a", fillColor = "#16a34a", opacity = 1, fillOpacity = 0.9,
        options = pathOptions(pane = "markers"), layerId = "home_marker"
      )
    }
    
    # Only show other places when there are NO active suggestions
    if (show_data_points && !has_suggestions) {
      # Filtered places (markers on top) - only show when no suggestions are active
      fp <- filtered[has_coords(filtered), , drop = FALSE]
      if (nrow(fp) > 0) {
        dist_txt <- ifelse(is.na(fp$distance_mi), "", paste0("<br>", round(fp$distance_mi, 1), " miles from home"))
        proxy <- proxy %>% addCircleMarkers(
          lng = fp$lng, lat = fp$lat, radius = 6,
          color = "#e08a63", fillColor = "#fef7f4", opacity = 0.95, fillOpacity = 0.85,
          popup = paste0("<b>", fp$title, "</b><br>", fp$tags, dist_txt),
          options = pathOptions(pane = "markers")
        )
      }
      
      # Visited places - only show when no suggestions are active
      if (nrow(visited) > 0) {
        proxy <- proxy %>% addCircleMarkers(
          lng = visited$lng, lat = visited$lat, radius = 5,
          color = "#9ca3af", fillColor = "#9ca3af", opacity = 0.95, fillOpacity = 0.75,
          options = pathOptions(pane = "markers")
        )
      }
    }
    
    # Suggested highlights - different colors for single vs multi-stop, only when location is locked
    if (has_suggestions) {
      # Check if this is a multi-stop adventure
      full_adventure <- values$inspiration_text$full_adventure
      is_multi_stop <- !is.null(full_adventure) && !is.null(full_adventure$num_locations) && full_adventure$num_locations > 1
      
      if (is_multi_stop && !is.null(full_adventure$places) && nrow(full_adventure$places) > 1) {
        # Multi-stop adventure: use different colors for each stop
        adventure_places <- full_adventure$places[has_coords(full_adventure$places), , drop = FALSE]
        stop_colors <- c("#3b82f6", "#ef4444", "#10b981")  # Blue, Red, Green
        stop_names <- c("Stop 1", "Stop 2", "Stop 3")
        
        for (i in 1:min(nrow(adventure_places), 3)) {
          place <- adventure_places[i, ]
          proxy <- proxy %>% addCircleMarkers(
            lng = place$lng, lat = place$lat, radius = 14,
            color = stop_colors[i], fillColor = stop_colors[i], opacity = 1, fillOpacity = 0.9,
            popup = paste0("<b>", stop_names[i], ": ", place$title, "</b>"),
            options = pathOptions(pane = "markers")
          )
        }
        # Don't force zoom - let user control the view
      } else {
        # Single stop: use bright blue
        proxy <- proxy %>% addCircleMarkers(
          lng = suggested$lng, lat = suggested$lat, radius = 14,
          color = "#3b82f6", fillColor = "#3b82f6", opacity = 1, fillOpacity = 0.9,
          popup = paste0("<b>", suggested$title, "</b>"),
          options = pathOptions(pane = "markers")
        )
        # Don't force zoom - let user control the view
      }
    }
    
    # Never force view changes - let user control zoom and pan freely
  })
  
  # Auto-apply rainy context if detected and none chosen
  observe({
    weather <- current_weather()
    if (!is.null(weather) && isTRUE(weather$is_rainy) &&
        (is.null(input$context_filter) || length(input$context_filter) == 0)) {
      updateSelectizeInput(session, "context_filter", selected = "☔ Rainy Weather")
      showNotification("☔ Rainy weather detected - filtering to indoor activities", type = "message")
    }
  })
  
  # Hero suggestion display
  output$hero_suggestion_display <- renderUI({
    if (is.null(values$suggested)) {
      div(class = "hero-empty",
          h2("🎯 What should you do today?"),
          p("First, set your starting location."),
          p("Then hit 'Surprise Me' for a spontaneous adventure, or set your preferences to get a personalized 'Guided Plan'!")
      )
    } else {
      place <- values$suggested
      clean_tags_display <- clean_tags(place$tags)
      nb_disp <- neigh_display_vec(place$neighborhood_geo, place$neighborhood)
      if (!is.null(values$inspiration_text) && is.list(values$inspiration_text)) {
        # Check if this is a multi-location adventure
        full_adventure <- values$inspiration_text$full_adventure
        
        if (!is.null(full_adventure) && !is.null(full_adventure$num_locations) && full_adventure$num_locations > 1) {
          # Multi-location adventure display
          adventure_places <- full_adventure$places
          activities <- full_adventure$activities
          
          # Create location details for multi-stop adventure
          location_details <- lapply(1:nrow(adventure_places), function(i) {
            place_name <- adventure_places$title[i]
            place_url <- adventure_places$url[i]
            activity <- if (length(activities) >= i) activities[i] else "explore"
            div(class = "adventure-stop", 
                style = "margin: 8px 0; padding: 14px 18px; background: rgba(255,255,255,0.6); border-radius: 8px; font-size: 1.3rem; display: flex; justify-content: space-between; align-items: center;",
                div(
                  tags$strong(paste("Stop", i, ":")), " ", place_name, " - ", activity
                ),
                if (nzchar(place_url %||% "")) 
                  tags$a("📍", href = place_url, target = "_blank", 
                         style = "color: var(--text); text-decoration: none; font-size: 1.5rem; margin-left: 12px;",
                         title = "View on Google Maps")
            )
          })
          
          div(class = "hero-card",
              h2(values$inspiration_text$title),
              div(class = "description", style = "margin-bottom: 16px;", "Your multi-stop adventure:"),
              div(class = "adventure-details", location_details),
              div(class = "details", style = "margin-top: 16px;",
                  if (!is.null(values$inspiration_text$estimated_time)) div(class = "detail-chip", paste("⏱️", values$inspiration_text$estimated_time)),
                  if (!is.null(values$inspiration_text$transit)) div(class = "detail-chip", values$inspiration_text$transit)
              ),
              div(style = "margin-top: 20px; text-align: center;",
                  actionButton("mark_complete", "✅ Mark as Complete", 
                               class = "btn-success", 
                               style = "padding: 12px 24px; font-weight: 500;")
              )
          )
        } else {
          # Single location display (existing format)
          div(class = "hero-card",
              h2(values$inspiration_text$title),
              div(class = "description", values$inspiration_text$description),
              div(class = "details",
                  if (!is.na(place$distance_mi)) div(class = "detail-chip", paste(round(place$distance_mi, 1), "miles away")),
                  if (!is.na(nb_disp)) div(class = "detail-chip", nb_disp),
                  if (!is.null(values$inspiration_text$estimated_time)) div(class = "detail-chip", paste("⏱️", values$inspiration_text$estimated_time)),
                  if (!is.null(values$inspiration_text$transit)) div(class = "detail-chip", values$inspiration_text$transit),
                  if (nzchar(place$url)) tags$a("📍 View on Map", href = place$url, target = "_blank", class = "detail-chip", style = "color: var(--text); text-decoration: none;")
              ),
              div(style = "margin-top: 20px; text-align: center;",
                  actionButton("mark_complete", "✅ Mark as Complete", 
                               class = "btn-success", 
                               style = "padding: 12px 24px; font-weight: 500;")
              )
          )
        }
      } else {
        div(class = "hero-card",
            h2("✨ ", place$title),
            div(class = "description", if (nzchar(clean_tags_display)) clean_tags_display else "A great spot to explore!"),
            div(class = "details",
                if (!is.na(place$distance_mi)) div(class = "detail-chip", paste(round(place$distance_mi, 1), "miles away")),
                if (!is.na(nb_disp)) div(class = "detail-chip", nb_disp),
                if (nzchar(place$url)) tags$a("📍 View on Map", href = place$url, target = "_blank", class = "detail-chip", style = "color: var(--text); text-decoration: none;")
            ),
            div(style = "margin-top: 20px; text-align: center;",
                actionButton("mark_complete", "✅ Mark as Complete", 
                             class = "btn-success", 
                             style = "padding: 12px 24px; font-weight: 500;")
            )
        )
      }
    }
  })
  
  # Table + visited preview
  output$places_table <- DT::renderDataTable({
    df <- filtered_places()
    if (nrow(df) == 0) return(NULL)
    df$clean_tags <- sapply(df$tags, clean_tags)
    df$neighborhood_display <- neigh_display_vec(df$neighborhood_geo, df$neighborhood)
    display_df <- df %>%
      dplyr::mutate(distance_mi = round(distance_mi, 1)) %>%
      dplyr::select(dplyr::any_of(c("title", "clean_tags", "feature", "neighborhood_display", "distance_mi"))) %>%
      dplyr::rename(Place = title, Tags = clean_tags, Neighborhood = neighborhood_display, `Distance (mi)` = distance_mi)
    if ("feature" %in% names(df)) names(display_df)[names(display_df) == "feature"] <- "Feature"
    DT::datatable(display_df, options = list(pageLength = 8, scrollX = TRUE, dom = 'tip'))
  })
  output$visited_count <- renderText({ paste("Visited:", length(values$completed), "places") })
  output$visited_preview <- renderUI({
    if (length(values$completed) == 0) {
      div("None yet - start exploring!")
    } else {
      visited_places <- places[places$id %in% values$completed, , drop = FALSE]
      recent <- head(visited_places$title, 5)
      div(
        lapply(recent, function(name) {
          tags$span(
            style = "background: #dcfce7; padding: 2px 6px; margin: 2px; border-radius: 6px; font-size: 11px; display: inline-block;",
            name
          )
        }),
        if (length(values$completed) > 5) p(paste("...", length(values$completed) - 5, "more")) else NULL
      )
    }
  })
}

# ---------------- LAUNCH ----------------
shinyApp(ui, server)
