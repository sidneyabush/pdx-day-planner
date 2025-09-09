# app.R — Portland Day Picker (simple & robust)

suppressPackageStartupMessages({
  library(shiny)
  library(sf)
  library(leaflet)
  library(dplyr)
  library(purrr)
  library(stringr)
})

options(shiny.sanitize.errors = FALSE)
sf::sf_use_s2(TRUE)

# ---------------- PATHS ----------------
proj_root <- "/Users/sidneybush/Documents/GitHub/Portland_shiny_app"
geo_dir   <- file.path(proj_root, "out_lists_portland_tagged")
persist_dir <- file.path(proj_root, "data")
dir.create(persist_dir, recursive = TRUE, showWarnings = FALSE)
completed_rds <- file.path(persist_dir, "completed_ids.rds")

# Optional neighborhoods polygon overlay (if any of these exist)
nbhd_candidates <- c(
  file.path(proj_root, "data", "Neighborhood_Boundaries.geojson"),
  file.path(proj_root, "data", "portland_neighborhoods.geojson"),
  file.path(proj_root, "data", "portland_neighborhoods.gpkg")
)
nbhd_path <- {
  exists <- nbhd_candidates[file.exists(nbhd_candidates)]
  if (length(exists) > 0) exists[1] else NA_character_
}

# ---------------- HELPERS ----------------
read_geo <- function(path) {
  x <- tryCatch(st_read(path, quiet = TRUE), error = function(e) NULL)
  if (is.null(x)) return(NULL)
  if (!inherits(x$geometry, "sfc_POINT")) x <- st_centroid(x)
  x$..srcfile <- basename(path)
  x
}

ensure_id <- function(g) {
  if (!("id" %in% names(g))) {
    g$id <- paste0(g$..srcfile, ":", seq_len(nrow(g)))
  } else {
    g$id <- make.unique(as.character(g$id))
  }
  g
}

# If common fields are missing, try to copy from likely synonyms; else create NA
normalize_fields <- function(g) {
  lower <- tolower(names(g))
  get_first <- function(cands) {
    hits <- which(lower %in% cands)
    if (length(hits) > 0) names(g)[hits[1]] else NA_character_
  }
  nm  <- get_first(c("name","title","place","label"))
  ds  <- get_first(c("description","desc","about","notes","note"))
  tg  <- get_first(c("tags","tag","category","categories","type","types"))
  sc  <- get_first(c("section","sections","region","area","group","list","set"))
  nh  <- get_first(c("neighborhood","neighbourhood","nhood","nbhd","hood","district"))
  
  if (!("name" %in% names(g)))         g$name         <- if (!is.na(nm)) g[[nm]] else NA_character_
  if (!("description" %in% names(g)))  g$description  <- if (!is.na(ds)) g[[ds]] else NA_character_
  if (!("tags" %in% names(g)))         g$tags         <- if (!is.na(tg)) g[[tg]] else NA_character_
  if (!("section" %in% names(g)))      g$section      <- if (!is.na(sc)) g[[sc]] else NA_character_
  if (!("neighborhood" %in% names(g))) g$neighborhood <- if (!is.na(nh)) g[[nh]] else NA_character_
  
  # Clean empties to NA
  for (col in c("name","description","tags","section","neighborhood")) {
    v <- g[[col]]
    v <- ifelse(is.na(v) | trimws(as.character(v)) == "", NA_character_, as.character(v))
    g[[col]] <- v
  }
  g
}

to_miles <- function(meters) as.numeric(meters) / 1609.344

compute_distance_mi <- function(home_lat, home_lng, pts) {
  if (is.na(home_lat) || is.na(home_lng) || nrow(pts) == 0) return(rep(NA_real_, nrow(pts)))
  home_pt <- st_sfc(st_point(c(home_lng, home_lat)), crs = 4326)
  as.numeric(st_distance(st_transform(pts, 4326), home_pt)) |> to_miles()
}

passes_keywords <- function(df, keywords) {
  if (length(keywords) == 0) return(rep(TRUE, nrow(df)))
  char_cols <- names(df)[sapply(df, is.character)]
  if (length(char_cols) == 0) return(rep(TRUE, nrow(df)))
  keep <- rep(TRUE, nrow(df))
  for (kw in keywords) {
    kw <- trimws(kw)
    if (kw == "") next
    has_kw <- apply(df[char_cols], 1, function(row) any(str_detect(tolower(row), fixed(tolower(kw)))))
    keep <- keep & has_kw
  }
  keep
}

load_completed <- function() {
  if (file.exists(completed_rds)) {
    out <- tryCatch(readRDS(completed_rds), error = function(e) character(0))
    as.character(out)
  } else character(0)
}
save_completed <- function(ids) saveRDS(as.character(ids), completed_rds)

filter_by_id <- function(sf_obj, id_val) {
  if (is.null(id_val) || length(id_val) == 0 || is.na(id_val)) {
    sf_obj[0, ]
  } else {
    sf_obj[as.character(sf_obj$id) == as.character(id_val), , drop = FALSE]
  }
}

# ---------------- LOAD DATA ----------------
geo_files <- list.files(geo_dir, pattern = "\\.geojson$", full.names = TRUE)
if (length(geo_files) == 0) stop("No .geojson files found in: ", geo_dir)

places <- geo_files |>
  map(read_geo) |>
  compact() |>
  bind_rows() |>
  st_transform(4326) |>
  ensure_id() |>
  normalize_fields()

section_vals     <- sort(unique(na.omit(as.character(places$section))))
neighborhood_vals <- sort(unique(na.omit(as.character(places$neighborhood))))

nbhd_polys <- NULL
if (!is.na(nbhd_path)) {
  nbhd_polys <- tryCatch(st_read(nbhd_path, quiet = TRUE), error = function(e) NULL)
  if (!is.null(nbhd_polys)) nbhd_polys <- st_transform(nbhd_polys, 4326)
}

# ---------------- UI ----------------
ui <- fluidPage(
  titlePanel("Portland Day Picker"),
  
  fluidRow(
    column(
      4,
      div(
        style = "background:#f8f9fa; padding:12px; border-radius:12px;",
        h4("Filters"),
        selectizeInput("sections", "Sections", choices = section_vals, multiple = TRUE),
        selectizeInput("neighborhoods", "Neighborhoods", choices = neighborhood_vals, multiple = TRUE),
        checkboxInput("bikeable", "Bikeable only (<= 10 miles)", FALSE),
        textInput("keyword_text", "Keywords (comma-separated)", ""),
        tags$hr(),
        h5("Home"),
        p("Click on the map to set home or enter lat/lon."),
        numericInput("home_lat", "Lat", value = NA_real_, step = 0.0001),
        numericInput("home_lng", "Lon", value = NA_real_, step = 0.0001),
        actionButton("clear_home", "Clear home"),
        tags$hr(),
        h4("Pick"),
        actionButton("suggest", "🎲 Suggest one", class = "btn-primary"),
        br(), br(),
        actionButton("mark_done", "✅ Mark complete", class = "btn-success"),
        actionButton("unmark_done", "↩️ Unmark"),
        br(), br(),
        actionButton("reset_done", "♻️ Reset all completed", class = "btn-warning"),
        br(), br(),
        h5("Completed"),
        verbatimTextOutput("completed_count"),
        uiOutput("completed_preview")
      )
    ),
    column(
      8,
      leafletOutput("map", height = 620),
      br(),
      h4("Details"),
      uiOutput("chosen_ui"),
      tableOutput("chosen_table")
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  completed_ids <- reactiveVal(load_completed())
  chosen_id <- reactiveVal(NULL)
  
  observeEvent(input$clear_home, {
    updateNumericInput(session, "home_lat", value = NA_real_)
    updateNumericInput(session, "home_lng", value = NA_real_)
  })
  
  observeEvent(input$map_click, {
    cl <- input$map_click
    updateNumericInput(session, "home_lat", value = cl$lat)
    updateNumericInput(session, "home_lng", value = cl$lng)
  })
  
  distances_mi <- reactive({
    compute_distance_mi(input$home_lat, input$home_lng, places)
  })
  
  filtered <- reactive({
    df <- places
    df$distance_mi <- distances_mi()
    
    if (!is.null(input$sections) && length(input$sections) > 0) {
      df <- df[df$section %in% input$sections, , drop = FALSE]
    }
    if (!is.null(input$neighborhoods) && length(input$neighborhoods) > 0) {
      df <- df[df$neighborhood %in% input$neighborhoods, , drop = FALSE]
    }
    if (isTRUE(input$bikeable)) {
      if (!is.na(input$home_lat) && !is.na(input$home_lng)) {
        df <- df[!is.na(df$distance_mi) & df$distance_mi <= 10, , drop = FALSE]
      } else {
        df <- df[0, ]
      }
    }
    
    kws <- str_split(ifelse(is.null(input$keyword_text), "", input$keyword_text), ",")[[1]]
    keep <- passes_keywords(st_drop_geometry(df), kws)
    df[keep, , drop = FALSE]
  })
  
  candidates <- reactive({
    df <- filtered()
    ex <- as.character(completed_ids())
    df[!(as.character(df$id) %in% ex), , drop = FALSE]
  })
  
  observeEvent(input$suggest, {
    pool <- candidates()
    if (nrow(pool) == 0) {
      showNotification("No matching places. Adjust filters or clear completed.", type = "warning")
      chosen_id(NULL)
    } else {
      chosen_id(as.character(sample(pool$id, 1)))
    }
  })
  
  observeEvent(input$mark_done, {
    cid <- chosen_id()
    if (is.null(cid)) {
      showNotification("Pick a place first.", type = "message")
    } else {
      new <- unique(c(as.character(completed_ids()), as.character(cid)))
      completed_ids(new); save_completed(new)
    }
  })
  
  observeEvent(input$unmark_done, {
    cid <- chosen_id()
    if (is.null(cid)) {
      showNotification("Pick a place first.", type = "message")
    } else {
      new <- setdiff(as.character(completed_ids()), as.character(cid))
      completed_ids(new); save_completed(new)
    }
  })
  
  observeEvent(input$reset_done, {
    completed_ids(character(0)); save_completed(character(0))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = -122.6765, lat = 45.5231, zoom = 12)
  })
  
  observe({
    df_all    <- filtered()
    df_done   <- places[as.character(places$id) %in% as.character(completed_ids()), , drop = FALSE]
    df_chosen <- filter_by_id(places, chosen_id())
    
    proxy <- leafletProxy("map")
    proxy %>% clearMarkers() %>% clearShapes()
    
    if (!is.na(nbhd_path) && !is.null(nbhd_polys)) {
      proxy %>% addPolygons(data = nbhd_polys, weight = 1, color = "#666666", fill = FALSE,
                            group = "Neighborhoods") %>%
        addLayersControl(overlayGroups = "Neighborhoods",
                         options = layersControlOptions(collapsed = TRUE))
    }
    
    if (nrow(df_all) > 0) {
      proxy %>% addCircleMarkers(data = df_all, radius = 5, color = "#666666",
                                 opacity = 0.9, fillOpacity = 0.6,
                                 label = ~ ifelse(!is.na(name), name, as.character(id)))
    }
    if (nrow(df_done) > 0) {
      proxy %>% addCircleMarkers(data = df_done, radius = 6, color = "#2e7d32",
                                 fillColor = "#2e7d32", fillOpacity = 0.7)
    }
    if (nrow(df_chosen) > 0) {
      proxy %>% addCircleMarkers(data = df_chosen, radius = 8, color = "#1976d2",
                                 fillColor = "#1976d2", fillOpacity = 0.9)
      bb <- st_bbox(df_chosen)
      proxy %>% fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
    if (!is.na(input$home_lat) && !is.na(input$home_lng)) {
      proxy %>% addMarkers(lng = input$home_lng, lat = input$home_lat, label = "Home")
    }
  })
  
  output$chosen_ui <- renderUI({
    cid <- chosen_id()
    if (is.null(cid)) return(div(em("Click “Suggest one” to pick a place.")))
    row <- filter_by_id(places, cid)
    if (nrow(row) == 0) return(NULL)
    
    dist <- NA_real_
    if (!is.na(input$home_lat) && !is.na(input$home_lng)) {
      dist <- compute_distance_mi(input$home_lat, input$home_lng, row)
    }
    
    div(
      h4(ifelse(!is.na(row$name), row$name, cid)),
      if (!is.na(row$section)) div(HTML(paste0("<i>Section:</i> ", row$section))),
      if (!is.na(row$neighborhood)) div(HTML(paste0("<i>Neighborhood:</i> ", row$neighborhood))),
      if (!is.na(dist)) div(sprintf("Distance: %.2f miles", dist))
    )
  })
  
  output$chosen_table <- renderTable({
    cid <- chosen_id()
    if (is.null(cid)) return(NULL)
    row <- filter_by_id(places, cid) %>% st_drop_geometry()
    if (nrow(row) == 0) return(NULL)
    
    if (!is.na(input$home_lat) && !is.na(input$home_lng)) {
      dmi <- compute_distance_mi(input$home_lat, input$home_lng, filter_by_id(places, cid))
      row$distance_mi <- round(dmi, 2)
    }
    
    preferred <- c("id","name","section","neighborhood","tags","description","distance_mi","..srcfile")
    cols <- c(intersect(preferred, names(row)), setdiff(names(row), preferred))
    row[, cols, drop = FALSE]
  })
  
  output$completed_count <- renderText({
    n <- length(completed_ids())
    sprintf("Completed: %d", n)
  })
  
  output$completed_preview <- renderUI({
    ids <- completed_ids()
    if (length(ids) == 0) return(div("(none yet)"))
    df <- places[as.character(places$id) %in% ids, , drop = FALSE] %>% st_drop_geometry()
    nm <- if ("name" %in% names(df)) df$name else df$id
    tags$ul(lapply(head(nm, 10), function(x) tags$li(x)))
  })
}

shinyApp(ui, server)
