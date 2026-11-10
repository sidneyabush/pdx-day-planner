# ---- coffee passport records ----
COFFEE_STRENGTHS <- c("Espresso", "Filter coffee", "Latte art", "Pastries", "Reading",
  "Drawing", "Working", "Meeting friends", "Outdoor seating", "Bike stop")
COFFEE_RATINGS <- c("quality", "atmosphere", "workspace", "value")

# check browser data and backups before using any saved entries
read_coffee_passport <- function(text) {
  if (!is.character(text) || length(text) != 1 || nchar(text, type = "bytes") > 2000000) {
    stop("This backup is too large or unreadable.")
  }
  data <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  if (!is.list(data) || !identical(data$version, 1L) || !is.list(data$entries) || length(data$entries) > 1000) {
    stop("Choose a Coffee Passport backup exported by this app.")
  }
  entries <- data$entries
  if (length(entries) && (is.null(names(entries)) || anyDuplicated(names(entries)) ||
      any(!nzchar(names(entries))) || any(nchar(names(entries)) > 200))) {
    stop("The backup contains invalid café names.")
  }
  for (entry in entries) {
    if (!is.list(entry) || !is.logical(entry$visited) || length(entry$visited) != 1 || is.na(entry$visited)) {
      stop("The backup contains an invalid visit entry.")
    }
    for (field in COFFEE_RATINGS) {
      rating <- entry[[field]]
      if (!is.numeric(rating) || length(rating) != 1 || is.na(rating) || !rating %in% 0:5) {
        stop("Ratings must be whole numbers from 1 to 5, or 0 for not rated.")
      }
    }
    for (field in c("date", "order", "notes")) {
      if (!is.character(entry[[field]]) || length(entry[[field]]) != 1 ||
          is.na(entry[[field]]) || nchar(entry[[field]]) > 10000) {
        stop("The backup contains an invalid note or date.")
      }
    }
    if (nzchar(entry$date) && (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", entry$date) ||
        is.na(as.Date(entry$date, format = "%Y-%m-%d")))) stop("A visit date is invalid.")
    strengths <- unlist(entry$strengths, use.names = FALSE)
    if (length(strengths) > length(COFFEE_STRENGTHS) || !all(strengths %in% COFFEE_STRENGTHS)) {
      stop("The backup contains an unknown café feature.")
    }
  }
  entries
}

coffee_passport_json <- function(entries) {
  jsonlite::toJSON(list(version = 1L, entries = entries), auto_unbox = TRUE, null = "null")
}

# ---- coffee passport server ----
coffee_passport_server <- function(input, output, session, places) {
  cafes <- places[vapply(places$tags, function(tags) {
    matches_activity(tags, c("coffee", "cafe", "espresso"))
  }, logical(1)), , drop = FALSE]
  cafes <- cafes[order(cafes$title, cafes$id), , drop = FALSE]
  cafe_labels <- cafes$title
  repeated <- duplicated(cafe_labels) | duplicated(cafe_labels, fromLast = TRUE)
  cafe_labels[repeated] <- paste(cafe_labels[repeated], cafes$neighborhood[repeated], sep = " — ")
  cafe_labels <- make.unique(cafe_labels, sep = " · ")
  entries <- reactiveVal(list())
  ready <- reactiveVal(FALSE)
  status <- reactiveVal("Loading your saved passport…")
  pending <- reactiveVal(NULL)
  request_number <- 0L

  # only accept a save after the browser confirms it was written successfully
  save_in_browser <- function(updated, message) {
    request_number <<- request_number + 1L
    pending(list(entries = updated, message = message, request = request_number))
    session$sendCustomMessage("coffee_store", list(json = as.character(coffee_passport_json(updated)), request = request_number))
  }
  observeEvent(input$coffee_loaded, {
    tryCatch({
      entries(read_coffee_passport(input$coffee_loaded))
      ready(TRUE)
      status("Your entries are saved in this browser. Export a backup to keep a separate copy.")
    }, error = function(e) {
      ready(FALSE)
      status(paste("Could not load saved entries:", conditionMessage(e)))
    })
  })
  observeEvent(input$coffee_storage_error, {
    status("Browser storage is unavailable. Allow site storage or try a regular browser window.")
  })
  observeEvent(input$coffee_storage_ack, {
    saved <- pending()
    req(saved, identical(as.integer(input$coffee_storage_ack$request), saved$request))
    if (isTRUE(input$coffee_storage_ack$ok)) {
      entries(saved$entries)
      ready(TRUE)
      status(saved$message)
    } else {
      status("The browser could not save this change. Your previous entries are still intact. Export a backup and check site storage.")
    }
    pending(NULL)
  })
  output$coffee_save_status <- renderText(status())

  # use the same café list for the selector and journal table
  available_cafes <- reactive({
    visited <- vapply(cafes$id, function(id) isTRUE(entries()[[id]]$visited), logical(1))
    keep <- switch(input$coffee_show %||% "All cafés",
      "Visited" = visited, "To visit" = !visited, rep(TRUE, nrow(cafes)))
    which(keep)
  })
  observe({
    rows <- available_cafes()
    current <- isolate(input$coffee_shop)
    ids <- cafes$id[rows]
    selected <- if (length(current) == 1 && current %in% ids) current else if (length(ids)) ids[1] else ""
    updateSelectizeInput(session, "coffee_shop", choices = setNames(ids, cafe_labels[rows]),
      selected = selected, server = TRUE)
  })
  observe({
    req(input$coffee_shop, input$coffee_shop %in% cafes$id)
    entry <- entries()[[input$coffee_shop]]
    updateCheckboxInput(session, "coffee_visited", value = isTRUE(entry$visited))
    updateDateInput(session, "coffee_date", value = if (nzchar(entry$date %||% "")) entry$date else Sys.Date())
    for (field in COFFEE_RATINGS) {
      updateRadioButtons(session, paste0("coffee_", field), selected = entry[[field]] %||% 0)
    }
    updateCheckboxGroupInput(session, "coffee_strengths", selected = if (length(entry$strengths)) unlist(entry$strengths) else character(0))
    updateTextAreaInput(session, "coffee_order", value = entry$order %||% "")
    updateTextAreaInput(session, "coffee_notes", value = entry$notes %||% "")
  })
  output$coffee_link <- renderUI({
    req(input$coffee_shop)
    cafe <- cafes[match(input$coffee_shop, cafes$id), , drop = FALSE]
    if (nrow(cafe) && !is.na(cafe$url) && startsWith(cafe$url, "https://")) {
      tags$a(icon("map-marker-alt"), "Find this café", href = cafe$url, target = "_blank", rel = "noopener")
    }
  })
  output$coffee_progress <- renderUI({
    visited <- sum(vapply(cafes$id, function(id) isTRUE(entries()[[id]]$visited), logical(1)))
    div(class = "passport-progress", tags$strong(paste(visited, "of", nrow(cafes))), "cafés visited")
  })
  observeEvent(input$coffee_save, {
    req(input$coffee_shop %in% cafes$id)
    if (!ready() || !is.null(pending())) {
      showNotification("Wait for browser storage to finish loading or saving.", type = "warning")
      return()
    }
    updated <- entries()
    entry <- list(visited = isTRUE(input$coffee_visited),
      date = if (isTRUE(input$coffee_visited)) as.character(input$coffee_date) else "",
      order = input$coffee_order %||% "", notes = input$coffee_notes %||% "",
      strengths = as.list(input$coffee_strengths %||% character(0)))
    for (field in COFFEE_RATINGS) entry[[field]] <- as.integer(input[[paste0("coffee_", field)]])
    updated[[input$coffee_shop]] <- entry
    tryCatch({
      read_coffee_passport(coffee_passport_json(updated))
      save_in_browser(updated, "Saved in this browser.")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })

  # clear only the selected cafe after confirmation
  observeEvent(input$coffee_clear, {
    req(ready(), input$coffee_shop %in% cafes$id, is.null(pending()))
    showModal(modalDialog(title = "Clear this café entry?",
      "This removes its visit, ratings, and notes from this browser.",
      footer = tagList(modalButton("Cancel"), actionButton("coffee_confirm_clear", "Clear entry", class = "btn-primary"))))
  })
  observeEvent(input$coffee_confirm_clear, {
    req(ready(), input$coffee_shop %in% cafes$id, is.null(pending()))
    updated <- entries()
    updated[[input$coffee_shop]] <- NULL
    removeModal()
    save_in_browser(updated, "Café entry cleared in this browser.")
  })

  output$coffee_table <- DT::renderDataTable({
    rows <- available_cafes()
    list_entries <- lapply(cafes$id[rows], function(id) entries()[[id]])
    display <- data.frame(Café = cafe_labels[rows], check.names = FALSE)
    display$Visited <- vapply(list_entries, function(e) if (isTRUE(e$visited)) "Yes" else "Not yet", character(1))
    display$`Last visit` <- vapply(list_entries, function(e) e$date %||% "", character(1))
    for (field in COFFEE_RATINGS) {
      display[[switch(field, quality = "Coffee", atmosphere = "Atmosphere", workspace = "Workspace", value = "Value")]] <-
        vapply(list_entries, function(e) if (is.null(e[[field]]) || e[[field]] == 0) NA_real_ else as.numeric(e[[field]]), numeric(1))
    }
    display$Highlights <- vapply(list_entries, function(e) paste(unlist(e$strengths), collapse = ", "), character(1))
    DT::datatable(display, rownames = FALSE, selection = "single", escape = TRUE,
      options = list(pageLength = 10, scrollX = TRUE, language = list(emptyTable = "No cafés in this view yet.", search = "Search cafés:")))
  }, server = TRUE)
  observeEvent(input$coffee_table_rows_selected, {
    rows <- available_cafes()
    selected <- input$coffee_table_rows_selected
    if (length(selected) == 1 && selected %in% seq_along(rows)) {
      updateSelectizeInput(session, "coffee_shop", selected = cafes$id[rows[selected]])
    }
  })
  output$coffee_export <- downloadHandler(
    filename = function() paste0("coffee-passport-", Sys.Date(), ".json"),
    content = function(file) {
      req(ready())
      writeLines(coffee_passport_json(entries()), file, useBytes = TRUE)
    }, contentType = "application/json"
  )
  imported <- reactiveVal(NULL)
  observeEvent(input$coffee_import, {
    req(input$coffee_import)
    tryCatch({
      if (input$coffee_import$size > 2000000) stop("Choose a backup smaller than 2 MB.")
      imported(read_coffee_passport(paste(readLines(input$coffee_import$datapath, warn = FALSE), collapse = "\n")))
      showModal(modalDialog(title = "Restore this passport?",
        "This replaces the entries in this browser. Export your current passport first if you want to keep it.",
        footer = tagList(modalButton("Cancel"), actionButton("coffee_restore", "Replace saved passport", class = "btn-primary"))))
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  observeEvent(input$coffee_restore, {
    req(!is.null(imported()), is.null(pending()))
    removeModal()
    save_in_browser(imported(), "Backup restored in this browser. Choose a café to see its entry.")
    imported(NULL)
  })
}
# ---- coffee passport layout ----
coffee_passport_ui <- function() {
  div(class = "coffee-passport",
    div(class = "passport-heading",
      div(p(class = "eyebrow", "YOUR PORTLAND COFFEE JOURNAL"),
        h2("Coffee Passport"),
        p("Find your next favorite. Keep track of every cup, cozy corner, and place worth returning to.")),
      icon("mug-hot")
    ),
    p(class = "passport-storage-note", "Saved only in this browser. Use Export backup before clearing browser data or moving devices."),
    uiOutput("coffee_progress"),
    fluidRow(
      column(4,
        div(class = "passport-card",
          h3("Choose a café"),
          selectizeInput("coffee_shop", "Coffee shop", choices = NULL,
            options = list(placeholder = "Search your coffee list")),
          radioButtons("coffee_show", "Browse", inline = TRUE,
            choices = c("All cafés", "To visit", "Visited")),
          uiOutput("coffee_link"),
          div(class = "passport-backups",
            downloadButton("coffee_export", "Export backup", icon = icon("download")),
            fileInput("coffee_import", "Restore a backup", accept = ".json")
          )
        )
      ),
      column(8,
        div(class = "passport-card",
          h3("Your visit"),
          checkboxInput("coffee_visited", "I've been here", FALSE),
          dateInput("coffee_date", "Most recent visit", value = Sys.Date()),
          div(class = "coffee-ratings",
            lapply(c("Coffee", "Atmosphere", "Seating / workspace", "Value"), function(label) {
              id <- c("Coffee" = "coffee_quality", "Atmosphere" = "coffee_atmosphere",
                "Seating / workspace" = "coffee_workspace", "Value" = "coffee_value")[[label]]
              radioButtons(id, paste0(label, " (1–5)"),
                choices = c("Not rated" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                selected = 0, inline = TRUE)
            })
          ),
          checkboxGroupInput("coffee_strengths", "What is it good for?", inline = TRUE,
            choices = c("Espresso", "Filter coffee", "Latte art", "Pastries", "Reading",
              "Drawing", "Working", "Meeting friends", "Outdoor seating", "Bike stop")),
          textAreaInput("coffee_order", "What did you order?", rows = 2,
            placeholder = "Your drink, pastry, and anything to try next time"),
          textAreaInput("coffee_notes", "Your notes", rows = 4,
            placeholder = "Favorite seat, music, light, outlets, or a reason to come back"),
          div(class = "passport-actions",
            actionButton("coffee_save", "Save café", icon = icon("check"), class = "btn-primary"),
            actionButton("coffee_clear", "Clear entry", class = "btn-default"),
            textOutput("coffee_save_status", inline = TRUE)
          )
        )
      )
    ),
    div(class = "passport-card",
      h3("Your coffee list"),
      DT::dataTableOutput("coffee_table")
    )
  )
}
coffee_styles <- "/* warm paper, forest green, and quieter controls */\n:root {\n  --bg:#f6f3ec; --bg-grad:#f6f3ec; --card:#fffdf8; --border:#dcded3;\n  --text:#28372f; --muted:#657269; --text-secondary:#657269; --primary:#355e49;\n  --accent:#355e49; --accent-600:#284a38; --accent-50:#edf2e9; --accent-hover:#284a38;\n  --secondary:#c6d2bc; --secondary-600:#a4b99a; --secondary-50:#edf2e9;\n  --tertiary:#e5d9c3; --success:#456b50;\n  --shadow-soft:0 3px 16px rgba(35,52,40,.05);\n  --shadow-medium:0 6px 24px rgba(35,52,40,.09);\n}\nbody {font-size:16px}\n.container-fluid {max-width:1500px; padding:24px}\nh1,h2,h3 {letter-spacing:-.025em}\n.header h1,.passport-heading h2 {font-family:Georgia,serif; font-weight:500}\n.header {margin:0 0 24px; border-radius:20px; padding:32px}\n.header-content-row {width:100%; max-width:960px; gap:32px}\n.app-weather-corner {position:static; flex-direction:row; justify-content:flex-end; margin-bottom:16px}\n.time-display,.weather-display {box-shadow:none; min-width:0; border:1px solid var(--border)}\n.nav-tabs {border-bottom:1px solid var(--border); margin-bottom:28px; display:flex; gap:8px}\n.nav-tabs>li>a {color:var(--muted); border-radius:10px 10px 0 0; padding:14px 22px; font-weight:600}\n.nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover {color:var(--accent); background:var(--card); border-color:var(--border); border-bottom-color:var(--card)}\n.passport-heading {display:flex; justify-content:space-between; align-items:center; gap:24px; margin:12px 0 28px}\n.passport-heading h2 {font-size:42px; margin:6px 0 12px}\n.passport-heading>i {font-size:52px; color:var(--accent); padding:24px; background:#e5ecde; border-radius:22px}\n.eyebrow {font-size:11px; letter-spacing:.16em; font-weight:700; color:var(--accent)}\n.passport-card {background:var(--card); border:1px solid var(--border); border-radius:18px; padding:26px; margin-bottom:24px; box-shadow:var(--shadow-soft)}\n.passport-card h3 {font-size:22px; margin:0 0 24px}\n.passport-progress {background:#e7eddf; padding:20px 26px; border-radius:14px; margin-bottom:24px}\n.passport-progress strong {font-size:24px; color:var(--accent); margin-right:12px}\n.coffee-ratings {display:grid; grid-template-columns:1fr 1fr; gap:12px 24px; margin:20px 0}\n.passport-card .shiny-input-container {width:100%}\n.passport-card input[type=radio],.passport-card input[type=checkbox] {accent-color:var(--accent)}\n.passport-card textarea {min-height:70px; resize:vertical}\n.passport-card .radio-inline,.passport-card .checkbox-inline {margin:0 12px 8px 0}\n.passport-actions {display:flex; align-items:center; gap:16px; flex-wrap:wrap}\n.passport-backups {border-top:1px solid var(--border); margin-top:24px; padding-top:20px}\n.passport-backups .shiny-input-container {margin-top:18px}\n.btn-primary:hover,.btn-outline-primary:hover,.image-container:hover {transform:none!important; box-shadow:none!important}\n.control-panel,.location-control-panel,.data-container {border:1px solid var(--border); box-shadow:var(--shadow-soft)}\n@media(max-width:768px) {\n  .container-fluid {padding:14px}\n  .coffee-ratings {grid-template-columns:1fr}\n  .passport-card {padding:20px}\n  .passport-heading h2 {font-size:34px}\n  .passport-heading>i {display:none}\n  .header {padding:20px}\n  .app-weather-corner {flex-wrap:wrap}\n  .nav-tabs>li>a {padding:12px 16px}\n}"
coffee_storage_script <- "// keep the passport on this device and report storage failures honestly\n(function () {\n  const key = 'pdx-coffee-passport-v1';\n  $(document).on('shiny:connected', function () {\n    try {\n      const saved = localStorage.getItem(key) || JSON.stringify({version: 1, entries: {}});\n      Shiny.setInputValue('coffee_loaded', saved, {priority: 'event'});\n    } catch (error) {\n      Shiny.setInputValue('coffee_storage_error', Date.now(), {priority: 'event'});\n    }\n  });\n  Shiny.addCustomMessageHandler('coffee_store', function (data) {\n    let ok = false;\n    try {\n      localStorage.setItem(key, data.json);\n      ok = true;\n    } catch (error) {\n      ok = false;\n    }\n    Shiny.setInputValue('coffee_storage_ack', {ok: ok, request: data.request}, {priority: 'event'});\n  });\n})();"
