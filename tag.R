suppressPackageStartupMessages({
  library(sf); library(dplyr); library(readr); library(stringi); library(tibble)
})

setwd("~/Documents/GitHub/Portland_shiny_app")
source("tag_points_with_areas.R")

dir.create("data", showWarnings = FALSE)
cm_path <- "data/neighborhood_combo_map.csv"

# Create skeleton once if missing
if (!file.exists(cm_path)) {
  write_combo_skeleton("data/Neighborhood_Boundaries.geojson", cm_path)
}

# Make sure Downtown mapping exists (normalize + append if missing)
norm_name <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  tolower(trimws(gsub("\\s+", " ", x)))
}

cm <- readr::read_csv(cm_path, show_col_types = FALSE)
if (!any(norm_name(cm$neighborhood) == "portland downtown")) {
  cm <- dplyr::bind_rows(
    cm,
    tibble(neighborhood = "PORTLAND DOWNTOWN", combo_label = "Downtown")
  ) |>
    dplyr::distinct(neighborhood, .keep_all = TRUE) |>
    dplyr::arrange(neighborhood)
  readr::write_csv(cm, cm_path)
  message("Added mapping: PORTLAND DOWNTOWN → Downtown")
}

library(readr); library(dplyr); library(stringi); library(tibble)

cm_path <- "data/neighborhood_combo_map.csv"

# create the skeleton if it doesn't exist
if (!file.exists(cm_path)) {
  write_combo_skeleton(
    neighborhoods_path = "data/Neighborhood_Boundaries.geojson",
    out_csv = cm_path
  )
}

norm_name <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  tolower(trimws(gsub("\\s+", " ", x)))
}

cm <- readr::read_csv(cm_path, show_col_types = FALSE)

# ensure mapping exists (insert or fill)
has <- norm_name(cm$neighborhood) == "portland downtown"
if (!any(has)) {
  cm <- bind_rows(cm, tibble(neighborhood = "PORTLAND DOWNTOWN", combo_label = "Downtown"))
} else {
  cm$combo_label[has] <- ifelse(is.na(cm$combo_label[has]) | cm$combo_label[has] == "",
                                "Downtown", cm$combo_label[has])
}
cm <- distinct(cm, neighborhood, .keep_all = TRUE) |> arrange(neighborhood)
write_csv(cm, cm_path)
message("✓ ensured mapping: PORTLAND DOWNTOWN → Downtown")

tag_points_with_areas(
  in_dir  = "out_lists_portland",
  out_dir = "out_lists_portland_tagged",
  neighborhoods_path = "data/Neighborhood_Boundaries.geojson",
  sextants_path      = "data/Portland_Administrative_Sextants.geojson",
  combo_map_csv      = cm_path
)
