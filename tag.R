source("tag_points_with_areas.R")

tag_points_with_areas(
  in_dir  = "out_lists_portland",
  out_dir = "out_lists_portland_tagged",
  neighborhoods_path = "data/Neighborhood_Boundaries.geojson",
  sextants_path      = "data/Portland_Administrative_Sextants.geojson",
  combo_map_csv      = "data/neighborhood_combo_map.csv"
)

# sanity check
pts2 <- sf::st_read("out_lists_portland_tagged/ALL_combined_dedup_tagged.geojson", quiet = TRUE)
sum(pts2$combo_label == pts2$neighborhood, na.rm = TRUE)  # should now be 0
