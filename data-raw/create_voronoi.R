# Skript zum Erstellen von Voronoi-Zerlegungen aus Wenkerorte-Punkten

library(sf)
library(dplyr)

# Funktion zum Erstellen von Voronoi-Polygonen mit Attributen
create_voronoi_polygons <- function(points_sf) {
  original_crs <- sf::st_crs(points_sf)

  # st_voronoi requires planar (projected) coordinates.
  # EPSG:25832 = ETRS89 / UTM zone 32N — suitable for Germany.
  points_proj <- sf::st_transform(points_sf, 25832)

  # Compute Voronoi on projected data
  voronoi_raw   <- st_voronoi(st_union(points_proj))
  voronoi_polys <- sf::st_collection_extract(voronoi_raw, "POLYGON")
  voronoi_sf    <- st_sf(geometry = voronoi_polys)

  # Match each Voronoi polygon to its nearest input point → copy attributes
  poly_to_point <- st_nearest_feature(voronoi_sf, points_proj)
  voronoi_sf    <- sf::st_sf(
    as.data.frame(st_drop_geometry(points_sf[poly_to_point, ])),
    geometry = sf::st_geometry(voronoi_sf)
  )

  # Return in the original CRS
  sf::st_transform(voronoi_sf, original_crs)
}

# Pfad zum data Ordner
data_dir <- "data"

# Wenkerorte-Dateien für regionale Voronoi-Diagramme
wenker_regions <- list(
  list(
    file = file.path(data_dir, "Wenker-Orte_Bonn.geojson"),
    name = "WenkerOrte_Bonn_Voronoi"
  ),
  list(
    file = file.path(data_dir, "Wenker-Orte_Muenster.geojson"),
    name = "WenkerOrte_Muenster_Voronoi"
  ),
  list(
    file = file.path(data_dir, "Wenker-Orte_Paderborn.geojson"),
    name = "WenkerOrte_Paderborn_Voronoi"
  ),
  list(
    file = file.path(data_dir, "Wenker-Orte_Siegen.geojson"),
    name = "WenkerOrte_Siegen_Voronoi"
  )
)

# Voronoi-Diagramme für jede Region erstellen
for (region in wenker_regions) {
  if (!file.exists(region$file)) {
    cat("File not found:", region$file, "\n")
    next
  }

  # Wenker-Orte laden
  points <- st_read(region$file, quiet = TRUE)

  # Voronoi-Polygone erstellen
  voronoi_data <- create_voronoi_polygons(points)

  # Dynamisch zuweisen
  assign(region$name, voronoi_data)

  # Als GeoJSON speichern
  geojson_path <- file.path(
    data_dir,
    paste0(region$name, ".geojson")
  )
  st_write(voronoi_data, geojson_path, delete_dsn = TRUE, quiet = TRUE)

  # Als .rda Datei speichern
  rda_path <- file.path(
    data_dir,
    paste0(region$name, ".rda")
  )
  save(list = region$name, file = rda_path, envir = environment())

  cat("Created and saved:", region$name, "\n")
}

# Kombinierte Voronoi-Zerlegung aller Wenkerorte
all_wenker_files <- c(
  file.path(data_dir, "Wenker-Orte_Bonn.geojson"),
  file.path(data_dir, "Wenker-Orte_Muenster.geojson"),
  file.path(data_dir, "Wenker-Orte_Paderborn.geojson"),
  file.path(data_dir, "Wenker-Orte_Siegen.geojson")
)

all_points <- do.call(
  rbind,
  lapply(all_wenker_files, function(file) {
    st_read(file, quiet = TRUE)
  })
)

# Voronoi-Zergliederung für alle Punkte
Wenkerorte_Voronoi <- create_voronoi_polygons(all_points)

# Speichern
st_write(
  Wenkerorte_Voronoi,
  file.path(data_dir, "Wenkerorte_Voronoi.geojson"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# Als .rda Datei speichern
save(Wenkerorte_Voronoi, file = file.path(data_dir, "Wenkerorte_Voronoi.rda"))

cat("Created and saved: Wenkerorte_Voronoi\n")
