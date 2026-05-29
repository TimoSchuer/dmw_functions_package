# Skript zum Erstellen von Voronoi-Zerlegungen aus Wenkerorte-Punkten

library(sf)
library(dplyr)

# Funktion zum Erstellen von Voronoi-Polygonen mit Attributen
create_voronoi_polygons <- function(points_sf) {
  # Voronoi-Diagramm erstellen
  voronoi <- st_voronoi(st_union(points_sf))

  # Die einzelnen Polygone der Voronoi-Zerlegung extrahieren
  voronoi_polys <- st_cast(voronoi, "POLYGON")

  # Voor jedes Polygon den ursprünglichen Punkt finden
  voronoi_sf <- st_sf(
    geometry = voronoi_polys
  )

  # Räumliche Verknüpfung: Welcher Punkt liegt in welchem Voronoi-Polygon?
  # st_distance benutzen, um den nächsten Punkt zu jedem Polygon zu finden
  indices <- st_nearest_feature(points_sf, voronoi_sf)

  # Umgekehrte Richtung: Für jedes Voronoi-Polygon finde den nächsten Punkt
  poly_to_point <- st_nearest_feature(voronoi_sf, points_sf)

  # Attribute des ursprünglichen Punktes an das Voronoi-Polygon anhängen
  voronoi_sf <- voronoi_sf |>
    mutate(
      .before = geometry,
      across(
        everything(),
        ~NA
      )
    )

  # Attribute von den ursprünglichen Punkten kopieren
  for (i in seq_len(nrow(voronoi_sf))) {
    point_idx <- poly_to_point[i]
    voronoi_sf[i, names(st_drop_geometry(points_sf))] <-
      st_drop_geometry(points_sf[point_idx, ])
  }

  return(voronoi_sf)
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
