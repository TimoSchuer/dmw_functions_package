# Skript zum Laden von geojson Geodaten und Speichern als R Datensätze

library(sf)
library(usethis)

# Pfad zum data Ordner
data_dir <- "data"

# EG Datensätze
eg_files <- list.files(
  data_dir,
  pattern = "^EG_.*\\.geojson$",
  full.names = TRUE
)

# Jeden EG Datensatz laden und speichern
for (file in eg_files) {
  # Dateiname ohne Erweiterung
  base_name <- tools::file_path_sans_ext(basename(file))

  # Bindestriche durch Unterstriche ersetzen (optional)
  var_name <- gsub("-", "_", base_name)

  # GeoJSON laden
  data_obj <- st_read(file, quiet = TRUE)

  # Variable mit dynamischem Namen zuweisen
  assign(var_name, data_obj)

  # Speichern mit use_data
  use_data(get(var_name), overwrite = TRUE)

  cat("Loaded and saved:", var_name, "\n")
}

# Wenker-Orte Datensätze kombinieren
wenker_files <- list.files(
  data_dir,
  pattern = "^Wenker.*\\.geojson$",
  full.names = TRUE
)

wenker_data_list <- lapply(wenker_files, function(file) {
  st_read(file, quiet = TRUE)
})

# Alle Wenker-Orte kombinieren
Wenkerorte <- do.call(rbind, wenker_data_list)

# Wenkerorte speichern
use_data(Wenkerorte, overwrite = TRUE)

cat("Loaded and saved: Wenkerorte\n")
