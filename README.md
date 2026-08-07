# dmwFunctions

A data-only R package providing geodata (`sf`) datasets for the DMW
linguistic annotation tool: survey area boundaries (Erhebungsgebiete) and
Wenker-Orte point/Voronoi datasets.

## Installation

```r
# install.packages("devtools")
devtools::install_github("<org>/dmw_functions_package")
```

## Loading the data

Load the package with `library()`; datasets are then available by name
(lazy-loaded, no need to call `data()` explicitly):

```r
library(dmwFunctions)
library(sf)

plot(EG_Bonn)
plot(Wenkerorte)
```

## Datasets

All datasets are `sf` objects. See `?geodata` and `?voronoi` for full
documentation, including source details.

### Erhebungsgebiete and Wenker-Orte (`?geodata`)

| Dataset | Description |
|---|---|
| `EG_2020_06_24` | Erhebungsgebiete vom 24. Juni 2020 |
| `EG_Bonn` | Erhebungsgebiete im Raum Bonn |
| `EG_DMW` | Erhebungsgebiete des DMW (Deutsch Mittelwort) |
| `EG_Muenster` | Erhebungsgebiete im Raum Münster |
| `EG_Paderborn` | Erhebungsgebiete im Raum Paderborn |
| `EG_Siegen` | Erhebungsgebiete im Raum Siegen |
| `Wenkerorte` | Kombinierte Wenker-Orte aus allen Regionen (Bonn, Münster, Paderborn, Siegen) |

### Voronoi-Zerlegungen der Wenker-Orte (`?voronoi`)

| Dataset | Description |
|---|---|
| `WenkerOrte_Bonn_Voronoi` | Voronoi-Zerlegung der Wenker-Orte im Raum Bonn |
| `WenkerOrte_Muenster_Voronoi` | Voronoi-Zerlegung der Wenker-Orte im Raum Münster |
| `WenkerOrte_Paderborn_Voronoi` | Voronoi-Zerlegung der Wenker-Orte im Raum Paderborn |
| `WenkerOrte_Siegen_Voronoi` | Voronoi-Zerlegung der Wenker-Orte im Raum Siegen |
| `Wenkerorte_Voronoi` | Kombinierte Voronoi-Zerlegung aller Wenker-Orte |

### Raw source files

The original GeoJSON files these datasets were built from are shipped under
`inst/extdata/` and can be located with `system.file()`, e.g.:

```r
system.file("extdata", "EG_Bonn.geojson", package = "dmwFunctions")
```

The scripts used to build the `.rda` datasets from these raw files are in
`data-raw/`.
