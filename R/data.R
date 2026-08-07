#' Geodaten von Erhebungsgebieten
#'
#' Diese Datensätze enthalten Geodaten (sf-Objekte) mit Erhebungsgebieten
#' (EG) und Wenker-Orte als GeoJSON-Daten.
#'
#' @format Ein sf-Objekt (simple features) mit Multipolygon oder Point Geometrien.
#' Die genaue Struktur variiert je nach Datensatz.
#'
#' @details
#' - `EG_2020_06_24`: Erhebungsgebiete vom 24. Juni 2020
#' - `EG_Bonn`: Erhebungsgebiete im Raum Bonn
#' - `EG_DMW`: Erhebungsgebiete des DMW (Deutsch Mittelwort)
#' - `EG_Muenster`: Erhebungsgebiete im Raum Münster
#' - `EG_Paderborn`: Erhebungsgebiete im Raum Paderborn
#' - `EG_Siegen`: Erhebungsgebiete im Raum Siegen
#' - `Wenkerorte`: Kombinierte Wenker-Orte aus allen Regionen (Bonn, Münster, Paderborn, Siegen)
#'
#' @source Ursprünglich als GeoJSON-Dateien im Ordner `data` verfügbar
#'
#' @name geodata
#' @docType data
#' @keywords datasets
#'
#' @examples
#' \dontrun{
#'   library(sf)
#'
#'   # Erhebungsgebiet Bonn
#'   plot(EG_Bonn)
#'
#'   # Wenker-Orte
#'   plot(Wenkerorte)
#' }
#'
NULL

#' @rdname geodata
"EG_2020_06_24"

#' @rdname geodata
"EG_Bonn"

#' @rdname geodata
"EG_DMW"

#' @rdname geodata
"EG_Muenster"

#' @rdname geodata
"EG_Paderborn"

#' @rdname geodata
"EG_Siegen"

#' @rdname geodata
"Wenkerorte"

#' Voronoi-Zerlegungen der Wenker-Orte
#'
#' Diese Datensätze enthalten Voronoi-Polygone (sf-Objekte), die aus den
#' Wenker-Orte-Punkten der jeweiligen Region erzeugt wurden. Jedes Polygon
#' trägt die Attribute des nächstgelegenen Wenker-Ortes.
#'
#' @format Ein sf-Objekt (simple features) mit Polygon-Geometrien.
#'
#' @details
#' - `WenkerOrte_Bonn_Voronoi`: Voronoi-Zerlegung der Wenker-Orte im Raum Bonn
#' - `WenkerOrte_Muenster_Voronoi`: Voronoi-Zerlegung der Wenker-Orte im Raum Münster
#' - `WenkerOrte_Paderborn_Voronoi`: Voronoi-Zerlegung der Wenker-Orte im Raum Paderborn
#' - `WenkerOrte_Siegen_Voronoi`: Voronoi-Zerlegung der Wenker-Orte im Raum Siegen
#' - `Wenkerorte_Voronoi`: Kombinierte Voronoi-Zerlegung aller Wenker-Orte
#'
#' @source Erzeugt aus den Wenker-Orte-Punktdaten mittels `sf::st_voronoi()`,
#' siehe `data-raw/create_voronoi.R`.
#'
#' @name voronoi
#' @docType data
#' @keywords datasets
#'
#' @examples
#' \dontrun{
#'   library(sf)
#'   plot(WenkerOrte_Bonn_Voronoi)
#' }
#'
NULL

#' @rdname voronoi
"WenkerOrte_Bonn_Voronoi"

#' @rdname voronoi
"WenkerOrte_Muenster_Voronoi"

#' @rdname voronoi
"WenkerOrte_Paderborn_Voronoi"

#' @rdname voronoi
"WenkerOrte_Siegen_Voronoi"

#' @rdname voronoi
"Wenkerorte_Voronoi"
