#' DMW Annotation Tool UI Module
#'
#' Creates the user interface for the DMW annotation tool with controls for
#' selecting linguistic phenomena, managing annotation tasks, and exporting results.
#'
#' @param id Character string. The module ID for namespacing Shiny inputs/outputs.
#' @param min_height Character string. Minimum height of the card container.
#'   Default is "200px".
#' @param class Character string. Additional CSS classes to apply to the card.
#'   Default is NULL.
#'
#' @return A Shiny UI element (shiny.tag) representing the annotation interface.
#'
#' @details
#' This module provides a comprehensive interface for linguistic annotation including:
#' - Selection of linguistic system level (Syntax/Morphology)
#' - Phenomena and task selection
#' - Audio player with playback controls
#' - Annotation input and category management
#' - Navigation buttons for items and categories
#' - Item flagging (doubt, unusable)
#' - Comment functionality
#' - Export options (standard and REDE format)
#'
#' @section Dependencies:
#' Requires the following packages: bslib, shiny, htmltools, shinyWidgets
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Shiny app
#' ui <- annotateDmwUI("annotate_dmw")
#' }
annotateDmwUI <- function(id, min_height = "200px", class = NULL) {
  bslib::card(
    min_height = min_height,
    class = class,
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        width = "300px",
        open = TRUE,
        shiny::selectizeInput(
          shiny::NS(id, "selectSystemebene"),
          label = "Systemebene auswählen",
          choices = c("Syntax", "Morphologie"),
          selected = "Syntax"
        ),
        shiny::selectizeInput(
          NS(id, "selectPhaenomen"),
          label = "Phänomen auswählen",
          choices = NULL,
        ),
        shiny::selectizeInput(
          shiny::NS(id, "selectTask"),
          label = "Aufgaben auswählen",
          choices = NULL,
          multiple = FALSE
        ),
        h3("Export"),
        shiny::downloadButton(
          shiny::NS(id, "exportAnnotations"),
          label = "Annotationen exportieren",
          icon = shiny::icon("download")
        ) |>
          bslib::tooltip(
            "Exportiert die bisher vorgenommenen Annotationen als Excel-Datei.",
            placement = "right"
          ),
        shiny::hr(),
        h4("REDE Export"),
        shiny::downloadButton(
          shiny::NS(id, "exportRede"),
          label = "REDE Export",
          icon = shiny::icon("file-excel")
        ) |>
          bslib::tooltip(
            "Exportiert annotierte Varianten der ausgewählten Aufgabe im REDE-Format.",
            placement = "right"
          )
      ),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = 4),
        bslib::value_box(
          title = "Aktuelles Item",
          style = "margin-bottom: 10px;",
          value = shiny::textOutput(shiny::NS(id, "currentItem"))
        ),
        bslib::value_box(
          value = shiny::textOutput(shiny::NS(id, "itemCounter")),
          title = "Item Fortschritt",
          style = "margin-bottom: 10px;"
        ),
        bslib::value_box(
          title = "Fortschritt Annotation",
          style = "margin-bottom: 10px;",
          value = shiny::textOutput(shiny::NS(id, "progressPercent"))
        )
      ),
      bslib::card(
        style = "border-style: none; margin-top: 15px;",
        shiny::uiOutput(shiny::NS(id, "audioPlayer"))
      ),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),
        bslib::card(
          min_height = "250px",
          class = class,
          shiny::h4(shiny::textOutput(shiny::NS(id, "currentCategory"))),
          shiny::selectizeInput(
            shiny::NS(id, "annValue"),
            label = "Variante auswählen oder eingeben",
            choices = NULL,
            width = "100%",
            options = list(
              create = TRUE,
              createOnBlur = TRUE,
              placeholder = "Variante auswählen oder neue eingeben..."
            )
          ),
          shiny::div(
            style = "display: flex; flex-wrap: wrap; gap: 4px; margin-bottom: 4px;",
            shiny::actionButton(
              shiny::NS(id, "previousItem"),
              label = "",
              icon = shiny::icon("backward-fast"),
              style = "flex: 1 1 20%;",
              disabled = TRUE
            ) |>
              bslib::tooltip(
                "Vorheriges Item (mit Speichern)",
                placement = "top"
              ),
            # shiny::actionButton(
            #   shiny::NS(id, "previousCategory"),
            #   label = "", icon = shiny::icon("backward-step"),
            #   style = "flex: 1 1 20%;", disabled = TRUE
            # ) |> bslib::tooltip("Vorherige Kategorie (mit Speichern)", placement = "top"),
            # shiny::actionButton(
            #   shiny::NS(id, "nextCategory"),
            #   label = "", icon = shiny::icon("forward-step"),
            #   style = "flex: 1 1 20%;", disabled = TRUE
            # ) |> bslib::tooltip("Nächste Kategorie (mit Speichern)", placement = "top"),
            shiny::actionButton(
              shiny::NS(id, "nextItem"),
              label = "",
              icon = shiny::icon("forward-fast"),
              style = "flex: 1 1 20%;"
            ) |>
              bslib::tooltip("Nächstes Item (mit Speichern)", placement = "top")
          ),
          shiny::div(
            style = "display: flex; flex-wrap: wrap; gap: 4px; margin-bottom: 8px;",
            shiny::actionButton(
              shiny::NS(id, "previousItemNoSave"),
              label = "",
              icon = shiny::icon("angles-left"),
              style = "flex: 1 1 20%;",
              disabled = TRUE,
              class = "btn-outline-secondary"
            ) |>
              bslib::tooltip(
                "Vorheriges Item (ohne Speichern)",
                placement = "top"
              ),
            # shiny::actionButton(
            #   shiny::NS(id, "previousCategoryNoSave"),
            #   label = "", icon = shiny::icon("angle-left"),
            #   style = "flex: 1 1 20%;", disabled = TRUE,
            #   class = "btn-outline-secondary"
            # ) |> bslib::tooltip("Vorherige Kategorie (ohne Speichern)", placement = "top"),
            # shiny::actionButton(
            #   shiny::NS(id, "nextCategoryNoSave"),
            #   label = "", icon = shiny::icon("angle-right"),
            #   style = "flex: 1 1 20%;", disabled = TRUE,
            #   class = "btn-outline-secondary"
            # ) |> bslib::tooltip("Nächste Kategorie (ohne Speichern)", placement = "top"),
            shiny::actionButton(
              shiny::NS(id, "nextItemNoSave"),
              label = "",
              icon = shiny::icon("angles-right"),
              style = "flex: 1 1 20%;",
              class = "btn-outline-secondary"
            ) |>
              bslib::tooltip(
                "Nächstes Item (ohne Speichern)",
                placement = "top"
              )
          ),
          shiny::uiOutput(shiny::NS(id, "comment")),
          shiny::div(
            style = "display: flex; flex-wrap: wrap; align-items: center; gap: 8px; margin-top: 8px;",
            shiny::actionButton(
              shiny::NS(id, "addComment"),
              label = "",
              icon = shiny::icon("comment-dots")
            ) |>
              bslib::tooltip("Kommentar hinzufügen", placement = "top"),
            bslib::input_switch(
              shiny::NS(id, "unclear"),
              label = "Zweifelsfall",
              value = FALSE
            ),
            bslib::input_switch(
              shiny::NS(id, "unusable"),
              label = "Nicht auswertbar",
              value = FALSE
            )
          )
        ),
        bslib::card(
          shiny::actionButton(
            shiny::NS(id, "showInfo"),
            label = "",
            icon = shiny::icon("info-circle"),
            width = "100%"
          ),
          shiny::actionButton(
            shiny::NS(id, "addCategory"),
            label = "Kategorie hinzufügen",
            icon = shiny::icon("plus"),
            width = "100%"
          ),
          shiny::textInput(
            shiny::NS(id, "skipItem"),
            label = "Item-ID:",
            width = "100%"
          ),
          shiny::actionButton(
            shiny::NS(id, "skipItemButton"),
            label = "Springe zu Item"
          ),
          bslib::input_switch(
            shiny::NS(id, "onlyDoubt"),
            label = "Nur Zweifelsfälle anzeigen",
            value = FALSE,
            width = "100%"
          )
        )
      )
    )
  )
}
