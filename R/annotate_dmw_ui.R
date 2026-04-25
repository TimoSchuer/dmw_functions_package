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
    bslib::card_header(
      class = "card-header-annotate",
      layout_column_wrap(
        width = NULL,
        style = htmltools::css(grid_template_columns = "auto 1fr"),
        shiny::tags$img(
          src = "CD/Logos DMW/logo.png",
          height = "40px",
          style = "padding: 0px 10px;"
        ),

        h2("Annotationstool (Syntax und Morphologie)")
      )
    ),
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
      bslib::layout_column_wrap(
        width = NULL,
        style = htmltools::css(grid_template_columns = "3fr 3fr 3fr"),
        bslib::value_box(
          title = "Aktuelles Item",
          min_height = "150px",
          max_height = "180px",
          style = "margin-right: 0px;margin-bottom: 10px;",
          value = shiny::textOutput(shiny::NS(id, "currentItem"))
        ),
        bslib::value_box(
          value = shiny::textOutput(shiny::NS(id, "itemCounter")),
          title = "Item Fortschritt",
          min_height = "150px",
          max_height = "180px",
          style = "margin-left: 0px;margin-bottom: 10px;"
        ),
        bslib::value_box(
          title = "Forschritt Annotation",
          min_height = "150px",
          max_height = "180px",
          style = "margin-left: 0px; margin-bottom: 10px;",
          value = shiny::textOutput(shiny::NS(id, "progressPercent"))
        )
      ),
      bslib::card(
        style = "border-style: none; margin-top: 15px;",
        uiOutput(shiny::NS(id, "audioPlayer")),
        min_height = "150px",
        max_height = "180px"
      ),
      bslib::layout_column_wrap(
        width = NULL,
        style = htmltools::css(grid_template_columns = " 6fr 3fr"),
        bslib::card(
          min_height = "250px",
          class = class,
          bslib::layout_column_wrap(
            width = NULL,
            style = htmltools::css(grid_template_columns = "1fr"),
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
            )
          ),
          bslib::layout_column_wrap(
            width = 1 / 4,
            shiny::actionButton(
              shiny::NS(id, "previousItem"),
              label = "",
              icon = shiny::icon("backward-fast"),
              width = "100%",
              disabled = TRUE
            ) |>
              bslib::tooltip(
                "Vorheriges Item (mit Speichern)",
                placement = "top"
              ),
            shiny::actionButton(
              shiny::NS(id, "previousCategory"),
              label = "",
              icon = shiny::icon("backward-step"),
              width = "100%",
              disabled = TRUE
            ) |>
              bslib::tooltip(
                "Vorherige Kategorie (mit Speichern)",
                placement = "top"
              ),
            shiny::actionButton(
              shiny::NS(id, "nextCategory"),
              label = "",
              icon = shiny::icon("forward-step"),
              width = "100%",
              disabled = TRUE
            ) |>
              bslib::tooltip(
                "Nächste Kategorie (mit Speichern)",
                placement = "top"
              ),
            shiny::actionButton(
              shiny::NS(id, "nextItem"),
              label = "",
              icon = shiny::icon("forward-fast"),
              width = "100%"
            ) |>
              bslib::tooltip(
                "Nächstes Item (mit Speichern)",
                placement = "top"
              )
          ),
          bslib::layout_column_wrap(
            width = 1 / 4,
            shiny::actionButton(
              shiny::NS(id, "previousItemNoSave"),
              label = "",
              icon = shiny::icon("angles-left"),
              width = "100%",
              disabled = TRUE,
              class = "btn-outline-secondary"
            ) |>
              bslib::tooltip(
                "Vorheriges Item (ohne Speichern)",
                placement = "top"
              ),
            shiny::actionButton(
              shiny::NS(id, "previousCategoryNoSave"),
              label = "",
              icon = shiny::icon("angle-left"),
              width = "100%",
              disabled = TRUE,
              class = "btn-outline-secondary"
            ) |>
              bslib::tooltip(
                "Vorherige Kategorie (ohne Speichern)",
                placement = "top"
              ),
            shiny::actionButton(
              shiny::NS(id, "nextCategoryNoSave"),
              label = "",
              icon = shiny::icon("angle-right"),
              width = "100%",
              disabled = TRUE,
              class = "btn-outline-secondary"
            ) |>
              bslib::tooltip(
                "Nächste Kategorie (ohne Speichern)",
                placement = "top"
              ),
            shiny::actionButton(
              shiny::NS(id, "nextItemNoSave"),
              label = "",
              icon = shiny::icon("angles-right"),
              width = "100%",
              class = "btn-outline-secondary"
            ) |>
              bslib::tooltip(
                "Nächstes Item (ohne Speichern)",
                placement = "top"
              )
          ),
          shiny::uiOutput(shiny::NS(id, "comment")),
          bslib::layout_column_wrap(
            width = 1 / 3,
            shiny::actionButton(
              shiny::NS(id, "addComment"),
              label = "",
              icon = shiny::icon("comment-dots"),
              width = "50%"
            ) |>
              bslib::tooltip(
                "Kommentar hinzufügen",
                placement = "top"
              ),
            bslib::input_switch(
              shiny::NS(id, "unclear"),
              label = "Item als Zweifelsfall markieren",
              value = FALSE,
              width = "100%"
            ),
            bslib::input_switch(
              shiny::NS(id, "unusable"),
              label = "Item nicht auswertbar",
              value = FALSE,
              width = "100%"
            ),
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
            NS(id, "onlyDoubt"),
            label = "Nur Zweifelsfälle anzeigen",
            value = FALSE,
            width = "100%"
          )
        ),
      )
    )
  )
}
