############# map module ###########

#' Karten-Modul UI
#'
#' Erstellt die Benutzeroberfläche für das interaktive Kartenmodul. Bietet
#' Steuerelemente zur Auswahl von Analysen, Datenfilterung, Kartengestaltung
#' und Export.
#'
#' @param id Character. Modul-ID für das Namespacing von Shiny-Inputs/Outputs.
#'
#' @return Ein Shiny-UI-Element (`shiny.tag`) mit dem vollständigen Karteninterface.
#'
#' @details
#' Das Modul besteht aus einer Seitenleiste mit drei Accordion-Panels:
#' - **Daten filtern**: Geschlecht, Altersklasse und Varianten-Auswahl
#' - **Karte gestalten**: Kartentyp, Grenzen (Politisch/Voronoi/Keine),
#'   Hintergrundfarbe, Kreistransparenz und Label-Einstellungen
#' - **Karte exportieren**: Download als Karte sowie Daten in verschiedenen Formaten
#'
#' Die Kartenansicht wird mit `ggiraph::girafeOutput()` gerendert und unterstützt
#' interaktive Hover-Effekte.
#'
#' @section Dependencies:
#' Benötigt die Pakete: bslib, shiny, ggiraph, colourpicker, bsicons
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In einer Shiny-App
#' ui <- mapUI("map")
#' }
mapUI <- function(id) {
  bslib::card(
    title = "Karten erstellen",
    class = "primary",
    # min_height = "800px",
    full_screen = TRUE,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = "400px",
        shiny::selectizeInput(
          NS(id, "mapVar"),
          "Analyse auswählen",
          choices = NULL,
          selected = NULL
        ),
        bslib::accordion(
          multiple = FALSE,
          open = 1,
          bslib::accordion_panel(
            title = "Daten filtern",
            icon = bsicons::bs_icon("funnel"),
            selectizeInput(
              NS(id, "Geschlecht"),
              "Geschlecht",
              multiple = TRUE,
              choices = NULL,
              selected = NULL
            ),

            selectizeInput(
              NS(id, "Altersklasse"),
              "Altersklasse",
              multiple = TRUE,
              choices = NULL,
              selected = NULL
            ),
            selectizeInput(
              NS(id, "selectedVar"),
              "Varianten auswählen",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            )
          ),
          bslib::accordion_panel(
            title = "Karte gestalten",
            icon = bsicons::bs_icon("palette"),
            shiny::radioButtons(
              NS(id, "mapType"),
              label = "Karten Typ",
              choices = c("Kreis", "Punkt"),
              inline = TRUE
            ),
            shiny::h6("Hintergrund"),
            shiny::radioButtons(
              NS(id, "border"),
              label = "Grenzen",
              choices = c("Politisch", "Voronoi", "Keine"),
              selected = "Politisch"
            ),
            colourpicker::colourInput(
              NS(id, "mapBackground"),
              label = "Hintergrundfarbe",
              value = "white",
              palette = "square"
            ),

            shiny::conditionalPanel(
              condition = "input.mapType == 'Punkt'",
              ns = NS(id),
              shiny::tagList(
                shiny::sliderInput(
                  NS(id, "transparency"),
                  label = "Transparenz Kreise",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.1
                ),
                shiny::p(),
                shiny::actionButton(
                  NS(id, "mapColor"),
                  label = "Farben manuell zuordnen",
                  icon = shiny::icon("palette")
                )
              )
            ),
            shiny::p(),
            shiny::actionButton(
              NS(id, "mapLabel"),
              label = "Label anpassen",
              icon = shiny::icon("font")
            )
          ),
          bslib::accordion_panel(
            "Karte exportieren",
            icon = bsicons::bs_icon("download"),
            shiny::downloadButton(
              outputId = NS(id, "downloadMap"),
              label = "Karte herunterladen"
            ),
            shiny::p(""),
            shiny::radioButtons(
              NS(id, "downloadType"),
              label = "Dateityp",
              choices = c("csv", "geojson", "rds"),
              selected = "csv"
            ),
            shiny::downloadButton(
              outputId = NS(id, "downloadData"),
              label = "Daten herunterladen"
            )
          ),
          shiny::p(""),
          shiny::actionButton(
            NS(id, "showMap"),
            label = "Karte anzeigen",
            icon = shiny::icon("map")
          )
        )
      ),
      shiny::uiOutput(NS(id, "mapAreaVar")),
      ggiraph::girafeOutput(NS(id, "mapPlot"), width = "100%", height = "100%")
    )
  )
}

#' Karten-Modul Server
#'
#' Server-seitige Logik für das interaktive Kartenmodul. Lädt Analysedaten aus
#' der Datenbank, filtert nach soziolinguistischen Merkmalen und rendert eine
#' interaktive Karte mit `ggiraph`.
#'
#' @param id Character. Modul-ID, muss mit der UI-Funktion übereinstimmen.
#' @param conAnn DBI-Verbindungsobjekt. Datenbankverbindung für Annotationsdaten.
#' @param conDMW DBI-Verbindungsobjekt. Datenbankverbindung für DMW-Referenzdaten.
#' @param user Character. Benutzername des aktuell angemeldeten Nutzers.
#' @param selectedAnalysis Optional. Vorausgewählte Analyse (wird aktuell nicht
#'   ausgewertet, reserviert für zukünftige Verwendung).
#'
#' @return Gibt unsichtbar `NULL` zurück (typisch für Shiny-Server-Module).
#'
#' @details
#' Das Modul führt folgende Aufgaben aus:
#' - Abfrage verfügbarer Analysen per `reactivePoll` (1-Sekunden-Intervall)
#' - Laden und Zusammenführen von Transkriptions-, Informanten- und Analysedaten
#' - Berechnung von Altersklassen aus Geburtsjahren
#' - Filterung nach Geschlecht, Altersklasse und Varianten-Kategorie
#' - Verknüpfung mit Wenker-Ortsdaten (`Wenkerorte`) und Erhebungsgebieten (`EG_DMW`)
#' - Interaktive Kartendarstellung mit Hover-Effekten via `ggiraph`
#' - Download-Handler für Karten- und Dateienexport
#'
#' @section Datenbank-Anforderungen:
#' Die Annotationsdatenbank (`conAnn`) sollte folgende Tabellen enthalten:
#' - `mapAnalysis`: Verfügbare Kartenanalysen
#' - `mapAnalysisAnalysis`: IPA/RWS-Zuordnungen je Analyse
#'
#' Die DMW-Datenbank (`conDMW`) sollte folgende Tabellen enthalten:
#' - `Transcription`: Transkriptionsdaten mit IPA und RWL
#' - `Informant`: Informantendaten inkl. Geschlecht und Geburtsjahr
#'
#' @section Dependencies:
#' Benötigt die Pakete: shiny, DBI, dplyr, glue, ggiraph, ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In einer Shiny-App
#' mapServer("map", con_annotation, con_dmw, user = "nutzer1")
#' }
mapServer <- function(id, conAnn, conDMW, user, selectedAnalysis = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    choicesAnalysis <- shiny::reactivePoll(
      intervalMillis = 1000,
      session = session,
      checkFunc = function() {
        DBI::dbGetQuery(
          conAnn,
          glue::glue_sql(
            "SELECT * FROM mapAnalysis ",
            .con = conAnn
          )
        )
      },
      valueFunc = function() {
        choices <- DBI::dbGetQuery(
          conAnn,
          glue::glue_sql(
            "SELECT name,user, visibileToOthers FROM mapAnalysis",
            .con = conAnn
          )
        )
        if (nrow(choices) == 0) {
          return(character(0))
        }
        choices |>
          dplyr::filter(
            user == user | visibileToOthers == 1
          ) |>
          dplyr::pull(name)
      }
    )

    observe({
      updateSelectizeInput(
        session = session,
        "mapVar",
        choices = choicesAnalysis()
      )
    }) |>
      shiny::bindEvent(choicesAnalysis())

    analysisData <- reactive({
      req(input$mapVar)
      analysis <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT a.id,a.name,a.user,a.visibileToOthers, aa.ipa, aa.rws, aa.category FROM mapAnalysis a
          JOIN mapAnalysisAnalysis aa ON a.id = aa.analysis_id
          WHERE a.name = {input$mapVar} AND (a.user = {user} OR a.visibileToOthers = 1)",
          .con = conAnn
        )
      )
      # print("analysis:")
      # print(head(analysis))
      rawData <- DBI::dbGetQuery(
        conDMW,
        glue::glue_sql(
          "SELECT * FROM Transcription WHERE rwl = {analysis$rws[1]}",
          .con = conDMW
        )
      )
      # print("rawData:")
      # print(head(rawData))
      informants <- DBI::dbGetQuery(
        conDMW,
        glue::glue_sql(
          "SELECT * FROM Informant",
          .con = conDMW
        )
      )
      # print("informants:")
      # print(head(informants))
      mergedData <- rawData |>
        select(informant_id, subtask_id, rwl, ipa, konfidenzwert, username) |>
        dplyr::left_join(informants, by = c("informant_id" = "informant_id")) |>
        dplyr::left_join(
          analysis |> dplyr::select(ipa, rws, category),
          by = c("ipa" = "ipa", "rwl" = "rws")
        ) |>
        dplyr::mutate(
          category = ifelse(is.na(category), "Unsortiert", category),
          birth_date = as.numeric(birth_date)
        ) |>
        mutate(
          Altersklasse = case_when(
            birth_date - 2026 <= 20 ~ "0-20",
            birth_date - 2026 <= 40 ~ "21-40",
            birth_date - 2026 <= 60 ~ "41-60",
            TRUE ~ "60+"
          )
        )
      # print("mergedData:")
      # print(head(mergedData))
      mergedData
    })

    shiny::observe({
      req(analysisData())
      updateSelectizeInput(
        session = session,
        "Geschlecht",
        choices = unique(analysisData()$gender),
        selected = unique(analysisData()$gender)
      )
      updateSelectizeInput(
        session = session,
        "Altersklasse",
        choices = unique(analysisData()$Altersklasse),
        selected = unique(analysisData()$Altersklasse)
      )
      updateSelectizeInput(
        session = session,
        "selectedVar",
        choices = unique(analysisData()$category),
        selected = unique(analysisData()$category)
      )
    }) |>
      shiny::bindEvent(analysisData())

    shiny::observe({
      output$mapPlot <- ggiraph::renderGirafe({
        mapData <- analysisData() |>
          dplyr::filter(
            gender %in% input$Geschlecht,
            Altersklasse %in% input$Altersklasse,
            category %in% input$selectedVar
          ) |>
          dplyr::left_join(
            Wenkerorte,
            by = c("city" = "name")
          )

        p <- mapData |>
          ggplot2::ggplot() +
          ggiraph::geom_sf_interactive(data = EG_DMW) +
          ggiraph::geom_sf_interactive(ggplot2::aes(
            geometry = geometry,
            color = category,
            tooltip = city,
            data_id = category
          )) +
          ggplot2::theme_void()
        ggiraph::girafe(
          ggobj = p,
          options = list(
            ggiraph::opts_hover(
              css = "opacity:1;stroke:black;stroke-width:0.5px;"
            ),
            ggiraph::opts_hover_inv(css = "opacity:0.4;stroke:gray;fill:gray;")
          )
        )
      })
    }) |>
      shiny::bindEvent(input$showMap)
  })
}
