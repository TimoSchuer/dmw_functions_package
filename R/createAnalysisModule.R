#' Analyse-Erstellungs-Modul UI
#'
#' Erstellt die Benutzeroberfläche für das Modul zur Verwaltung von
#' Kartenanalysen. Ermöglicht das Anlegen, Laden und Löschen von Analysen
#' sowie die Zuordnung von IPA-Transkriptionen zu Kategorien per Drag-and-Drop.
#'
#' @param id Character. Modul-ID für das Namespacing von Shiny-Inputs/Outputs.
#'
#' @return Ein Shiny-UI-Element (`shiny.tag`) mit dem vollständigen
#'   Analyse-Management-Interface.
#'
#' @details
#' Das Interface besteht aus einer Seitenleiste mit:
#' - Aufgaben-Auswahl (`rwl`-Schlüssel aus der Transkriptionsdatenbank)
#' - Schaltflächen zum Erstellen, Laden und Löschen von Analysen
#' - Sichtbarkeitsschalter (sichtbar für andere Nutzer)
#' - Kategorienverwaltung (hinzufügen, umbenennen, löschen)
#'
#' Der Hauptbereich enthält zwei Karten:
#' - **Übersicht Kategorien**: zeigt alle IPA-Tokens je Kategorie als Badges
#' - **Sortieren**: interaktives Drag-and-Drop-Interface (`sortable::bucket_list`)
#'   mit optionaler Filterung nach Silbenposition und IPA-Muster
#'
#' @section Dependencies:
#' Benötigt die Pakete: bslib, shiny, bsicons, shinyWidgets, shinycssloaders, sortable
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In einer Shiny-App
#' ui <- createAnalysisUI("createAnalysis")
#' }
createAnalysisUI <- function(id) {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = "400px",
      # Task selection
      shiny::selectizeInput(
        NS(id, "task"),
        label = "Aufgabe auswählen",
        choices = NULL,
        selected = NULL
      ),
      shiny::hr(),
      # Analysis management
      shiny::h6("Analyse verwalten"),
      shiny::actionButton(
        NS(id, "newAnalysis"),
        label = "Neue Analyse anlegen",
        icon = shiny::icon("plus-circle")
      ),
      shiny::actionButton(
        NS(id, "loadAnalysis"),
        label = "Analyse laden",
        icon = shiny::icon("folder-open")
      ),
      shiny::actionButton(
        NS(id, "deleteAnalysis"),
        label = "Analyse löschen",
        icon = shiny::icon("trash")
      ),
      shiny::hr(),
      # Analysis visibility
      shiny::h6("Sichtbarkeit"),
      shinyWidgets::switchInput(
        NS(id, "analysisVisible"),
        label = "Für andere sichtbar",
        value = FALSE,
        onStatus = "success",
        offStatus = "danger",
        size = "small",
        labelWidth = "80px"
      ),
      shiny::hr(),

      shiny::actionButton(
        NS(id, "addCategory"),
        label = "Kategorie hinzufügen",
        icon = shiny::icon("plus")
      ),
      shiny::actionButton(
        NS(id, "renameCategory"),
        label = "Kategorie umbenennen",
        icon = shiny::icon("pen")
      ),
      shiny::actionButton(
        NS(id, "removeCategory"),
        label = "Kategorie löschen",
        icon = shiny::icon("trash")
      )
    ),
    # Main content area with cards
    shiny::actionButton(
      NS(id, "overview"),
      label = "Aktualisieren",
      icon = shiny::icon("sync"),
      width = "100%"
    ),
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        title = "Übersicht Kategorien",
        icon = bsicons::bs_icon("list-check"),
        class = "h-100",
        style = "overflow: auto;",
        shiny::uiOutput(NS(id, "categoryOverviewUI")) |>
          shinycssloaders::withSpinner(type = 4)
      ),
      bslib::card(
        title = "Sortieren",
        height = "800px",
        full_screen = TRUE,
        style = "min-width: 300px; display: flex; flex-direction: column;",
        bslib::card(
          bslib::layout_column_wrap(
            width = 1 / 2,
            selectizeInput(
              NS(id, "sortStartCategory"),
              "Quelle",
              choices = ""
            ),
            selectizeInput(
              NS(id, "sortEndCategory"),
              "Ziel",
              choices = ""
            )
          ),
        ),
        bslib::card(
          title = "Filter",
          min_height = "100px",
          max_height = "150px",
          h5("Filter"),
          bslib::layout_column_wrap(
            width = NULL,
            style = css(
              grid_template_columns = "3fr 1fr",
              align_items = "flex-end"
            ),
            selectizeInput(
              NS(id, "activeFilters"),
              "Aktive Filter",
              choices = "",
              multiple = TRUE
            ),
            actionButton(
              NS(id, "addFilter"),
              "Filter hinzufügen",
              icon = shiny::icon("filter")
            ),
          )
        ),
        bslib::card(
          class = "flex-fill",
          style = "overflow: auto;",
          uiOutput(NS(id, "sortBucketUI")) |>
            shinycssloaders::withSpinner(type = 4)
        )
      )
    )
  )
}

#' Analyse-Erstellungs-Modul Server
#'
#' Server-seitige Logik für das Modul zur Verwaltung von Kartenanalysen.
#' Steuert das Erstellen, Laden und Löschen von Analysen sowie die interaktive
#' Zuordnung von IPA-Tokens zu Kategorien per Drag-and-Drop.
#'
#' @param id Character. Modul-ID, muss mit der UI-Funktion übereinstimmen.
#' @param conDMW DBI-Verbindungsobjekt. Datenbankverbindung für DMW-Transkriptionsdaten.
#' @param conAnn DBI-Verbindungsobjekt. Datenbankverbindung für Analysedaten.
#' @param task Character oder NULL. Optionale vorausgewählte Aufgabe (`rwl`-Wert).
#'   Wird aktuell nicht ausgewertet; die Aufgabe wird reaktiv über das UI gewählt.
#' @param user Character. Benutzername des aktuell angemeldeten Nutzers.
#'
#' @return Gibt unsichtbar `NULL` zurück (typisch für Shiny-Server-Module).
#'
#' @details
#' Das Modul führt folgende Aufgaben aus:
#' - Laden aller verfügbaren `rwl`-Werte aus der Transkriptionsdatenbank
#' - Erstellen einer neuen Analyse inkl. optionaler Startkategorien (Modal)
#' - Laden einer bestehenden Analyse (eigene oder freigegebene)
#' - Löschen einer Analyse (noch nicht vollständig implementiert)
#' - Sichtbarkeitssteuerung: Analyse für andere Nutzer freigeben/sperren
#' - Kategorienverwaltung: Hinzufügen, Umbenennen und Löschen von Kategorien
#'   (mit kaskadierten Updates in `mapAnalysisCategory` und `mapAnalysisAnalysis`)
#' - Drag-and-Drop-Sortierung von IPA-Tokens zwischen Kategorien via
#'   `sortable::bucket_list`; Änderungen werden sofort in die Datenbank geschrieben
#' - Filterung der anzuzeigenden Tokens nach Silbenposition und IPA-Muster
#'   (clientseitig über `apply_multiple_filters`)
#'
#' @section Datenbank-Anforderungen:
#' Die DMW-Datenbank (`conDMW`) sollte folgende Tabelle enthalten:
#' - `Transcription`: Spalten `rwl` und `ipa`
#'
#' Die Annotationsdatenbank (`conAnn`) sollte folgende Tabellen enthalten:
#' - `mapAnalysis`: `id`, `name`, `rws`, `user`, `visibileToOthers`
#' - `mapAnalysisCategory`: `analysis_id`, `category_name`, `created_by`
#' - `mapAnalysisAnalysis`: `analysis_id`, `rws`, `ipa`, `category`, `changed_by`
#'
#' @section Dependencies:
#' Benötigt die Pakete: shiny, DBI, dplyr, glue, shinyWidgets, sortable
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In einer Shiny-App
#' createAnalysis("createAnalysis", con_dmw, con_annotation, user = "nutzer1")
#' }
createAnalysis <- function(
  id,
  conDMW = conDmw,
  conAnn = conAnn,
  task = NULL,
  user = user_name()
) {
  shiny::moduleServer(id, function(input, output, session) {
    print(user)
    transcriptions <- reactive(
      {
        req(conDMW)
        req(input$task)
        transcriptions <- DBI::dbGetQuery(
          conDMW,
          glue::glue_sql(
            "SELECT * FROM Transcription WHERE rwl = {input$task}",
            .con = conDMW
          )
        )
        transcriptions
      }
    )
    selectedAnalysis <- reactiveVal(NULL)

    # categories <- shiny::reactivePoll(
    #   intervalMillis = 1000,
    #   session = session,
    #   checkFunc = function() {
    #     req(conAnn)
    #     req(selectedAnalysis())
    #     last_update <- DBI::dbGetQuery(
    #       conAnn,
    #       glue::glue_sql(
    #         "SELECT MAX(updated_at) AS last_update FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
    #         .con = conAnn
    #       )
    #     )$last_update
    #     last_update
    #   },
    #   valueFunc = function() {
    #     req(conAnn)
    #     req(selectedAnalysis())
    #     categories <- DBI::dbGetQuery(
    #       conAnn,
    #       glue::glue_sql(
    #         "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
    #         .con = conAnn
    #       )
    #     )$category_name
    #     categories
    #   }
    # )
    categories <- reactive({
      req(conAnn)
      req(selectedAnalysis())
      categories <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
          .con = conAnn
        )
      )$category_name
      categories
    }) |>
      shiny::bindEvent(c(
        selectedAnalysis(),
        input$confirmAddCategory,
        input$confirmRenameCategory,
        input$confirmRemoveCategory
      ))

    observe({
      req(conDMW)
      updateSelectizeInput(
        session = session,
        inputId = "task",
        choices = unique(
          DBI::dbGetQuery(conDMW, "SELECT DISTINCT rwl FROM Transcription")$rwl
        ),
        selected = unique(
          DBI::dbGetQuery(conDMW, "SELECT DISTINCT rwl FROM Transcription")$rwl
        )[1]
      )
    }) |>
      shiny::bindEvent(conDMW, once = TRUE)

    shiny::bindEvent(
      shiny::observe({
        req(input$task)
        showModal(
          shiny::modalDialog(
            title = "Neue Analyse anlegen",
            easyClose = TRUE,
            footer = NULL,
            size = "m",
            shiny::tagList(
              textInput(NS(id, "analysisName"), "Name der Analyse"),
              textInput(
                NS(id, "analysisCategory"),
                "Kategorie der Analyse (mit Komma getrennt)"
              ),
              shiny::checkboxInput(
                NS(id, "visibileUser"),
                label = "Für andere Nutzer sichtbar",
                value = TRUE
              ),
              actionButton(
                NS(id, "confirmCreate"),
                "Erstellen",
                class = "btn-primary"
              )
            )
          )
        )
      }),
      input$newAnalysis
    )

    shiny::bindEvent(
      shiny::observe({
        req(input$confirmCreate)
        if (is.null(input$analysisName) || input$analysisName == "") {
          showNotification(
            "Bitte einen Namen für die Analyse eingeben.",
            type = "error"
          )
          return()
        }
        if (is.null(input$task) || input$task == "") {
          showNotification("Bitte eine Aufgabe auswählen.", type = "error")
          return()
        }
        tryCatch(
          {
            # Insert new analysis
            DBI::dbBegin(conAnn)
            DBI::dbExecute(
              conAnn,
              glue::glue_sql(
                "INSERT INTO mapAnalysis (name, rws,user, visibileToOthers) VALUES ({input$analysisName}, {input$task},{user}, {input$visibileUser})",
                .con = conAnn
              )
            )
            DBI::dbCommit(conAnn)
            # insert categories
            if (
              !is.null(input$analysisCategory) && input$analysisCategory != ""
            ) {
              categories <- strsplit(input$analysisCategory, ",")[[1]] |>
                trimws() |>
                unique()
              if (length(categories) == 0) {
                return()
              }
              analysis_id <- DBI::dbGetQuery(
                conAnn,
                glue::glue_sql(
                  "SELECT id FROM mapAnalysis WHERE name = {input$analysisName} AND rws = {input$task}",
                  .con = conAnn
                )
              )$id[1]

              for (cat in categories) {
                DBI::dbBegin(conAnn)
                DBI::dbExecute(
                  conAnn,
                  glue::glue_sql(
                    "INSERT INTO mapAnalysisCategory (analysis_id, category_name, created_by) VALUES ({analysis_id}, {cat}, {user})",
                    .con = conAnn
                  )
                )
                DBI::dbCommit(conAnn)
              }
            }
          },
          error = function(e) {
            DBI::dbRollback(conAnn)
            print(e)
            showNotification(
              glue::glue(
                "Fehler beim Erstellen der Analyse: {e$message}"
              ),
              type = "error"
            )
          },
          finally = {
            selectedAnalysis(DBI::dbGetQuery(
              conAnn,
              glue::glue_sql(
                "SELECT id FROM mapAnalysis WHERE name = {input$analysisName} AND rws = {input$task}",
                .con = conAnn
              )
            )$id[1])
            print(paste("Selected analysis ID:", selectedAnalysis()))
            removeModal()
          }
        )
      }),
      input$confirmCreate
    )

    choicesAnalysis <- shiny::bindEvent(
      reactive({
        req(conAnn)
        choices <- DBI::dbGetQuery(
          conAnn,
          glue::glue_sql(
            "SELECT name,user, visibileToOthers FROM mapAnalysis WHERE rws = {input$task}",
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
      }),
      input$task
    )

    observe({
      showModal(
        shiny::modalDialog(
          title = "Analyse laden",
          easyClose = TRUE,
          footer = NULL,
          size = "m",
          shiny::tagList(
            selectizeInput(
              NS(id, "analysisToLoad"),
              "Analyse auswählen",
              choices = choicesAnalysis()
            ),
            selected = choicesAnalysis()[1]
          ),
          actionButton(
            NS(id, "confirmLoad"),
            "Laden",
            class = "btn-primary"
          )
        )
      )
    }) |>
      shiny::bindEvent(input$loadAnalysis)

    observe({
      req(input$confirmLoad)
      if (is.null(input$analysisToLoad) || input$analysisToLoad == "") {
        showNotification(
          "Bitte eine Analyse zum Laden auswählen.",
          type = "error"
        )
        return()
      }
      analysis_id <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT id FROM mapAnalysis WHERE name = {input$analysisToLoad} AND rws = {input$task}",
          .con = conAnn
        )
      )$id[1]
      selectedAnalysis(analysis_id)

      # Load visibility status
      visibility <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT visibileToOthers FROM mapAnalysis WHERE id = {analysis_id}",
          .con = conAnn
        )
      )$visibileToOthers[1]

      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "analysisVisible",
        value = as.logical(visibility)
      )

      print(paste("Selected analysis ID:", selectedAnalysis()))
      removeModal()
    }) |>
      shiny::bindEvent(input$confirmLoad)

    observe({
      selectedAnalysis(NULL)
      # Reset sorting selections when task changes
      updateSelectizeInput(
        session = session,
        "sortStartCategory",
        choices = "",
        selected = ""
      )
      updateSelectizeInput(
        session = session,
        "sortEndCategory",
        choices = "",
        selected = ""
      )
      # Reset visibility switch
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "analysisVisible",
        value = FALSE
      )
    }) |>
      bindEvent(input$task)

    # Update analysis visibility in database
    observe({
      req(selectedAnalysis())
      req(input$analysisVisible)

      DBI::dbBegin(conAnn)
      DBI::dbExecute(
        conAnn,
        glue::glue_sql(
          "UPDATE mapAnalysis SET visibileToOthers = {as.integer(input$analysisVisible)} WHERE id = {selectedAnalysis()}",
          .con = conAnn
        )
      )
      DBI::dbCommit(conAnn)

      showNotification(
        if (input$analysisVisible) {
          "Analyse ist jetzt für andere Nutzer sichtbar."
        } else {
          "Analyse ist jetzt nur für Sie sichtbar."
        },
        type = "message",
        duration = 2
      )
    }) |>
      bindEvent(input$analysisVisible)

    observe({
      output$categoryOverviewUI <- renderUI({
        req(input$task)
        # req(selectedAnalysis())
        if (is.null(selectedAnalysis())) {
          return(shiny::p("Keine Analyse ausgewählt."))
        }
        categories <- c(
          "Unsortiert",
          DBI::dbGetQuery(
            conAnn,
            glue::glue_sql(
              "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
              .con = conAnn
            )
          )$category_name
        )
        if (length(categories) == 0) {
          return(shiny::p("Keine Kategorien definiert."))
        }

        catBoxes <- lapply(categories, function(cat) {
          bslib::card(
            title = cat,
            class = "info",
            min_height = "200px",
            style = "min-width: 300px; resize: both; overflow: auto; flex-shrink: 0;",
            p(paste("Kategorie:", cat)),
            # hier die werte der jeweiligen Kategorie einfügen, z.B. als Tabelle oder Liste
            shiny::renderUI({
              items <- if (cat == "Unsortiert") {
                transcriptions() |>
                  dplyr::filter(
                    !ipa %in%
                      DBI::dbGetQuery(
                        conAnn,
                        glue::glue_sql(
                          "SELECT ipa FROM mapAnalysisAnalysis
                          WHERE analysis_id={selectedAnalysis()} AND rws={input$task}",
                          .con = conAnn
                        )
                      )$ipa
                  ) |>
                  dplyr::pull(ipa) |>
                  unique()
              } else {
                DBI::dbGetQuery(
                  conAnn,
                  glue::glue_sql(
                    "SELECT ipa FROM mapAnalysisAnalysis 
                WHERE analysis_id={selectedAnalysis()} AND rws={input$task} AND category = {cat}",
                    .con = conAnn
                  )
                )$ipa
              }

              if (length(items) == 0) {
                shiny::p(
                  "Keine Items in dieser Kategorie.",
                  style = "color: #999; font-style: italic;"
                )
              } else {
                # Display items as formatted badges
                item_badges <- lapply(items, function(item) {
                  shiny::span(
                    item,
                    style = "display: inline-block; background-color: #e8f4f8; padding: 4px 8px; margin: 2px; border-radius: 4px; font-family: monospace; font-size: 12px; border: 1px solid #b3d9e8;"
                  )
                })
                shiny::div(
                  style = "padding: 8px; line-height: 1.8;",
                  item_badges
                )
              }
            })
          )
        })
        bslib::layout_column_wrap(
          width = 1 / length(categories),
          !!!catBoxes
        )
      })
    }) |>
      shiny::bindEvent(c(input$task, selectedAnalysis(), input$overview))

    ## manuelle Sortierung

    observe({
      req(selectedAnalysis())
      categories <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
          .con = conAnn
        )
      )$category_name

      # Always include "Unsortiert" as first option
      categories <- c("Unsortiert", categories)

      updateSelectizeInput(
        session = session,
        "sortStartCategory",
        choices = categories,
        selected = categories[1]
      )
      updateSelectizeInput(
        session = session,
        "sortEndCategory",
        choices = categories,
        selected = if (length(categories) > 1) categories[2] else categories[1]
      )
    }) |>
      shiny::bindEvent(selectedAnalysis())

    observe({
      output$sortBucketUI <- renderUI({
        req(input$sortStartCategory)
        req(input$sortEndCategory)
        req(selectedAnalysis())
        if (input$sortStartCategory == input$sortEndCategory) {
          return(p("Bitte unterschiedliche Kategorien auswählen."))
        }
        labels_unsorted <- if (input$sortStartCategory == "Unsortiert") {
          transcriptions() |>
            dplyr::filter(
              !ipa %in%
                DBI::dbGetQuery(
                  conAnn,
                  glue::glue_sql(
                    "SELECT ipa FROM mapAnalysisAnalysis
                    WHERE analysis_id={selectedAnalysis()}",
                    .con = conAnn
                  )
                )$ipa
            ) |>
            dplyr::pull(ipa) |>
            unique()
        } else {
          DBI::dbGetQuery(
            conAnn,
            glue::glue_sql(
              "SELECT ipa FROM mapAnalysisAnalysis 
              WHERE analysis_id={selectedAnalysis()} AND category = {input$sortStartCategory}",
              .con = conAnn
            )
          )$ipa
        }
        if (input$activeFilters != "" && length(input$activeFilters) > 0) {
          labels_unsorted <- apply_multiple_filters(
            labels_unsorted,
            filters(),
            input$activeFilters
          )
        }
        sortable::bucket_list(
          header = "Sortieren durch Ziehen und Ablegen",
          # id = NS(id, "sortBuckets"),
          group_name = "sortGroup",
          orientation = "horizontal",
          add_rank_list(
            text = input$sortStartCategory,
            labels = labels_unsorted,
            options = sortable_options(
              multiDrag = TRUE,
              animation = 0,
              sort = FALSE
            ),
            input_id = NS(id, "sortStartBucket")
          ),
          add_rank_list(
            text = input$sortEndCategory,
            labels = if (input$sortEndCategory == "Unsortiert") {
              transcriptions() |>
                dplyr::filter(
                  !ipa %in%
                    DBI::dbGetQuery(
                      conAnn,
                      glue::glue_sql(
                        "SELECT ipa FROM mapAnalysisAnalysis
                    WHERE analysis_id={selectedAnalysis()}",
                        .con = conAnn
                      )
                    )$ipa
                ) |>
                dplyr::pull(ipa) |>
                unique()
            } else {
              DBI::dbGetQuery(
                conAnn,
                glue::glue_sql(
                  "SELECT ipa FROM mapAnalysisAnalysis 
              WHERE analysis_id={selectedAnalysis()} AND rws={input$task} AND category = {input$sortEndCategory}",
                  .con = conAnn
                )
              )$ipa
            },
            options = sortable_options(
              multiDrag = TRUE,
              animation = 0,
              sort = FALSE
            ),
            input_id = NS(id, "sortEndBucket")
          )
        )
      })
    }) |>
      shiny::bindEvent(c(
        input$task,
        selectedAnalysis(),
        input$sortStartCategory,
        input$sortEndCategory,
        input$activeFilters
      ))

    observe({
      req(input$sortStartBucket)
      req(selectedAnalysis())
      req(input$task)
      ## speichern der Sortierung in der Datenbank
      if (input$sortStartCategory == "Unsortiert") {
        itemsToDelete <- DBI::dbGetQuery(
          conAnn,
          glue::glue_sql(
            "SELECT id FROM mapAnalysisAnalysis 
          WHERE analysis_id={selectedAnalysis()} AND rws={input$task} AND ipa IN ({input$sortStartBucket*})",
            .con = conAnn
          )
        ) |>
          dplyr::pull(id)
        if (length(itemsToDelete) > 0) {
          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "DELETE FROM mapAnalysisAnalysis WHERE id IN ({itemsToDelete*})",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)
        }
      } else {
        for (item in input$sortStartBucket) {
          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "INSERT INTO mapAnalysisAnalysis (analysis_id, rws, ipa, category, changed_by) 
              VALUES ({selectedAnalysis()}, {input$task}, {item}, {input$sortStartCategory}, {user})
              ON DUPLICATE KEY UPDATE category = {input$sortStartCategory}, changed_by = {user}
              ",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)
        }
      }
    }) |>
      shiny::bindEvent(input$sortStartBucket)

    observe({
      req(input$sortEndBucket)
      req(selectedAnalysis())
      req(input$task)

      ## speichern der Sortierung in der Datenbank
      if (input$sortEndCategory == "Unsortiert") {
        itemsToDelete <- DBI::dbGetQuery(
          conAnn,
          glue::glue_sql(
            "SELECT id FROM mapAnalysisAnalysis 
          WHERE analysis_id={selectedAnalysis()} AND rws={input$task} AND ipa IN ({input$sortEndBucket*})",
            .con = conAnn
          )
        ) |>
          dplyr::pull(id)
        if (length(itemsToDelete) > 0) {
          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "DELETE FROM mapAnalysisAnalysis WHERE id IN ({itemsToDelete*})",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)
        }
      } else {
        for (item in input$sortEndBucket) {
          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "INSERT INTO mapAnalysisAnalysis (analysis_id, rws, ipa, category, changed_by) 
              VALUES ({selectedAnalysis()}, {input$task}, {item}, {input$sortEndCategory}, {user})
              ON DUPLICATE KEY UPDATE category = {input$sortEndCategory}, changed_by = {user}
              ",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)
        }
      }
    }) |>
      shiny::bindEvent(input$sortEndBucket)

    # Add category modal
    shiny::observe({
      req(selectedAnalysis())
      showModal(
        shiny::modalDialog(
          title = "Kategorie hinzufügen",
          easyClose = TRUE,
          footer = NULL,
          size = "m",
          shiny::tagList(
            textInput(NS(id, "newCategoryName"), "Kategoriename"),
            actionButton(
              NS(id, "confirmAddCategory"),
              "Hinzufügen",
              class = "btn-primary"
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(input$addCategory)

    # Rename category modal
    shiny::observe({
      req(selectedAnalysis())
      categories <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
          .con = conAnn
        )
      )$category_name

      showModal(
        shiny::modalDialog(
          title = "Kategorie umbenennen",
          easyClose = TRUE,
          footer = NULL,
          size = "m",
          shiny::tagList(
            selectizeInput(
              NS(id, "categoryToRename"),
              "Kategorie auswählen",
              choices = categories
            ),
            textInput(NS(id, "newRenameCategoryName"), "Neuer Name"),
            actionButton(
              NS(id, "confirmRenameCategory"),
              "Umbenennen",
              class = "btn-primary"
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(input$renameCategory)

    # Remove category modal
    shiny::observe({
      req(selectedAnalysis())
      categories <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
          .con = conAnn
        )
      )$category_name

      showModal(
        shiny::modalDialog(
          title = "Kategorie löschen",
          easyClose = TRUE,
          footer = NULL,
          size = "m",
          shiny::tagList(
            selectizeInput(
              NS(id, "categoryToRemove"),
              "Kategorie auswählen",
              choices = categories
            ),
            p("Warnung: Diese Aktion kann nicht rückgängig gemacht werden."),
            actionButton(
              NS(id, "confirmRemoveCategory"),
              "Löschen",
              class = "btn-danger"
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(input$removeCategory)

    # Add category handler
    observe({
      req(input$confirmAddCategory)
      if (is.null(input$newCategoryName) || input$newCategoryName == "") {
        showNotification(
          "Bitte einen Kategorienamen eingeben.",
          type = "error"
        )
        return()
      }
      tryCatch(
        {
          categories <- strsplit(input$newCategoryName, ",")[[1]] |>
            trimws() |>
            unique()

          for (cat in categories) {
            DBI::dbBegin(conAnn)
            DBI::dbExecute(
              conAnn,
              glue::glue_sql(
                "INSERT INTO mapAnalysisCategory (analysis_id, category_name, created_by) VALUES ({selectedAnalysis()}, {cat}, {user})",
                .con = conAnn
              )
            )
            DBI::dbCommit(conAnn)
          }

          showNotification(
            glue::glue("Kategorie(n) erfolgreich hinzugefügt."),
            type = "message"
          )
        },
        error = function(e) {
          DBI::dbRollback(conAnn)
          showNotification(
            glue::glue("Fehler beim Hinzufügen der Kategorie: {e$message}"),
            type = "error"
          )
        },
        finally = {
          removeModal()
        }
      )
    }) |>
      bindEvent(input$confirmAddCategory)

    # Rename category handler
    observe({
      req(input$confirmRenameCategory)
      if (
        is.null(input$newRenameCategoryName) ||
          input$newRenameCategoryName == ""
      ) {
        showNotification(
          "Bitte einen neuen Kategorienamen eingeben.",
          type = "error"
        )
        return()
      }
      if (is.null(input$categoryToRename) || input$categoryToRename == "") {
        showNotification(
          "Bitte eine Kategorie auswählen.",
          type = "error"
        )
        return()
      }
      tryCatch(
        {
          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "UPDATE mapAnalysisCategory SET category_name = {input$newRenameCategoryName} WHERE analysis_id = {selectedAnalysis()} AND category_name = {input$categoryToRename}",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)

          # Also update references in mapAnalysisAnalysis table
          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "UPDATE mapAnalysisAnalysis SET category = {input$newRenameCategoryName} WHERE analysis_id = {selectedAnalysis()} AND category = {input$categoryToRename}",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)

          showNotification(
            glue::glue("Kategorie erfolgreich umbenannt."),
            type = "message"
          )
        },
        error = function(e) {
          DBI::dbRollback(conAnn)
          showNotification(
            glue::glue("Fehler beim Umbenennen der Kategorie: {e$message}"),
            type = "error"
          )
        },
        finally = {
          removeModal()
        }
      )
    }) |>
      bindEvent(input$confirmRenameCategory)

    # Remove category handler
    observe({
      req(input$confirmRemoveCategory)
      if (is.null(input$categoryToRemove) || input$categoryToRemove == "") {
        showNotification(
          "Bitte eine Kategorie auswählen.",
          type = "error"
        )
        return()
      }
      tryCatch(
        {
          # Delete from mapAnalysisAnalysis first (foreign key)
          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "DELETE FROM mapAnalysisAnalysis WHERE analysis_id = {selectedAnalysis()} AND category = {input$categoryToRemove}",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)

          # Then delete from mapAnalysisCategory
          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "DELETE FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()} AND category_name = {input$categoryToRemove}",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)
          showNotification(
            glue::glue("Kategorie erfolgreich gelöscht."),
            type = "message"
          )
        },
        error = function(e) {
          DBI::dbRollback(conAnn)
          showNotification(
            glue::glue("Fehler beim Löschen der Kategorie: {e$message}"),
            type = "error"
          )
        },
        finally = {
          removeModal()
        }
      )
    }) |>
      bindEvent(input$confirmRemoveCategory)

    observe({
      req(categories())
      # Include "Unsortiert" as first option
      categories_with_unsorted <- c("Unsortiert", categories())

      shiny::updateSelectizeInput(
        "sortStartCategory",
        choices = categories_with_unsorted,
        selected = input$sortStartCategory,
        session = session
      )
      shiny::updateSelectizeInput(
        "sortEndCategory",
        choices = categories_with_unsorted,
        selected = input$sortEndCategory,
        session = session
      )
    }) |>
      bindEvent(categories())

    observe({
      req(input$addFilter)
      req(selectedAnalysis())
      showModal(
        shiny::modalDialog(
          title = "Filter hinzufügen",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::tagList(
            textInput(
              NS(id, "FilterName"),
              "Filtername"
            ),
            bslib::layout_column_wrap(
              width = NULL,
              style = css(grid_template_columns = "2fr 2fr 2fr 1fr"),
              selectizeInput(
                NS(id, "Silbe"),
                "Silbe",
                choices = c(
                  "Erste Silbe",
                  "Letzte Silbe",
                  "Betonte Silben",
                  "Silbe mit Nebenakzent",
                  "Alle Silben"
                ),
                selected = "Alle Silben",
                multiple = FALSE,
              ),
              selectizeInput(
                NS(id, "FilterOperator"),
                "Operator",
                choices = c("beginnt mit", "endet mit", "enthält"),
                selected = "enthält",
                multiple = FALSE
              ),
              textInput(
                NS(id, "FilterValue"),
                "Wert"
              ),
              actionButton(
                NS(id, "IPAHelper"),
                "IPA-Hilfe",
                icon = shiny::icon("question-circle")
              )
            ),
            actionButton(
              NS(id, "confirmAddFilter"),
              "Hinzufügen",
              class = "btn-primary"
            )
          )
        )
      )
    }) |>
      bindEvent(c(input$addFilter))

    observe({
      shiny::browseURL("https://ipa.typeit.org/full/")
    }) |>
      shiny::bindEvent(input$IPAHelper)

    filters <- reactiveVal(data.frame(
      name = character(),
      silbe = character(),
      operator = character(),
      value = character(),
      stringsAsFactors = FALSE
    ))

    observe({
      req(input$confirmAddFilter)
      if (is.null(input$FilterName) || input$FilterName == "") {
        showNotification(
          "Bitte einen Namen für den Filter eingeben.",
          type = "error"
        )
        return()
      }
      if (is.null(input$FilterValue) || input$FilterValue == "") {
        showNotification(
          "Bitte einen Wert für den Filter eingeben.",
          type = "error"
        )
        return()
      }
      newFilter <- data.frame(
        name = input$FilterName,
        silbe = input$Silbe,
        operator = input$FilterOperator,
        value = input$FilterValue
      )
      # Hier müsste der neue Filter in einer reaktiven Variable oder Datenbanktabelle gespeichert werden, damit er in der App verwendet werden kann.
      filters(rbind(filters(), newFilter))
      # print(filters())
      showNotification(
        glue::glue("Filter '{input$FilterName}' erfolgreich hinzugefügt."),
        type = "message"
      )
      removeModal()
    }) |>
      bindEvent(input$confirmAddFilter)

    observe({
      updateSelectizeInput(
        session = session,
        "activeFilters",
        choices = filters()$name,
        selected = c(input$activeFilters, dplyr::last(filters()$name))
      )
    }) |>
      shiny::bindEvent(filters())
  })
}
