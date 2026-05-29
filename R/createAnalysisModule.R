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
        shiny::NS(id, "task"),
        label = "Aufgabe auswählen",
        choices = NULL,
        selected = NULL
      ),
      shiny::hr(),
      # Analysis management
      shiny::h6("Analyse verwalten"),
      shiny::actionButton(
        shiny::NS(id, "newAnalysis"),
        label = "Neue Analyse anlegen",
        icon = shiny::icon("plus-circle")
      ),
      shiny::actionButton(
        shiny::NS(id, "loadAnalysis"),
        label = "Analyse laden",
        icon = shiny::icon("folder-open")
      ),
      shiny::actionButton(
        shiny::NS(id, "deleteAnalysis"),
        label = "Analyse löschen",
        icon = shiny::icon("trash")
      ),
      shiny::hr(),
      # Analysis visibility
      shiny::h6("Sichtbarkeit"),
      shinyWidgets::switchInput(
        shiny::NS(id, "analysisVisible"),
        label = "Für andere sichtbar",
        value = FALSE,
        onStatus = "success",
        offStatus = "danger",
        size = "small",
        labelWidth = "80px"
      ),
      shiny::hr(),
      shiny::actionButton(
        shiny::NS(id, "addCategory"),
        label = "Kategorie hinzufügen",
        icon = shiny::icon("plus")
      ),
      shiny::actionButton(
        shiny::NS(id, "renameCategory"),
        label = "Kategorie umbenennen",
        icon = shiny::icon("pen")
      ),
      shiny::actionButton(
        shiny::NS(id, "removeCategory"),
        label = "Kategorie löschen",
        icon = shiny::icon("trash")
      )
    ),
    # Main content area with cards
    shiny::actionButton(
      shiny::NS(id, "overview"),
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
        shiny::uiOutput(shiny::NS(id, "categoryOverviewUI")) |>
          shinycssloaders::withSpinner(type = 4)
      ),
      bslib::card(
        title = "Sortieren",
        height = "800px",
        full_screen = TRUE,
        style = "min-width: 300px; display: flex; flex-direction: column; overflow: hidden; padding: 0;",
        # Fixed controls header — never shrinks regardless of bucket list size
        shiny::div(
          style = "flex: 0 0 auto; padding: 12px 16px; border-bottom: 1px solid var(--bs-border-color);",
          bslib::layout_column_wrap(
            width = 1 / 2,
            shiny::selectizeInput(
              shiny::NS(id, "sortStartCategory"),
              "Quelle",
              choices = ""
            ),
            shiny::selectizeInput(
              shiny::NS(id, "sortEndCategory"),
              "Ziel",
              choices = ""
            )
          ),
          shiny::h6("Filter", style = "margin-bottom: 6px;"),
          bslib::layout_column_wrap(
            width = NULL,
            style = htmltools::css(
              grid_template_columns = "3fr 1fr",
              align_items = "flex-end"
            ),
            shiny::selectizeInput(
              shiny::NS(id, "activeFilters"),
              label = NULL,
              choices = "",
              multiple = TRUE
            ),
            shiny::actionButton(
              shiny::NS(id, "addFilter"),
              "Filter hinzufügen",
              icon = shiny::icon("filter")
            )
          )
        ),
        # Scrollable bucket list — fills all remaining space
        shiny::div(
          style = "flex: 1 1 0; min-height: 0; overflow: auto; padding: 12px 16px;",
          shiny::uiOutput(shiny::NS(id, "sortBucketUI")) |>
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
    transcriptions <- shiny::reactive({
      shiny::req(conDMW)
      shiny::req(input$task)
      DBI::dbGetQuery(
        conDMW,
        glue::glue_sql(
          "SELECT * FROM Transcription WHERE rwl = {input$task}",
          .con = conDMW
        )
      )
    })

    selectedAnalysis <- shiny::reactiveVal(NULL)

    categories <- shiny::reactive({
      shiny::req(conAnn)
      shiny::req(selectedAnalysis())
      DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
          .con = conAnn
        )
      )$category_name
    }) |>
      shiny::bindEvent(c(
        selectedAnalysis(),
        input$confirmAddCategory,
        input$confirmRenameCategory,
        input$confirmRemoveCategory
      ))

    shiny::observe({
      shiny::req(conDMW)
      shiny::updateSelectizeInput(
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
        shiny::req(input$task)
        shiny::showModal(
          shiny::modalDialog(
            title = "Neue Analyse anlegen",
            easyClose = TRUE,
            footer = NULL,
            size = "m",
            shiny::tagList(
              shiny::textInput(shiny::NS(id, "analysisName"), "Name der Analyse"),
              shiny::textInput(
                shiny::NS(id, "analysisCategory"),
                "Kategorie der Analyse (mit Komma getrennt)"
              ),
              shiny::checkboxInput(
                shiny::NS(id, "visibileUser"),
                label = "Für andere Nutzer sichtbar",
                value = TRUE
              ),
              shiny::actionButton(
                shiny::NS(id, "confirmCreate"),
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
        shiny::req(input$confirmCreate)
        if (is.null(input$analysisName) || input$analysisName == "") {
          shiny::showNotification(
            "Bitte einen Namen für die Analyse eingeben.",
            type = "error"
          )
          return()
        }
        if (is.null(input$task) || input$task == "") {
          shiny::showNotification("Bitte eine Aufgabe auswählen.", type = "error")
          return()
        }
        tryCatch(
          {
            DBI::dbBegin(conAnn)
            DBI::dbExecute(
              conAnn,
              glue::glue_sql(
                "INSERT INTO mapAnalysis (name, rws,user, visibileToOthers) VALUES ({input$analysisName}, {input$task},{user}, {input$visibileUser})",
                .con = conAnn
              )
            )
            DBI::dbCommit(conAnn)
            if (
              !is.null(input$analysisCategory) && input$analysisCategory != ""
            ) {
              cats <- strsplit(input$analysisCategory, ",")[[1]] |>
                trimws() |>
                unique()
              if (length(cats) == 0) {
                return()
              }
              analysis_id <- DBI::dbGetQuery(
                conAnn,
                glue::glue_sql(
                  "SELECT id FROM mapAnalysis WHERE name = {input$analysisName} AND rws = {input$task}",
                  .con = conAnn
                )
              )$id[1]

              for (cat in cats) {
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
            shiny::showNotification(
              glue::glue("Fehler beim Erstellen der Analyse: {e$message}"),
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
            shiny::removeModal()
          }
        )
      }),
      input$confirmCreate
    )

    choicesAnalysis <- shiny::bindEvent(
      shiny::reactive({
        shiny::req(conAnn)
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

    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Analyse laden",
          easyClose = TRUE,
          footer = NULL,
          size = "m",
          shiny::tagList(
            shiny::selectizeInput(
              shiny::NS(id, "analysisToLoad"),
              "Analyse auswählen",
              choices = choicesAnalysis()
            ),
            selected = choicesAnalysis()[1]
          ),
          shiny::actionButton(
            shiny::NS(id, "confirmLoad"),
            "Laden",
            class = "btn-primary"
          )
        )
      )
    }) |>
      shiny::bindEvent(input$loadAnalysis)

    shiny::observe({
      shiny::req(input$confirmLoad)
      if (is.null(input$analysisToLoad) || input$analysisToLoad == "") {
        shiny::showNotification(
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
      shiny::removeModal()
    }) |>
      shiny::bindEvent(input$confirmLoad)

    shiny::observe({
      selectedAnalysis(NULL)
      shiny::updateSelectizeInput(
        session = session,
        "sortStartCategory",
        choices = "",
        selected = ""
      )
      shiny::updateSelectizeInput(
        session = session,
        "sortEndCategory",
        choices = "",
        selected = ""
      )
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "analysisVisible",
        value = FALSE
      )
    }) |>
      shiny::bindEvent(input$task)

    # Update analysis visibility in database
    shiny::observe({
      shiny::req(selectedAnalysis())
      shiny::req(input$analysisVisible)

      DBI::dbBegin(conAnn)
      DBI::dbExecute(
        conAnn,
        glue::glue_sql(
          "UPDATE mapAnalysis SET visibileToOthers = {as.integer(input$analysisVisible)} WHERE id = {selectedAnalysis()}",
          .con = conAnn
        )
      )
      DBI::dbCommit(conAnn)

      shiny::showNotification(
        if (input$analysisVisible) {
          "Analyse ist jetzt für andere Nutzer sichtbar."
        } else {
          "Analyse ist jetzt nur für Sie sichtbar."
        },
        type = "message",
        duration = 2
      )
    }) |>
      shiny::bindEvent(input$analysisVisible)

    shiny::observe({
      output$categoryOverviewUI <- shiny::renderUI({
        shiny::req(input$task)
        if (is.null(selectedAnalysis())) {
          return(shiny::p("Keine Analyse ausgewählt."))
        }
        cats <- c(
          "Unsortiert",
          DBI::dbGetQuery(
            conAnn,
            glue::glue_sql(
              "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
              .con = conAnn
            )
          )$category_name
        )
        if (length(cats) == 0) {
          return(shiny::p("Keine Kategorien definiert."))
        }

        catBoxes <- lapply(cats, function(cat) {
          bslib::card(
            title = cat,
            class = "info",
            min_height = "200px",
            style = "min-width: 300px; resize: both; overflow: auto; flex-shrink: 0;",
            shiny::p(paste("Kategorie:", cat)),
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
          width = 1 / length(cats),
          !!!catBoxes
        )
      })
    }) |>
      shiny::bindEvent(c(input$task, selectedAnalysis(), input$overview))

    ## manuelle Sortierung

    shiny::observe({
      shiny::req(selectedAnalysis())
      cats <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
          .con = conAnn
        )
      )$category_name

      cats <- c("Unsortiert", cats)

      shiny::updateSelectizeInput(
        session = session,
        "sortStartCategory",
        choices = cats,
        selected = cats[1]
      )
      shiny::updateSelectizeInput(
        session = session,
        "sortEndCategory",
        choices = cats,
        selected = if (length(cats) > 1) cats[2] else cats[1]
      )
    }) |>
      shiny::bindEvent(selectedAnalysis())

    shiny::observe({
      output$sortBucketUI <- shiny::renderUI({
        shiny::req(input$sortStartCategory)
        shiny::req(input$sortEndCategory)
        shiny::req(selectedAnalysis())
        if (input$sortStartCategory == input$sortEndCategory) {
          return(shiny::p("Bitte unterschiedliche Kategorien auswählen."))
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
          group_name = "sortGroup",
          orientation = "horizontal",
          sortable::add_rank_list(
            text = input$sortStartCategory,
            labels = labels_unsorted,
            options = sortable::sortable_options(
              multiDrag = TRUE,
              animation = 0,
              sort = FALSE
            ),
            input_id = shiny::NS(id, "sortStartBucket")
          ),
          sortable::add_rank_list(
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
            options = sortable::sortable_options(
              multiDrag = TRUE,
              animation = 0,
              sort = FALSE
            ),
            input_id = shiny::NS(id, "sortEndBucket")
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

    shiny::observe({
      shiny::req(input$sortStartBucket)
      shiny::req(selectedAnalysis())
      shiny::req(input$task)
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
              ON DUPLICATE KEY UPDATE category = {input$sortStartCategory}, changed_by = {user}",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)
        }
      }
    }) |>
      shiny::bindEvent(input$sortStartBucket)

    shiny::observe({
      shiny::req(input$sortEndBucket)
      shiny::req(selectedAnalysis())
      shiny::req(input$task)
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
              ON DUPLICATE KEY UPDATE category = {input$sortEndCategory}, changed_by = {user}",
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
      shiny::req(selectedAnalysis())
      shiny::showModal(
        shiny::modalDialog(
          title = "Kategorie hinzufügen",
          easyClose = TRUE,
          footer = NULL,
          size = "m",
          shiny::tagList(
            shiny::textInput(shiny::NS(id, "newCategoryName"), "Kategoriename"),
            shiny::actionButton(
              shiny::NS(id, "confirmAddCategory"),
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
      shiny::req(selectedAnalysis())
      cats <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
          .con = conAnn
        )
      )$category_name

      shiny::showModal(
        shiny::modalDialog(
          title = "Kategorie umbenennen",
          easyClose = TRUE,
          footer = NULL,
          size = "m",
          shiny::tagList(
            shiny::selectizeInput(
              shiny::NS(id, "categoryToRename"),
              "Kategorie auswählen",
              choices = cats
            ),
            shiny::textInput(shiny::NS(id, "newRenameCategoryName"), "Neuer Name"),
            shiny::actionButton(
              shiny::NS(id, "confirmRenameCategory"),
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
      shiny::req(selectedAnalysis())
      cats <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category_name FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()}",
          .con = conAnn
        )
      )$category_name

      shiny::showModal(
        shiny::modalDialog(
          title = "Kategorie löschen",
          easyClose = TRUE,
          footer = NULL,
          size = "m",
          shiny::tagList(
            shiny::selectizeInput(
              shiny::NS(id, "categoryToRemove"),
              "Kategorie auswählen",
              choices = cats
            ),
            shiny::p("Warnung: Diese Aktion kann nicht rückgängig gemacht werden."),
            shiny::actionButton(
              shiny::NS(id, "confirmRemoveCategory"),
              "Löschen",
              class = "btn-danger"
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(input$removeCategory)

    # Add category handler
    shiny::observe({
      shiny::req(input$confirmAddCategory)
      if (is.null(input$newCategoryName) || input$newCategoryName == "") {
        shiny::showNotification(
          "Bitte einen Kategorienamen eingeben.",
          type = "error"
        )
        return()
      }
      tryCatch(
        {
          cats <- strsplit(input$newCategoryName, ",")[[1]] |>
            trimws() |>
            unique()

          for (cat in cats) {
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

          shiny::showNotification(
            glue::glue("Kategorie(n) erfolgreich hinzugefügt."),
            type = "message"
          )
        },
        error = function(e) {
          DBI::dbRollback(conAnn)
          shiny::showNotification(
            glue::glue("Fehler beim Hinzufügen der Kategorie: {e$message}"),
            type = "error"
          )
        },
        finally = {
          shiny::removeModal()
        }
      )
    }) |>
      shiny::bindEvent(input$confirmAddCategory)

    # Rename category handler
    shiny::observe({
      shiny::req(input$confirmRenameCategory)
      if (
        is.null(input$newRenameCategoryName) ||
          input$newRenameCategoryName == ""
      ) {
        shiny::showNotification(
          "Bitte einen neuen Kategorienamen eingeben.",
          type = "error"
        )
        return()
      }
      if (is.null(input$categoryToRename) || input$categoryToRename == "") {
        shiny::showNotification(
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

          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "UPDATE mapAnalysisAnalysis SET category = {input$newRenameCategoryName} WHERE analysis_id = {selectedAnalysis()} AND category = {input$categoryToRename}",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)

          shiny::showNotification(
            glue::glue("Kategorie erfolgreich umbenannt."),
            type = "message"
          )
        },
        error = function(e) {
          DBI::dbRollback(conAnn)
          shiny::showNotification(
            glue::glue("Fehler beim Umbenennen der Kategorie: {e$message}"),
            type = "error"
          )
        },
        finally = {
          shiny::removeModal()
        }
      )
    }) |>
      shiny::bindEvent(input$confirmRenameCategory)

    # Remove category handler
    shiny::observe({
      shiny::req(input$confirmRemoveCategory)
      if (is.null(input$categoryToRemove) || input$categoryToRemove == "") {
        shiny::showNotification(
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
              "DELETE FROM mapAnalysisAnalysis WHERE analysis_id = {selectedAnalysis()} AND category = {input$categoryToRemove}",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)

          DBI::dbBegin(conAnn)
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "DELETE FROM mapAnalysisCategory WHERE analysis_id = {selectedAnalysis()} AND category_name = {input$categoryToRemove}",
              .con = conAnn
            )
          )
          DBI::dbCommit(conAnn)
          shiny::showNotification(
            glue::glue("Kategorie erfolgreich gelöscht."),
            type = "message"
          )
        },
        error = function(e) {
          DBI::dbRollback(conAnn)
          shiny::showNotification(
            glue::glue("Fehler beim Löschen der Kategorie: {e$message}"),
            type = "error"
          )
        },
        finally = {
          shiny::removeModal()
        }
      )
    }) |>
      shiny::bindEvent(input$confirmRemoveCategory)

    shiny::observe({
      shiny::req(categories())
      cats_with_unsorted <- c("Unsortiert", categories())

      shiny::updateSelectizeInput(
        "sortStartCategory",
        choices = cats_with_unsorted,
        selected = input$sortStartCategory,
        session = session
      )
      shiny::updateSelectizeInput(
        "sortEndCategory",
        choices = cats_with_unsorted,
        selected = input$sortEndCategory,
        session = session
      )
    }) |>
      shiny::bindEvent(categories())

    shiny::observe({
      shiny::req(input$addFilter)
      shiny::req(selectedAnalysis())
      shiny::showModal(
        shiny::modalDialog(
          title = "Filter hinzufügen",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::tagList(
            shiny::textInput(
              shiny::NS(id, "FilterName"),
              "Filtername"
            ),
            bslib::layout_column_wrap(
              width = NULL,
              style = htmltools::css(grid_template_columns = "2fr 2fr 2fr 1fr"),
              shiny::selectizeInput(
                shiny::NS(id, "Silbe"),
                "Silbe",
                choices = c(
                  "Erste Silbe",
                  "Letzte Silbe",
                  "Betonte Silben",
                  "Silbe mit Nebenakzent",
                  "Alle Silben"
                ),
                selected = "Alle Silben",
                multiple = FALSE
              ),
              shiny::selectizeInput(
                shiny::NS(id, "FilterOperator"),
                "Operator",
                choices = c("beginnt mit", "endet mit", "enthält"),
                selected = "enthält",
                multiple = FALSE
              ),
              shiny::textInput(
                shiny::NS(id, "FilterValue"),
                "Wert"
              ),
              shiny::actionButton(
                shiny::NS(id, "IPAHelper"),
                "IPA-Hilfe",
                icon = shiny::icon("question-circle")
              )
            ),
            shiny::actionButton(
              shiny::NS(id, "confirmAddFilter"),
              "Hinzufügen",
              class = "btn-primary"
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(input$addFilter)

    shiny::observe({
      shiny::browseURL("https://ipa.typeit.org/full/")
    }) |>
      shiny::bindEvent(input$IPAHelper)

    filters <- shiny::reactiveVal(data.frame(
      name = character(),
      silbe = character(),
      operator = character(),
      value = character(),
      stringsAsFactors = FALSE
    ))

    shiny::observe({
      shiny::req(input$confirmAddFilter)
      if (is.null(input$FilterName) || input$FilterName == "") {
        shiny::showNotification(
          "Bitte einen Namen für den Filter eingeben.",
          type = "error"
        )
        return()
      }
      if (is.null(input$FilterValue) || input$FilterValue == "") {
        shiny::showNotification(
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
      filters(rbind(filters(), newFilter))
      shiny::showNotification(
        glue::glue("Filter '{input$FilterName}' erfolgreich hinzugefügt."),
        type = "message"
      )
      shiny::removeModal()
    }) |>
      shiny::bindEvent(input$confirmAddFilter)

    shiny::observe({
      shiny::updateSelectizeInput(
        session = session,
        "activeFilters",
        choices = filters()$name,
        selected = c(input$activeFilters, dplyr::last(filters()$name))
      )
    }) |>
      shiny::bindEvent(filters())
  })
}
