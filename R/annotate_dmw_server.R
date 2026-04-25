#' DMW Annotation Tool Server Module
#'
#' Server-side logic for the DMW annotation tool. Manages item navigation,
#' annotation saving, category management, and data export.
#'
#' @param id Character string. The module ID, must match the UI module.
#' @param conAnn DBI connection object. Database connection for annotation data.
#' @param conDMW DBI connection object. Database connection for DMW reference data.
#' @param username Character string. Username of the current annotator.
#'
#' @return Invisibly returns NULL (typical for Shiny server modules).
#'
#' @details
#' This module handles:
#' - Dynamic selection of phenomena and tasks
#' - Item navigation with optional saving
#' - Audio playback with speed control
#' - Annotation storage and retrieval
#' - Category management
#' - Item filtering (doubt/unusable states)
#' - Excel export functionality
#' - REDE format export
#'
#' The module maintains reactive values for:
#' - Current item position
#' - Current category
#' - Loaded items and categories
#'
#' @section Database Requirements:
#' The annotation database should contain tables:
#' - phaenomen
#' - categories
#' - annotations
#' - itemDoubt
#' - itemUnusable
#' - comments
#' - user_last_item
#'
#' @section Dependencies:
#' Requires packages: shiny, DBI, dplyr, glue, howler, stringr, openxlsx
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Shiny app
#' shiny::moduleServer("annotate_dmw", annotateDmwServer,
#'   args = list(
#'     conAnn = con_annotation,
#'     conDMW = con_dmw,
#'     username = "annotator1"
#'   )
#' )
#' }
annotateDmwServer <- function(id, conAnn, conDMW, username) {
  shiny::moduleServer(id, function(input, output, session) {
    print("Annotate DMW Module Server gestartet")
    if (DBI::dbIsValid(conAnn)) {
      print("Annotation DB connection is valid.")
    } else {
      stop("Annotation DB connection is not valid.")
    }

    if (DBI::dbIsValid(conDMW)) {
      print("DMW DB connection is valid.")
    } else {
      stop("DMW DB connection is not valid.")
    }
    observe({
      phaenomene <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT DISTINCT Phänomen FROM phaenomen WHERE Systemebene = {input$selectSystemebene}",
          .con = conAnn
        )
      ) |>
        dplyr::arrange(Phänomen) |>
        dplyr::pull(1)

      updateSelectizeInput(
        session,
        "selectPhaenomen",
        choices = phaenomene,
        selected = phaenomene[1],
        server = TRUE
      )
    }) |>
      shiny::bindEvent(input$selectSystemebene)

    shiny::observe({
      req(input$selectPhaenomen)
      aufgaben <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT Teilaufgaben_ID, Vollform FROM phaenomen WHERE Phänomen = {input$selectPhaenomen}",
          .con = conAnn
        )
      ) |>
        dplyr::arrange(Teilaufgaben_ID) |>
        dplyr::mutate(
          choice_label = paste0(Teilaufgaben_ID, ": ", Vollform)
        )
      aufgaben <- aufgaben |> dplyr::pull(Teilaufgaben_ID) |> as.list()
      aufgaben <- aufgaben |> setNames(aufgaben$choice_label)
      shiny::updateSelectizeInput(
        session,
        "selectTask",
        choices = aufgaben,
        selected = aufgaben[[1]]
      )
    }) |>
      shiny::bindEvent(input$selectPhaenomen)

    categories <- reactiveVal(NULL)

    observe({
      req(input$selectTask)
      cats <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category FROM categories 
            WHERE subtask_id IN ({input$selectTask*}) ORDER BY id",
          .con = conAnn
        )
      ) |>
        pull(1) |>
        unique()
      categories(cats)
      if (length(categories()) == 0) {
        categoryNumber(0)
        print("Keine Kategorien vorhanden")
        updateActionButton(session, "previousCategory", disabled = TRUE)
        updateActionButton(session, "nextCategory", disabled = TRUE)
        updateActionButton(session, "previousCategoryNoSave", disabled = TRUE)
        updateActionButton(session, "nextCategoryNoSave", disabled = TRUE)
      } else if (length(categories()) == 1) {
        categoryNumber(1)
        print("Nur eine Kategorie vorhanden")
        updateActionButton(session, "previousCategory", disabled = TRUE)
        updateActionButton(session, "nextCategory", disabled = TRUE)
        updateActionButton(session, "previousCategoryNoSave", disabled = TRUE)
        updateActionButton(session, "nextCategoryNoSave", disabled = TRUE)
      } else {
        categoryNumber(1)
        updateActionButton(session, "previousCategory", disabled = TRUE)
        updateActionButton(session, "nextCategory", disabled = FALSE)
        updateActionButton(session, "previousCategoryNoSave", disabled = TRUE)
        updateActionButton(session, "nextCategoryNoSave", disabled = FALSE)
      }
    }) |>
      shiny::bindEvent(input$selectTask)

    categoryNumber <- shiny::reactiveVal(1)

    observe({
      req(input$selectTask)
      cats <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category FROM categories 
            WHERE subtask_id IN ({input$selectTask*}) ORDER BY id",
          .con = conAnn
        )
      ) |>
        pull(1)
      categories(cats)
      if (length(cats) > 0) {
        categoryNumber(1)
      } else {
        categoryNumber(0)
      }
    }) |>
      shiny::bindEvent(input$selectTask)

    observe({
      req(categories())
      if (categoryNumber() <= 1) {
        updateActionButton(session, "previousCategory", disabled = TRUE)
        updateActionButton(session, "previousCategoryNoSave", disabled = TRUE)
      }
      if (categoryNumber() > 1) {
        updateActionButton(session, "previousCategory", disabled = FALSE)
        updateActionButton(session, "previousCategoryNoSave", disabled = FALSE)
      }
      if (categoryNumber() < length(categories())) {
        updateActionButton(session, "nextCategory", disabled = FALSE)
        updateActionButton(session, "nextCategoryNoSave", disabled = FALSE)
      }
      if (categoryNumber() >= length(categories())) {
        updateActionButton(session, "nextCategory", disabled = TRUE)
        updateActionButton(session, "nextCategoryNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(categoryNumber())

    observe({
      req(categories())
      if (categoryNumber() < length(categories())) {
        categoryNumber(categoryNumber() + 1)
      }
      if (categoryNumber() == length(categories())) {
        shiny::showNotification("Letzte Kategorie erreicht.", type = "message")
        updateActionButton(session, "nextCategory", disabled = TRUE)
        updateActionButton(session, "nextCategoryNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(input$nextCategory)

    observe({
      req(categories())
      if (categoryNumber() < length(categories())) {
        categoryNumber(categoryNumber() + 1)
      }
      if (categoryNumber() == length(categories())) {
        shiny::showNotification("Letzte Kategorie erreicht.", type = "message")
        updateActionButton(session, "nextCategory", disabled = TRUE)
        updateActionButton(session, "nextCategoryNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(input$nextCategoryNoSave)

    observe({
      req(categories())
      if (categoryNumber() > 1) {
        categoryNumber(categoryNumber() - 1)
      }
      if (categoryNumber() == 1) {
        shiny::showNotification("Erste Kategorie erreicht.", type = "message")
        updateActionButton(session, "previousCategory", disabled = TRUE)
        updateActionButton(session, "previousCategoryNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(input$previousCategory)

    observe({
      req(categories())
      if (categoryNumber() > 1) {
        categoryNumber(categoryNumber() - 1)
      }
      if (categoryNumber() == 1) {
        shiny::showNotification("Erste Kategorie erreicht.", type = "message")
        updateActionButton(session, "previousCategory", disabled = TRUE)
        updateActionButton(session, "previousCategoryNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(input$previousCategoryNoSave)

    observe({
      req(items())
      if (itemRow() < nrow(items())) {
        itemRow(itemRow() + 1)
      }
      if (itemRow() == nrow(items())) {
        shiny::showNotification("Letztes Item erreicht.", type = "message")
        updateActionButton(session, "nextItem", disabled = TRUE)
        updateActionButton(session, "nextItemNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(input$nextItem)

    observe({
      req(items())
      if (itemRow() < nrow(items())) {
        itemRow(itemRow() + 1)
      }
      if (itemRow() == nrow(items())) {
        shiny::showNotification("Letztes Item erreicht.", type = "message")
        updateActionButton(session, "nextItem", disabled = TRUE)
        updateActionButton(session, "nextItemNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(input$nextItemNoSave)

    observe({
      req(items())
      if (itemRow() > 1) {
        itemRow(itemRow() - 1)
      }
      if (itemRow() == 1) {
        shiny::showNotification("Erstes Item erreicht.", type = "message")
        updateActionButton(session, "previousItem", disabled = TRUE)
        updateActionButton(session, "previousItemNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(input$previousItem)

    observe({
      req(items())
      if (itemRow() > 1) {
        itemRow(itemRow() - 1)
      }
      if (itemRow() == 1) {
        shiny::showNotification("Erstes Item erreicht.", type = "message")
        updateActionButton(session, "previousItem", disabled = TRUE)
        updateActionButton(session, "previousItemNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(input$previousItemNoSave)

    output$currentCategory <- shiny::renderText({
      if (
        is.null(categories()) ||
          length(categories()) == 0 ||
          categoryNumber() == 0
      ) {
        "Annotation"
      } else {
        current_cat <- categories()[categoryNumber()]
        if (current_cat == "Phänomen") {
          vollform <- DBI::dbGetQuery(
            conAnn,
            glue::glue_sql(
              "SELECT Vollform FROM phaenomen 
               WHERE Teilaufgaben_ID = {input$selectTask}
               LIMIT 1",
              .con = conAnn
            )
          ) |>
            dplyr::pull(1)

          if (length(vollform) > 0 && !is.na(vollform)) {
            paste0("Phänomen: ", vollform)
          } else {
            "Phänomen"
          }
        } else {
          current_cat
        }
      }
    })

    observe({
      shiny::req(input$selectTask)
      choices <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "
          SELECT DISTINCT Varianten FROM varianten
          WHERE Teilaufgaben_ID IN ({input$selectTask*})
          ",
          .con = conAnn
        )
      ) |>
        dplyr::arrange(Varianten) |>
        dplyr::pull(1)

      if (length(choices) == 0 || is.null(choices)) {
        choices <- c(" ")
      } else {
        choices <- c(" ", choices)
      }

      shiny::updateSelectizeInput(
        session,
        "annValue",
        choices = choices,
        selected = choices[1],
        server = TRUE,
        options = list(
          create = TRUE,
          createOnBlur = TRUE,
          placeholder = "Variante auswählen oder neue eingeben..."
        )
      )
    }) |>
      shiny::bindEvent(input$selectTask)

    allItems <- shiny::reactive({
      req(input$selectTask)
      DBI::dbGetQuery(
        conDMW,
        glue::glue_sql(
          "
          SELECT * FROM TrackAudioProcess
          WHERE subtask_id IN ({input$selectTask*})
          ORDER BY subtask_id, informant_id ",
          .con = conDMW
        )
      )
    })

    items <- shiny::reactive({
      req(allItems())

      if (isTRUE(input$onlyDoubt)) {
        doubt_items <- DBI::dbGetQuery(
          conAnn,
          glue::glue_sql(
            "SELECT informant_id, subtask_id FROM itemDoubt 
             WHERE subtask_id IN ({input$selectTask*}) 
             AND is_doubt = TRUE",
            .con = conAnn
          )
        )

        if (nrow(doubt_items) == 0) {
          shiny::showNotification(
            "Keine Zweifelsfälle gefunden.",
            type = "warning"
          )
          return(allItems()[0, ])
        }

        doubt_items <- doubt_items |>
          dplyr::mutate(
            informant_id = as.character(informant_id),
            subtask_id = as.character(subtask_id)
          )

        filtered <- allItems() |>
          dplyr::mutate(
            informant_id = as.character(informant_id),
            subtask_id = as.character(subtask_id)
          ) |>
          dplyr::semi_join(
            doubt_items,
            by = c("informant_id", "subtask_id")
          )

        if (nrow(filtered) == 0) {
          shiny::showNotification(
            "Keine Zweifelsfälle gefunden.",
            type = "warning"
          )
        }

        filtered
      } else {
        allItems()
      }
    })

    isFirstLoad <- reactiveVal(TRUE)

    observe({
      req(items())
      if (isTRUE(isFirstLoad())) {
        isFirstLoad(FALSE)
        last_item <- DBI::dbGetQuery(
          conAnn,
          glue::glue_sql(
            "SELECT last_item_id FROM user_last_item
             WHERE username = {username}
             AND subtask_id = {input$selectTask}
             LIMIT 1",
            .con = conAnn
          )
        )
        if (nrow(last_item) > 0 && !is.na(last_item$last_item_id[1])) {
          target_row <- last_item$last_item_id[1]
          target_row <- min(target_row, nrow(items()))
          target_row <- max(target_row, 1L)
          itemRow(target_row)
        } else {
          itemRow(1)
        }
      } else {
        itemRow(1)
      }
    }) |>
      bindEvent(input$onlyDoubt, input$selectTask)

    itemRow <- shiny::reactiveVal(1)

    observe({
      req(items())
      req(itemRow())
      req(input$selectTask)
      DBI::dbExecute(
        conAnn,
        glue::glue_sql(
          "INSERT INTO user_last_item (username, subtask_id, last_item_id)
           VALUES ({username}, {input$selectTask}, {itemRow()})
           ON DUPLICATE KEY UPDATE
             last_item_id = VALUES(last_item_id),
             updated_at = CURRENT_TIMESTAMP;",
          .con = conAnn
        )
      )
    }) |>
      bindEvent(itemRow(), ignoreInit = TRUE)

    observe({
      req(items())
      if (itemRow() == 1) {
        updateActionButton(session, "previousItem", disabled = TRUE)
        updateActionButton(session, "previousItemNoSave", disabled = TRUE)
      }
      if (itemRow() > 1) {
        updateActionButton(session, "previousItem", disabled = FALSE)
        updateActionButton(session, "previousItemNoSave", disabled = FALSE)
      }
      if (itemRow() < nrow(items())) {
        updateActionButton(session, "nextItem", disabled = FALSE)
        updateActionButton(session, "nextItemNoSave", disabled = FALSE)
      }
      if (itemRow() == nrow(items())) {
        updateActionButton(session, "nextItem", disabled = TRUE)
        updateActionButton(session, "nextItemNoSave", disabled = TRUE)
      }
    }) |>
      bindEvent(itemRow())

    observe({
      output$currentItem <- shiny::renderText({
        items()[itemRow(), "id"]
      })
    })

    observe({
      output$itemCounter <- shiny::renderText({
        req(items())
        req(itemRow())
        paste0(itemRow(), " / ", nrow(items()))
      })
    }) |>
      bindEvent(c(itemRow(), items()))

    observe({
      shiny::updateSelectizeInput(session, "annValue", selected = 1)
    }) |>
      bindEvent(itemRow())

    observe({
      req(items())
      req(itemRow())

      doubt_state <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT is_doubt FROM itemDoubt 
           WHERE informant_id = {items()[itemRow(), 'informant_id']}
           AND subtask_id = {items()[itemRow(), 'subtask_id']}
           LIMIT 1",
          .con = conAnn
        )
      )

      if (
        nrow(doubt_state) > 0 &&
          !is.na(doubt_state$is_doubt[1]) &&
          as.logical(doubt_state$is_doubt[1]) == TRUE
      ) {
        bslib::update_switch(
          id = "unclear",
          session = session,
          value = TRUE
        )
      } else {
        bslib::update_switch(
          id = "unclear",
          session = session,
          value = FALSE
        )
      }
    }) |>
      bindEvent(itemRow())

    observe({
      req(items())
      req(itemRow())

      DBI::dbExecute(
        conAnn,
        glue::glue_sql(
          "INSERT INTO itemDoubt (informant_id, subtask_id, is_doubt, username)
           VALUES (
             {items()[itemRow(), 'informant_id']},
             {items()[itemRow(), 'subtask_id']},
             {input$unclear},
             {username})
           ON DUPLICATE KEY UPDATE
             is_doubt = VALUES(is_doubt),
             username = VALUES(username),
             created_at = CURRENT_TIMESTAMP;",
          .con = conAnn
        )
      )
    }) |>
      bindEvent(input$unclear, ignoreInit = TRUE)

    observe({
      req(items())
      req(itemRow())

      unusable_state <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT is_unusable FROM itemUnusable 
           WHERE informant_id = {items()[itemRow(), 'informant_id']}
           AND subtask_id = {items()[itemRow(), 'subtask_id']}
           LIMIT 1",
          .con = conAnn
        )
      )

      if (
        nrow(unusable_state) > 0 &&
          !is.na(unusable_state$is_unusable[1]) &&
          as.logical(unusable_state$is_unusable[1]) == TRUE
      ) {
        bslib::update_switch(
          id = "unusable",
          session = session,
          value = TRUE
        )
      } else {
        bslib::update_switch(
          id = "unusable",
          session = session,
          value = FALSE
        )
      }
    }) |>
      bindEvent(itemRow())

    observe({
      req(items())
      req(itemRow())

      DBI::dbExecute(
        conAnn,
        glue::glue_sql(
          "INSERT INTO itemUnusable (informant_id, subtask_id, is_unusable, username)
           VALUES (
             {items()[itemRow(), 'informant_id']},
             {items()[itemRow(), 'subtask_id']},
             {input$unusable},
             {username})
           ON DUPLICATE KEY UPDATE
             is_unusable = VALUES(is_unusable),
             username = VALUES(username),
             created_at = CURRENT_TIMESTAMP;",
          .con = conAnn
        )
      )
    }) |>
      bindEvent(input$unusable, ignoreInit = TRUE)

    output$comment <- shiny::renderUI({
      req(items())
      req(itemRow())
      comments <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT comment_text FROM comments
        WHERE annotation_id = {items()[itemRow(), 'id']}
        ORDER BY id DESC",
          .con = conAnn
        )
      ) |>
        dplyr::pull(1)
      print(comments)
      if (length(comments) > 0) {
        output$comment <- shiny::renderUI({
          shiny::tags$div(
            style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
            shiny::tags$p(glue::glue_collapse(comments, sep = "\n"))
          )
        })
      } else {
        output$comment <- shiny::renderUI({
          tags$p()
        })
      }
    })
    observe({
      req(items())
      req(itemRow())
      comments <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT comment_text FROM comments
        WHERE annotation_id = {items()[itemRow(), 'id']}
        ORDER BY id DESC",
          .con = conAnn
        )
      ) |>
        dplyr::pull(1)
      print(comments)
      if (length(comments) > 0) {
        output$comment <- shiny::renderUI({
          shiny::tags$div(
            style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
            shiny::tags$p(glue::glue_collapse(comments, sep = "\n"))
          )
        })
      } else {
        output$comment <- shiny::renderUI({
          tags$p()
        })
      }
    }) |>
      bindEvent(itemRow())

    observe({
      output$progressPercent <- shiny::renderText({
        req(items())
        req(itemRow())
        annotated <- DBI::dbGetQuery(
          conAnn,
          glue::glue_sql(
            "SELECT COUNT(*) AS n FROM annotations
            WHERE subtask_id IN ({input$selectTask*})",
            .con = conAnn
          )
        ) |>
          dplyr::pull(n)

        percent <- round(annotated / nrow(items()) * 100, 1)
        paste0(percent, " %")
      })
    })

    observe({
      output$audioPlayer <- shiny::renderUI({
        req(items())
        req(itemRow())
        print(paste("rendering audio path", items()[itemRow(), "wav_path"]))
        if (
          is.na(items()[itemRow(), "wav_path"]) ||
            items()[itemRow(), "wav_path"] == "" ||
            !grepl("\\.wav$", items()[itemRow(), "wav_path"])
        ) {
          return(shiny::tags$p("Keine Audiodatei verfügbar"))
        } else {
          wav_path <- items()[itemRow(), "wav_path"]
          informant_with_zeros <- stringr::str_split_1(wav_path, "_")[1]

          informant_id <- sub(
            "^([A-Za-z]+)0+(\\d+)$",
            "\\1\\2",
            informant_with_zeros
          )

          wav_path_uncut <- sub("_\\d+\\.wav$", ".wav", wav_path)

          print(paste0(
            "https://dmw.zimt.uni-siegen.de/unencrypted_origional_uploads/speechrecorder/",
            informant_id,
            "/",
            wav_path_uncut
          ))
          shiny::div(
            class = "audio-player-container",
            howler::howler(
              elementId = shiny::NS(id, "sound"),
              tracks = list(
                "cut" = paste0(
                  "https://dmw.zimt.uni-siegen.de/Fertig/",
                  wav_path
                ),
                "uncut" = paste0(
                  "https://dmw.zimt.uni-siegen.de/unencrypted_origional_uploads/speechrecorder/",
                  informant_id,
                  "/",
                  wav_path_uncut
                )
              ),
              seek_ping_rate = 100
            ),
            shiny::div(
              class = "howler-controls",
              howler::howlerSeekSlider(shiny::NS(id, "sound")),
              howler::howlerPlayPauseButton(shiny::NS(id, "sound")),
              howler::howlerVolumeSlider(shiny::NS(id, "sound")),
              howler::howlerSeekTime(shiny::NS(id, "sound")),
              shiny::div(
                class = "playrate-toggle",
                shiny::div(
                  class = "playrate-display",
                  shiny::textOutput(shiny::NS(id, "playRateDisplay"))
                ),
                shiny::div(
                  class = "playrate-slider",
                  shiny::sliderInput(
                    shiny::NS(id, "playRate"),
                    label = "Speed",
                    min = 0.5,
                    max = 2.0,
                    value = 1,
                    step = 0.1,
                    ticks = FALSE,
                    width = "120px"
                  )
                )
              ),
              shinyWidgets::radioGroupButtons(
                shiny::NS(id, "inputAudioUncut"),
                label = "",
                choices = c("geschnitten", "ungeschnitten"),
                selected = "geschnitten",
              )
            )
          )
        }
      })
    }) |>
      bindEvent(c(items(), itemRow()))
    observe({
      req(input$inputAudioUncut)
      if (input$inputAudioUncut == "ungeschnitten") {
        howler::changeTrack(
          "sound",
          session,
          track = 2
        )
        print("change to uncut")
      }
      if (input$inputAudioUncut == "geschnitten") {
        howler::changeTrack(
          "sound",
          session,
          track = 1
        )
        print("change to cut")
      }
    }) |>
      bindEvent(input$inputAudioUncut)

    observe({
      output$playRateDisplay <- shiny::renderText({
        paste0(input$playRate, "x")
      })
    })
    observe({
      print("change playrate")
      howler::changeHowlSpeed(
        "sound",
        session,
        rate = input$playRate
      )
    }) |>
      bindEvent(input$playRate)
    shiny::observe({
      req(items())
      req(itemRow())

      current_category <- if (
        !is.null(categories()) &&
          length(categories()) > 0 &&
          categoryNumber() > 0
      ) {
        categories()[categoryNumber()]
      } else {
        NA_character_
      }

      DBI::dbExecute(
        conAnn,
        glue::glue_sql(
          "INSERT INTO annotations (informant_id, subtask_id, systemebene, category, annotation_text, username)
           VALUES (
                {items()[itemRow(), 'informant_id']},
                {items()[itemRow(), 'subtask_id']},
                {input$selectSystemebene},
                {current_category},
                {input$annValue},
                {username})
           ON DUPLICATE KEY UPDATE
                annotation_text = VALUES(annotation_text),
                username = VALUES(username),
                created_at = CURRENT_TIMESTAMP;",
          .con = conAnn
        )
      )
    }) |>
      bindEvent(
        c(
          input$nextItem,
          input$previousItem,
          input$nextCategory,
          input$previousCategory
        ),
        ignoreInit = TRUE
      )

    howler::howlerModuleServer("sound")
    tasks <- shiny::reactive({
      req(input$selectTask)
      input$selectTask
    })

    shiny::observe({
      ns <- session$ns
      showModal(
        modalDialog(
          title = "Variante hinzufügen",
          shiny::checkboxGroupInput(
            ns("selectTasksAddVariant"),
            "Teilaufgaben auswählen",
            choices = tasks(),
            selected = tasks(),
            inline = TRUE
          ),
          shiny::textInput(ns("variantName"), "Variante"),
          shiny::actionButton(ns("saveVariant"), "Variante speichern"),
          easyClose = TRUE,
          footer = NULL,
        )
      )
      shiny::updateCheckboxGroupInput(
        session,
        ns("selectTasksAddVariant"),
        choices = input$selectedTasks,
        selected = input$selectedTasks
      )
      phanomenID <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT Phänomen_ID FROM phaenomen
          WHERE Teilaufgaben_ID IN ({tasks()*}) ",
          .con = conAnn
        )
      ) |>
        dplyr::pull(1) |>
        unique()
    }) |>
      shiny::bindEvent(input$addVariant)

    shiny::observe({
      ns <- session$ns
      showModal(
        modalDialog(
          title = "Kommentar hinzufügen",
          shiny::textAreaInput(
            ns("commentText"),
            "Kommentar",
            width = "100%",
            height = "200px"
          ),
          shiny::actionButton(ns("saveComment"), "Kommentar speichern"),
          easyClose = TRUE,
          footer = NULL,
        )
      )
    }) |>
      shiny::bindEvent(input$addComment)

    observe({
      req(input$saveComment)
      DBI::dbExecute(
        conAnn,
        glue::glue_sql(
          "INSERT INTO comments (annotation_id, comment_text, username)
        VALUES (
                {items()[itemRow(), \"id\"]},
                {input$commentText},
                {username})
                ",
          .con = conAnn
        )
      )
      comments <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT comment_text FROM comments
        WHERE annotation_id = {items()[itemRow(), 'id']}
        ORDER BY id DESC",
          .con = conAnn
        )
      ) |>
        dplyr::pull(1)
      output$comment <- shiny::renderUI({
        shiny::tags$div(
          style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
          shiny::tags$p(glue::glue_collapse(comments, sep = "\n"))
        )
      })

      shiny::removeModal()
    }) |>
      bindEvent(input$saveComment)

    shiny::observe({
      ns <- session$ns
      showModal(
        modalDialog(
          title = "Kategorie hinzufügen",
          shiny::checkboxGroupInput(
            ns("selectTasksAddCategory"),
            "Teilaufgaben auswählen",
            choices = tasks(),
            selected = tasks(),
            inline = TRUE
          ),
          shiny::textInput(ns("categoryName"), "Kategorie"),
          shiny::actionButton(ns("saveCategory"), "Kategorie speichern"),
          easyClose = TRUE,
          footer = NULL
        )
      )
    }) |>
      shiny::bindEvent(input$addCategory)

    observe({
      req(input$saveCategory)
      req(input$categoryName)
      req(input$selectTasksAddCategory)

      if (nchar(trimws(input$categoryName)) == 0) {
        shiny::showNotification(
          "Bitte einen Kategorienamen eingeben.",
          type = "error"
        )
        return()
      }

      for (subtask in input$selectTasksAddCategory) {
        existing <- DBI::dbGetQuery(
          conAnn,
          glue::glue_sql(
            "SELECT id FROM categories 
             WHERE subtask_id = {subtask} AND category = {input$categoryName}",
            .con = conAnn
          )
        )

        if (nrow(existing) == 0) {
          DBI::dbExecute(
            conAnn,
            glue::glue_sql(
              "INSERT INTO categories (subtask_id, category) 
               VALUES ({subtask}, {input$categoryName});",
              .con = conAnn
            )
          )
        }
      }

      shiny::showNotification(
        paste("Kategorie", input$categoryName, "hinzugefügt."),
        type = "message"
      )

      cats <- DBI::dbGetQuery(
        conAnn,
        glue::glue_sql(
          "SELECT category FROM categories 
           WHERE subtask_id IN ({input$selectTask*}) ORDER BY id",
          .con = conAnn
        )
      ) |>
        dplyr::pull(1) |>
        unique()
      categories(cats)

      if (length(categories()) > 1) {
        updateActionButton(session, "nextCategory", disabled = FALSE)
        updateActionButton(session, "nextCategoryNoSave", disabled = FALSE)
      }

      shiny::removeModal()
    }) |>
      bindEvent(input$saveCategory)

    shiny::observe({
      ns <- session$ns
      showModal(
        modalDialog(
          title = "Informationen zum aktuellen Item",
          shiny::verbatimTextOutput(ns("itemInfo")),
          easyClose = TRUE,
          footer = NULL,
        )
      )
      output$itemInfo <- shiny::renderPrint({
        DBI::dbGetQuery(
          conDMW,
          glue::glue_sql(
            "SELECT informant_id,gender  FROM Informant WHERE informant_id = {items()[itemRow(), 'informant_id']}",
            .con = conDMW
          )
        )
      })
    }) |>
      shiny::bindEvent(input$showInfo)

    observe({
      req(input$skipItemButton)
      req(input$skipItem)
      req(items())

      target_id <- trimws(input$skipItem)

      if (nchar(target_id) == 0) {
        shiny::showNotification(
          "Bitte eine Item-ID eingeben.",
          type = "warning"
        )
        return()
      }

      matching_rows <- which(as.character(items()$id) == target_id)

      if (length(matching_rows) == 0) {
        target_numeric <- suppressWarnings(as.numeric(target_id))
        if (!is.na(target_numeric)) {
          matching_rows <- which(items()$id == target_numeric)
        }
      }

      if (length(matching_rows) > 0) {
        itemRow(matching_rows[1])
        shiny::showNotification(
          paste("Springe zu Item:", target_id),
          type = "message"
        )
        shiny::updateTextInput(session, "skipItem", value = "")
      } else {
        shiny::showNotification(
          paste("Item mit ID", target_id, "nicht gefunden."),
          type = "error"
        )
      }
    }) |>
      bindEvent(input$skipItemButton)

    output$exportAnnotations <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "annotationen_",
          input$selectSystemebene,
          "_",
          input$selectPhaenomen,
          "_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".xlsx"
        )
      },
      content = function(file) {
        shiny::withProgress(message = "Exportiere Annotationen...", {
          annotations_df <- DBI::dbGetQuery(
            conAnn,
            glue::glue_sql(
              "SELECT 
                a.informant_id,
                a.subtask_id,
                a.systemebene,
                a.category,
                a.annotation_text,
                a.username,
                a.created_at
              FROM annotations a
              WHERE a.subtask_id IN ({input$selectTask*})
              ORDER BY a.informant_id, a.subtask_id, a.category",
              .con = conAnn
            )
          )

          shiny::incProgress(0.2)

          comments_df <- DBI::dbGetQuery(
            conAnn,
            glue::glue_sql(
              "SELECT 
                c.annotation_id,
                c.comment_text,
                c.username,
                c.created_at
              FROM comments c
              INNER JOIN annotations a ON c.annotation_id = a.id
              WHERE a.subtask_id IN ({input$selectTask*})
              ORDER BY c.annotation_id",
              .con = conAnn
            )
          )

          shiny::incProgress(0.2)

          doubt_df <- DBI::dbGetQuery(
            conAnn,
            glue::glue_sql(
              "SELECT 
                informant_id,
                subtask_id,
                is_doubt,
                username,
                created_at
              FROM itemDoubt
              WHERE subtask_id IN ({input$selectTask*})
              AND is_doubt = TRUE
              ORDER BY informant_id",
              .con = conAnn
            )
          )

          shiny::incProgress(0.2)

          unusable_df <- DBI::dbGetQuery(
            conAnn,
            glue::glue_sql(
              "SELECT 
                informant_id,
                subtask_id,
                is_unusable,
                username,
                created_at
              FROM itemUnusable
              WHERE subtask_id IN ({input$selectTask*})
              AND is_unusable = TRUE
              ORDER BY informant_id",
              .con = conAnn
            )
          )

          shiny::incProgress(0.2)

          wb <- openxlsx::createWorkbook()

          openxlsx::addWorksheet(wb, "Annotationen")
          if (nrow(annotations_df) > 0) {
            openxlsx::writeData(wb, "Annotationen", annotations_df)
          } else {
            openxlsx::writeData(
              wb,
              "Annotationen",
              data.frame(Hinweis = "Keine Annotationen vorhanden")
            )
          }

          openxlsx::addWorksheet(wb, "Kommentare")
          if (nrow(comments_df) > 0) {
            openxlsx::writeData(wb, "Kommentare", comments_df)
          } else {
            openxlsx::writeData(
              wb,
              "Kommentare",
              data.frame(Hinweis = "Keine Kommentare vorhanden")
            )
          }

          openxlsx::addWorksheet(wb, "Zweifelsfaelle")
          if (nrow(doubt_df) > 0) {
            openxlsx::writeData(wb, "Zweifelsfaelle", doubt_df)
          } else {
            openxlsx::writeData(
              wb,
              "Zweifelsfaelle",
              data.frame(Hinweis = "Keine Zweifelsfälle markiert")
            )
          }

          openxlsx::addWorksheet(wb, "Nicht_auswertbar")
          if (nrow(unusable_df) > 0) {
            openxlsx::writeData(wb, "Nicht_auswertbar", unusable_df)
          } else {
            openxlsx::writeData(
              wb,
              "Nicht_auswertbar",
              data.frame(Hinweis = "Keine Items als nicht auswertbar markiert")
            )
          }

          shiny::incProgress(0.2)

          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        })
      }
    )

    output$exportRede <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "REDE_",
          input$selectPhaenomen,
          "_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".xlsx"
        )
      },
      content = function(file) {
        shiny::withProgress(message = "Erstelle REDE Export...", {
          req(input$selectTask)

          annotations_df <- DBI::dbGetQuery(
            conAnn,
            glue::glue_sql(
              "SELECT informant_id, annotation_text AS Variante
               FROM annotations
               WHERE subtask_id IN ({input$selectTask*})
               AND category = 'Phänomen'",
              .con = conAnn
            )
          )

          if (nrow(annotations_df) == 0) {
            shiny::showNotification(
              "Keine Annotationen für diese Aufgabe vorhanden.",
              type = "warning"
            )
            return(NULL)
          }

          shiny::incProgress(0.2)

          on.exit(DBI::dbDisconnect(conDMW), add = TRUE)

          informant_db <- DBI::dbReadTable(conDMW, "Informant")
          wenker_db <- DBI::dbReadTable(conDMW, "Wenker")
          profession_db <- DBI::dbReadTable(conDMW, "Profession")
          profession_class_db <- DBI::dbReadTable(
            conDMW,
            "ProfessionClassification"
          )

          shiny::incProgress(0.2)

          annotations_df <- annotations_df |>
            dplyr::mutate(informant_id = as.integer(informant_id))

          export_data <- annotations_df |>
            dplyr::left_join(
              informant_db,
              by = "informant_id"
            ) |>
            dplyr::left_join(
              wenker_db |> dplyr::select(id, place_name, gid),
              by = c("place_id" = "id")
            ) |>
            dplyr::left_join(
              profession_db |>
                dplyr::select(
                  informant_id,
                  learned_profession,
                  current_profession
                ) |>
                dplyr::distinct(informant_id, .keep_all = TRUE),
              by = "informant_id"
            ) |>
            dplyr::left_join(
              profession_class_db |>
                dplyr::select(informant_id, classification) |>
                dplyr::distinct(informant_id, .keep_all = TRUE),
              by = "informant_id"
            )

          shiny::incProgress(0.2)

          export_data <- export_data |>
            dplyr::mutate(
              Geburtsjahr = suppressWarnings(as.integer(birth_date)),
              age_2024 = 2024 - Geburtsjahr,
              ag_suffix = dplyr::if_else(age_2024 >= 70, "-1", "-2"),
              beruf_suffix = dplyr::case_when(
                classification == "handwerklich" ~ "-H",
                classification == "kommunikativ" ~ "-K",
                TRUE ~ "-"
              ),
              geschlecht_suffix = dplyr::if_else(
                gender == "Männlich",
                "-M",
                "-W"
              ),
              Profession = dplyr::case_when(
                !is.na(current_profession) &
                  current_profession != "" ~ current_profession,
                !is.na(learned_profession) &
                  learned_profession != "" ~ learned_profession,
                TRUE ~ NA_character_
              )
            )

          export_data <- export_data |>
            dplyr::mutate(
              Ortspunkt = place_name,
              GID = gid,
              AG = paste0(Variante, ag_suffix),
              Geschlecht_REDE = paste0(Variante, geschlecht_suffix),
              Beruf = paste0(Variante, beruf_suffix),
              Geschlecht = gender
            ) |>
            dplyr::select(
              Ortspunkt,
              GID,
              Variante,
              AG,
              Geschlecht_REDE,
              Beruf,
              Geburtsjahr,
              Geschlecht,
              Profession
            )

          shiny::incProgress(0.2)

          openxlsx::write.xlsx(export_data, file, overwrite = TRUE)

          shiny::incProgress(0.2)
        })

        shiny::showNotification(
          "REDE Export erstellt.",
          type = "message"
        )
      }
    )
  })
}
