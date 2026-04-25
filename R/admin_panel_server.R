#' Admin Panel Server Module
#'
#' Server-side logic for the admin panel. Manages user creation, deletion,
#' password generation, and email notification functionality.
#'
#' @param id Character string. The module ID, must match the UI module.
#' @param conAnn DBI connection object. Database connection for storing user data.
#' @param pass Character string. SMTP password from environment.
#'   Defaults to `Sys.getenv("STMP_PASS")`.
#'
#' @return Invisibly returns NULL (typical for Shiny server modules).
#'
#' @details
#' This module provides:
#' - User creation with role assignment
#' - Automatic password generation
#' - Email notification for new users
#' - User deletion with admin protection (prevents deleting last admin)
#' - User listing in interactive data table
#'
#' Database Requirements:
#' The annotation database should contain a `users` table with columns:
#' - user (character)
#' - email (character, nullable)
#' - password (character, hashed)
#' - role (character: 'user' or 'admin')
#'
#' @section Dependencies:
#' Requires packages: shiny, DBI, sodium, blastula, DT
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Shiny app
#' shiny::moduleServer("admin_panel", adminPanelServer,
#'   args = list(
#'     conAnn = con_annotation,
#'     pass = Sys.getenv("SMTP_PASS")
#'   )
#' )
#' }
adminPanelServer <- function(id, conAnn, pass = Sys.getenv("STMP_PASS")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    Sys.setenv("SMTP_PASS" = pass)

    observeEvent(input$open_admin_modal, {
      shiny::showModal(adminPanelModal(ns))
    })

    user_list_trigger <- reactiveVal(0)

    observeEvent(input$generate_password, {
      chars <- c(letters, LETTERS, 0:9, strsplit("!@#$%&*", "")[[1]])
      pw <- paste0(sample(chars, 12, replace = TRUE), collapse = "")
      shiny::updateTextInput(session, "new_password", value = pw)
    })

    users_df <- reactive({
      user_list_trigger()
      userdata <- DBI::dbGetQuery(
        conAnn,
        "SELECT user, email, role FROM users ORDER BY user"
      )
      updateSelectInput(
        session,
        "delete_user",
        choices = userdata$user
      )
      userdata
    })

    output$user_table <- DT::renderDataTable({
      DT::datatable(
        users_df(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "tip"
        ),
        rownames = FALSE,
        selection = "none"
      )
    })

    observe({
      shiny::updateSelectInput(
        session,
        "delete_user",
        choices = users_df()$user
      )
    })

    observeEvent(input$add_user, {
      req(input$new_username, input$new_password)

      existing <- DBI::dbGetQuery(
        conAnn,
        "SELECT user FROM users WHERE user = ?",
        params = list(input$new_username)
      )

      if (nrow(existing) > 0) {
        shiny::showNotification(
          "Benutzername existiert bereits!",
          type = "error"
        )
        return()
      }

      user_email <- if (nchar(trimws(input$new_email)) > 0) {
        input$new_email
      } else {
        NA_character_
      }

      hashed_pw <- sodium::password_store(input$new_password)
      DBI::dbExecute(
        conAnn,
        "INSERT INTO users (user, email, password, role) VALUES (?, ?, ?, ?);",
        params = list(input$new_username, user_email, hashed_pw, input$new_role)
      )

      if (isTRUE(input$send_email) && !is.na(user_email)) {
        tryCatch(
          {
            email_body <- paste0(
              "Hallo ",
              input$new_username,
              ",<br><br>",
              "Ihr Benutzerkonto wurde erstellt. Hier sind Ihre Zugangsdaten:<br><br>",
              "<b>Benutzername:</b> ",
              input$new_username,
              "<br>",
              "<b>Passwort:</b> ",
              input$new_password,
              "<br><br>",
              "Link zum Tool: <a href='https://dmwtest.timoschuermann.org/app/01_DMW/'>DMW Annotationstool</a><br><br>",
              "Bei Fragen wenden Sie sich bitte an info@timoschuermann.org<br><br>",
              "Mit freundlichen Grüßen,<br>",
              "Dialektatlas Mittleres Westdeutschland"
            )

            email_msg <- blastula::compose_email(
              body = blastula::md(email_body)
            )
            email_msg |>
              blastula::smtp_send(
                from = "info@timoschuermann.org",
                to = user_email,
                subject = "Ihre Zugangsdaten für das DMW Annotationstool",
                credentials = blastula::creds_envvar(
                  host = "timoschuermann.org",
                  port = 465,
                  user = "info@timoschuermann.org",
                  pass_envvar = "SMTP_PASS",
                  use_ssl = FALSE
                )
              )

            shiny::showNotification(
              paste("Zugangsdaten per E-Mail an", user_email, "gesendet."),
              type = "message"
            )
          },
          error = function(e) {
            shiny::showNotification(
              paste("E-Mail konnte nicht gesendet werden:", e$message),
              type = "error"
            )
          }
        )
      }

      shiny::showNotification(
        paste("Benutzer", input$new_username, "hinzugefügt."),
        type = "message"
      )

      shiny::updateTextInput(session, "new_username", value = "")
      shiny::updateTextInput(session, "new_email", value = "")
      shiny::updateTextInput(session, "new_password", value = "")
      user_list_trigger(user_list_trigger() + 1)
    })

    observeEvent(input$delete_user_btn, {
      req(input$delete_user)

      admin_count <- DBI::dbGetQuery(
        conAnn,
        "SELECT COUNT(*) as n FROM users WHERE role = 'admin'"
      )$n

      user_role <- DBI::dbGetQuery(
        conAnn,
        "SELECT role FROM users WHERE user = ?",
        params = list(input$delete_user)
      )$role

      if (admin_count <= 1 && user_role == "admin") {
        shiny::showNotification(
          "Letzter Admin kann nicht gelöscht werden!",
          type = "error"
        )
        return()
      }

      DBI::dbExecute(
        conAnn,
        "DELETE FROM users WHERE user = ?;",
        params = list(input$delete_user)
      )

      shiny::showNotification(
        paste("Benutzer", input$delete_user, "gelöscht."),
        type = "message"
      )

      user_list_trigger(user_list_trigger() + 1)
    })
  })
}
