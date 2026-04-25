#' Admin Panel Modal Dialog
#'
#' Creates a modal dialog for user management with add and delete functionality.
#'
#' @param ns Shiny namespace function. Used for creating properly scoped input IDs.
#'
#' @return A Shiny modal dialog UI element.
#'
#' @details
#' The modal contains:
#' - Left column: Form to add new users (username, email, password, role)
#' - Right column: Table of existing users and delete functionality
#'
#' Includes options for:
#' - Automatic password generation
#' - Sending credentials via email
#' - Role assignment (user or admin)
#'
#' @keywords internal
adminPanelModal <- function(ns) {
  shiny::modalDialog(
    title = "Benutzerverwaltung",
    size = "xl",
    easyClose = TRUE,
    footer = shiny::modalButton("Schließen"),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::h5("Benutzer hinzufügen"),
        shiny::textInput(ns("new_username"), "Benutzername"),
        shiny::textInput(ns("new_email"), "E-Mail"),
        shiny::textInput(ns("new_password"), "Passwort"),
        shiny::actionButton(
          ns("generate_password"),
          "Passwort generieren",
          icon = shiny::icon("key"),
          class = "btn-outline-secondary btn-sm mb-2"
        ),
        shiny::selectInput(
          ns("new_role"),
          "Rolle",
          choices = c("user", "admin"),
          selected = "user"
        ),
        shiny::checkboxInput(
          ns("send_email"),
          "Zugangsdaten per E-Mail senden",
          value = TRUE
        ),
        shiny::actionButton(
          ns("add_user"),
          "Benutzer hinzufügen",
          class = "btn-primary"
        )
      ),
      shiny::column(
        width = 8,
        shiny::h5("Bestehende Benutzer"),
        DT::dataTableOutput(ns("user_table")),
        shiny::hr(),
        shiny::selectInput(
          ns("delete_user"),
          "Benutzer löschen",
          choices = NULL
        ),
        shiny::actionButton(
          ns("delete_user_btn"),
          "Benutzer löschen",
          class = "btn-danger"
        )
      )
    )
  )
}
