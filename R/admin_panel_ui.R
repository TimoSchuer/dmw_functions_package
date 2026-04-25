#' Admin Panel UI Module
#'
#' Creates a button that opens the admin panel modal for user management.
#'
#' @param id Character string. The module ID for namespacing Shiny inputs/outputs.
#'
#' @return A Shiny UI element (shiny.tag.list) with an admin panel button.
#'
#' @details
#' This UI function creates a button that triggers the admin panel modal.
#' The button includes a gear icon and is styled with the secondary button class.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Shiny app
#' ui <- adminPanelUI("admin_panel")
#' }
adminPanelUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(
      ns("open_admin_modal"),
      "Admin Panel",
      icon = shiny::icon("gear"),
      class = "btn-secondary"
    )
  )
}
