#' Example App to Illustrate Async Patterns
#'
#' @export
hello <- function() {
  shiny::shinyApp(
    hello_ui(),
    hello_server,
    options = list(
      shiny.host = options::opt("shiny_hostname"),
      port = options::opt("shiny_port")
    )
  )
}

#' @describeIn hello UI
#' @export
hello_ui <- function() {
  require_namespace2("bslib")
  bslib::page_sidebar(
    shiny::textOutput("reload"),
    title = NULL,
    sidebar = bslib::sidebar(
      shiny::actionButton("reload", label = "Reload App")
    )
  )
}

#' @describeIn hello Server
#' @param input,output,session See [shiny::shinyApp()].
#' @export
hello_server <- function(input, output, session) {
  output$reload <- shiny::renderText({
    input$reload
  })
}
