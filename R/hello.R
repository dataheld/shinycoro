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
    bslib::card(
      bslib::card_header("Output A"),
      bslib::card_body(shiny::textOutput("A"))
    ),
    bslib::card(
      bslib::card_header("Output B"),
      bslib::card_body(shiny::textOutput("B"))
    ),
    title = NULL,
    sidebar = bslib::sidebar(
      shiny::actionButton("reload", label = "Reload App"),
      bslib::value_box(
        title = "Run Counter",
        value = shiny::textOutput("reload")
      )
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
  output$A <- shiny::renderText({
    slow_fun(input$reload)
  })
  output$B <- shiny::renderText({
    slow_fun(input$reload)
  })
}

#' A slow function
#' @param x Integer, giving the current run count.
#' @export
slow_fun <- function(x = 0) {
  Sys.sleep(3)
  paste("Done with run", x)
}
