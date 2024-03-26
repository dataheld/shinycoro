# test app ====

#' Example App to Illustrate Async Patterns
#'
#' @export
hello <- function() {
  future::plan(future::multicore, workers = n_of_ex)
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
    title = NULL,
    sidebar = bslib::sidebar(
      shiny::radioButtons(
        inputId = "order",
        label = "Execution Order",
        choices = c(
          `Synchronous (shiny default)` = "sync",
          `Asynchronous (using promises)` = "async"
        ),
        selected = "async"
      ),
      shiny::actionButton("reload", label = "Invalidate"),
      bslib::value_box(
        title = "Run Counter",
        value = shiny::textOutput("counter")
      )
    ),
    ex_cards_ui("done")
  )
}

#' @describeIn hello Server
#' @param input,output,session See [shiny::shinyApp()].
#' @export
hello_server <- function(input, output, session) {
  counter <- shiny::reactive(input$reload)
  res_fun <- shiny::reactive({
    switch(
      input$order,
      sync = slow_fun,
      async = promisefy(slow_fun)
    )
  })
  ex_cards_server(
    id = "done",
    counter = counter,
    res_fun = res_fun
  )
  output$counter <- shiny::renderText(counter())
}

n_of_ex <- 4

# slow funs ===

#' A slow function
#' @export
slow_fun <- function() {
  Sys.sleep(2)
  "Done"
}

# several examples ====

#' Example Cards
#' @name ex_cards
NULL

#' @describeIn ex_cards Module UI
#' @inheritParams shiny::NS
#' @export
ex_cards_ui <- function(id) {
  ns <- shiny::NS(id)
  purrr::map(
    letters[1:n_of_ex],
    function(x) {
      ex_card_ui(ns(x))
    }
  )
}

#' @describeIn ex_cards Module Server
#' @param counter A reactive giving the current counter run (integer).
#' @param res_fun A (slow) function to calculate each of the results.
#' @export
ex_cards_server <- function(id, counter, res_fun) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      purrr::map(
        letters[1:n_of_ex],
        function(x) {
          ex_card_server(id = x, counter = counter, res_fun = res_fun)
        }
      )
    }
  )
}

# one example result ====

#' Example Card
#' @name ex_card
NULL

#' @describeIn ex_card Module UI
#' @inheritParams shiny::NS
#' @export
ex_card_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(paste("Output", id)),
    ex_card_body_ui(ns("body")),
    bslib::card_footer(shiny::textOutput(ns("counter_this")))
  )
}

#' @describeIn ex_card Module Server
#' @inheritParams ex_cards_server
#' @export
ex_card_server <- function(id, counter, res_fun) {
  abort_if_not_reactive(counter)
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ex_card_body_server(id = "body", counter = counter, res_fun = res_fun)
      output$counter_this <- shiny::renderText({
        paste("This is invalidation count", counter())
      })
    }
  )
}

# one example body ====

#' Example Card Body
#' @name ex_card_body
NULL

#' @describeIn ex_card_body Module UI
#' @inheritParams shiny::NS
#' @export
ex_card_body_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card_body(shiny::textOutput(ns("res")))
}

#' @describeIn ex_card_body Module Server
#' @inheritParams ex_cards_server
#' @export
ex_card_body_server <- function(id, counter, res_fun) {
  abort_if_not_reactive(counter)
  abort_if_not_reactive(res_fun)
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      res_fun <- res_fun |> shiny::bindEvent(counter())
      output$res <- shiny::renderText(res_fun()())
    }
  )
}
