# test app ====

#' Example App to Illustrate Async Patterns
#'
#' @export
hello <- function() {
  future::plan(future::multicore, workers = n_of_ex)
  options(shiny.reactlog = TRUE)
  shiny::shinyApp(
    hello_ui(),
    hello_server,
    options = list(
      shiny.host = options::opt("shiny_hostname"),
      port = options::opt("shiny_port")
    )
  )
}

hello_pv <- function() {
  rlang::check_installed("profvis")
  profvis::profvis(shiny::runApp(hello()))
}

#' @describeIn hello UI
#' @export
hello_ui <- function() {
  rlang::check_installed("bslib")
  rlang::check_installed("reactlog")
  bslib::page_sidebar(
    title = NULL,
    sidebar = bslib::sidebar(
      bslib::card(
        popover_hover(
          trigger = bslib::card_header("Setup"),
          "Only applies to the long-running task."
        ),
        bslib::card_body(
          shiny::radioButtons(
            inputId = "order",
            label = "Execution Order",
            selected = "async",
            choiceValues = c("sync", "async"),
            choiceNames = list(
              popover_hover(
                trigger = "Synchronous",
                "Shiny default behavior."
              ),
              popover_hover_md(
                trigger = "Asynchronous",
                "Reactives return *immediately* as promises.",
                "Work in the same or other sessions can run in parallel.",
                "But outputs are only refreshed when all promises are resolved."
              )
            )
          )
        )
      ),
      bslib::card(
        bslib::card_header("Long-Running Task"),
        bslib::card_body(
          shiny::radioButtons(
            inputId = "fun",
            label = "Function",
            selected = "slow_fun",
            choiceValues = c(
              "slow_fun"
            ),
            choiceNames = list(
              popover_hover_md(
                # writing markdown here via markdown() is not possible,
                # because it wraps in a p, not a span
                trigger = shiny::HTML("<code>slow_fun()</code>"),
                "Placeholder function for a long-running task.",
                "Just pauses for a few seconds."
              )
            )
          )
        ),
        bslib::card_body(
          bslib::input_task_button("run", label = "Run"),
          bslib::value_box(
            title = "Run Counter",
            value = shiny::textOutput("counter")
          )
        )
      ),
      other_task_ui("histo")[["input"]]
    ),
    bslib::accordion(
      bslib::accordion_panel(
        title = "Results",
        bslib::card(
          bslib::card_header("Long-Running Task"),
          bslib::card_body(
            bslib::layout_column_wrap(
              width = 1 / n_of_ex,
              fill = FALSE,
              !!!ex_cards_ui("done")
            )
          )
        ),
        other_task_ui("histo")[["output"]]
      ),
      bslib::accordion_panel(
        title = "Diagnostics",
        bslib::navset_underline(
          bslib::nav_panel(
            title = "Reactive Graph",
            bslib::card(full_screen =  TRUE, reactlog::reactlog_module_ui())
          )
        )
      )
    )
  )
}

#' @describeIn hello Server
#' @param input,output,session See [shiny::shinyApp()].
#' @export
hello_server <- function(input, output, session) {
  counter <- shiny::reactive(input$run)
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
  other_task_server("histo")
  reactlog::reactlog_module_server()
  output$counter <- shiny::renderText(counter())
}

n_of_ex <- 3

# slow funs ===

#' A slow function
#' @export
slow_fun <- function() {
  rlang::check_installed("profvis")
  profvis::pause(5)
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

# other task ====

#' Example of Another Task
#'
#' Based on shiny example.
#' @name other_task
NULL

#' @describeIn other_task Module UI
#' @inheritParams shiny::NS
#' @export
other_task_ui <- function(id) {
  ns <- shiny::NS(id)
  list(
    input = bslib::card(
      popover_hover(
        trigger = bslib::card_header("Other Task"),
        "Serves only to illustrate continued activity of other shiny elements."
      ),
      bslib::card_body(
        shiny::sliderInput(
          inputId = ns("bins"),
          label = "Number of bins:",
          min = 1,
          max = 50,
          value = 30
        )
      )
    ),
    output = bslib::card(
      bslib::card_header("Other Task"),
      bslib::card_body(shiny::plotOutput(outputId = ns("dist_plot")))
    )
  )
}

#' @describeIn other_task Module Server
#' @export
other_task_server <-  function(id) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      output$dist_plot <- shiny::renderPlot({
        x <- datasets::faithful$waiting
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        graphics::hist(
          x,
          breaks = bins,
          col = "#007bc2",
          border = "white",
          xlab = "Waiting time to next eruption (in mins)",
          main = "Histogram of waiting times"
        )
      })
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
        paste("This is run count", counter())
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
