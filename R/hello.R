# test app ====

n_of_ex <- 3

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
      setup_async_ui("setup"),
      long_task_ui("long")[["sidebar"]],
      other_task_ui("other")[["sidebar"]]
    ),
    bslib::accordion(
      bslib::accordion_panel(
        title = "Results",
        long_task_ui("long")[["main"]],
        other_task_ui("other")[["main"]]
      ),
      bslib::accordion_panel(
        title = "Diagnostics",
        diagnostics_ui("diagnostics")
      )
    )
  )
}

#' @describeIn hello Server
#' @param input,output,session See [shiny::shinyApp()].
#' @export
hello_server <- function(input, output, session) {
  fun <- setup_async_server("setup")
  long_task_server("long", fun = fun)
  other_task_server("other")
  diagnostics_server("diagnostics")
}

# slow funs ====

#' A slow function
#' @param seed
#' Scalar integer to be used by the slow function.
#' This is a placeholder variable used for invalidation.
#' @export
slow_fun <- function(seed = 1) {
  rlang::check_installed("profvis")
  withr::local_seed(seed = seed)
  sleep_time <- runif(1, 2, 5)
  profvis::pause(sleep_time)
  paste("Done based on seed", seed)
}

# setup ====

#' Setup of Async etc. for the long-running task
#' @name setup_async
NULL

#' @describeIn setup_async Module UI
#' @inheritParams shiny::NS
#' @export
setup_async_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Setup for Long-Running Task"),
    bslib::card_body(
      shiny::radioButtons(
        inputId = ns("order"),
        label = "Execution Order",
        selected = "sync",
        choiceValues = c("sync", "async", "background"),
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
          ),
          popover_hover_md(
            trigger = "Asynchronous & Background",
            "In addition to the advantages of asynchronous,",
            "`ExtendedTask` unblocks the *current* session.",
            "Results are returned *as soon as they are ready*,",
            "and other interactions can be done in the meantime."
          )
        )
      )
    )
  )
}

#' @describeIn setup_async Module Server
#' @return The modified long-running function.
#' @export
setup_async_server <- function(id) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      shiny::reactive({
        switch(input$order,
          sync = slow_fun,
          async = asyncify(slow_fun),
          background = asyncify(slow_fun)
        )
      })
    }
  )
}

# long task ====

#' Example of a Long Task
#' @name long_task
NULL

#' @describeIn long_task Module UI
#' @inheritParams shiny::NS
#' @export
long_task_ui <- function(id) {
  ns <- shiny::NS(id)
  list(
    sidebar = bslib::card(
      bslib::card_header("Long-Running Task"),
      bslib::card_body(
        shiny::radioButtons(
          inputId = ns("fun"),
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
        ),
        popover_hover_md(
          trigger = shiny::numericInput(
            inputId = ns("seed"),
            label = "Seed",
            value = 1
          ),
          "This input is a placeholder input.",
          "The long-running task only uses this in its return,",
          "thereby creating a reactive depencency on it."
        ),
        bslib::input_task_button(ns("run"), label = "Run")
      )
    ),
    main = bslib::card(
      bslib::card_header("Long-Running Task"),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1 / n_of_ex,
          fill = FALSE,
          !!!ex_cards_ui(ns("done"))
        )
      )
    )
  )
}

#' @describeIn long_task Module Server
#' @inheritParams ex_cards_server
#' @export
long_task_server <- function(id, fun) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      counter <- shiny::reactive(input$run)
      seed <- shiny::reactive(input$seed)
      ex_cards_server("done", counter = counter, fun = fun, seed = seed)
    }
  )
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
#' @param fun A (slow) function to calculate each of the results.
#' @inheritParams slow_fun
#' @export
ex_cards_server <- function(id, counter, fun, seed) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      purrr::map(
        letters[1:n_of_ex],
        function(x) {
          ex_card_server(id = x, counter = counter, fun = fun, seed = seed)
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
ex_card_server <- function(id, counter, fun, seed) {
  abort_if_not_reactive(counter)
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ex_card_body_server("body", counter = counter, fun = fun, seed = seed)
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
ex_card_body_server <- function(id, counter, fun, seed) {
  abort_if_not_reactive(counter)
  abort_if_not_reactive(fun)
  abort_if_not_reactive(seed)
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      res <- shiny::reactive(fun()(shiny::isolate(seed()))) |>
        shiny::bindCache(seed()) |>
        shiny::bindEvent(counter())
      output$res <- shiny::renderText(res())
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
    sidebar = bslib::card(
      bslib::card_header("Other Task"),
      bslib::card_body(
        shiny::sliderInput(
          inputId = ns("decile"),
          label = "Decile",
          min = 1,
          max = 10,
          value = 3
        )
      )
    ),
    main = bslib::card(
      bslib::card_header("Other Task"),
      bslib::card_body(
        bslib::value_box(
          title = "Next Volcanic Eruption in",
          value = shiny::textOutput(ns("time_2_erupt"))
        )
      )
    )
  )
}

#' @describeIn other_task Module Server
#' @export
other_task_server <- function(id) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      output$time_2_erupt <- shiny::renderText({
        x <- datasets::faithful$waiting
        paste(
          stats::quantile(x, probs = seq(0, 1, by = .1))[input$decile],
          "Years"
        )
      })
    }
  )
}

# diagnostics ====

#' Show Diagnostics in Shiny App
#' @name diagnostics
NULL

#' @describeIn diagnostics Module UI
#' @inheritParams shiny::NS
#' @export
diagnostics_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_underline(
    bslib::nav_panel(
      title = "Reactive Graph",
      bslib::card(
        full_screen = TRUE,
        reactlog::reactlog_module_ui(id = ns("reactlog"))
      )
    )
  )
}

#' @describeIn diagnostics Module Server
#' @export
diagnostics_server <- function(id) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      reactlog::reactlog_module_server(id = "reactlog")
    }
  )
}
