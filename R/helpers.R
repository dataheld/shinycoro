options::define_options(
  "Shiny hostname for test app",
  shiny_hostname = "localhost"
)
options::define_options(
  "Shiny port for test app",
  shiny_port = 7798
)

#' Helper to stop if input is not a reactive
#' @noRd
abort_if_not_reactive <- function(x) {
  if (!shiny::is.reactive(x)) {
    rlang::abort("Input must be reactive.")
  }
}

#' Helper to default to hover tooltip
#' @noRd
popover_hover <- function(...) {
  bslib::popover(
    ...,
    options = list(trigger = "hover")
  )
}
popover_hover_md <- function(trigger, ...) {
  popover_hover(trigger = trigger, shiny::markdown(mds = c(...)))
}
