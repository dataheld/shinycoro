#' Access package files
#' @noRd
system_file2 <- function(...) {
  system.file(..., package = "shinycoro")
}

#' Embed the shiny app in the pkgdown site
#'
#' Embeds the shiny app in the pkgdown website.
#' Does not offer a print method for non-html output.
#'
#' @details
#' This is copied from [shinycaas](https://github.com/maxheld83/shinycaas/).
#' Copying is better here rather then importing shinycaas
#' to avoid taking on a non-cran dependency.
#'
#' @return A tag list as from [htmltools::tagList()].
#'
#' @noRd
#' @keywords internal
include_app2 <- function() {
  htmltools::tags$div(
    htmltools::tags$iframe(
      src = build_url_shiny(),
      height = "100%",
      width = "100%",
      name = "app"
    ),
    class = "shiny-embed-pkgdown",
    htmltools::htmlDependency(
      name = "shinycoro",
      version = utils::packageVersion("shinycoro"),
      src = system_file2("css"),
      stylesheet = "embed_pkgdown.css"
    )
  )
}

build_url_shiny <- function() {
  httr2::url_build(
    list(
      scheme = "http",
      hostname = options::opt("shiny_hostname"),
      port = options::opt("shiny_port")
    )
  )
}

options::define_options(
  "Shiny hostname for test app",
  shiny_hostname = "localhost"
)
options::define_options(
  "Shiny port for test app",
  shiny_port = 7798
)

#' Error out on unavailable optional pkgs
#' @inheritParams requireNamespace
#' @noRd
require_namespace2 <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    rlang::abort(
      paste(
        x,
        "needed for this function to work.",
        "Please install it."
      )
    )
  }
}

#' Helper2 to stop if input is or is not a reactive
#' @noRd
abort_if_reactive <- function(x) {
  if (shiny::is.reactive(x)) {
    rlang::abort("Input must not be reactive.")
  }
}

abort_if_not_reactive <- function(x) {
  if (!shiny::is.reactive(x)) {
    rlang::abort("Input must be reactive.")
  }
}
