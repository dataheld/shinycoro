backgroundify <- function(.f) {
  attr(.f, "background") <- TRUE
  .f
}

is_background_function <- function(x) {
  isTRUE(attr(x, "background"))
}
