asyncify <- function(.f) {
  function(...) promises::future_promise(.f(...), seed = NULL)
}
