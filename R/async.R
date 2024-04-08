asyncify <- function(.f) {
  function(...) {
    promises::then(
      promises::future_promise(.f(...), seed = NULL),
      onFulfilled = function(value) value
    )
  }
}
