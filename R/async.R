asyncify <- function(.f) {
  function() {
    promises::then(
      promises::future_promise(.f()),
      onFulfilled = function(value) value
    )
  }
}
