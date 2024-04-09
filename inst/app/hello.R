future::plan(
  future::multicore,
  workers = length(shinycoro:::examples)
)
shiny::shinyApp(
  ui = shinycoro::hello_ui(),
  server = shinycoro::hello_server
)
