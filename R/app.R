newForecastingDashboardApp <- function(...) {
  shinyApp(ui = ui, server = server, options = list(includeCSS("./R/styles.css")), ...)
}
