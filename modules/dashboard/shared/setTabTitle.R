setTabTitleUI <- function(id) {

  ns <- NS(id)
  uiOutput(ns("dashboard-tab-title"))
}

setTabTitle <- function(id, title) {
  moduleServer(
    id,
    function(input, output, session) {

      dashboardTabTitleStyle <- paste0(
      '
      text-align: center;
      font-weight: bold;
      font-size: 1.4em;
      margin: 15px 0;
      '
      )

      output$`dashboard-tab-title` <- renderUI(
        p(title, style = dashboardTabTitleStyle)
      )
    }
  )
}
