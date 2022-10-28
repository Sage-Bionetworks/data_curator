# setTabTitleUI <- function(id) {
#   ns <- NS(id)
#   uiOutput(ns("dashboard-tab-title"))
# }

# setTabTitle <- function(id, title) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       output$`dashboard-tab-title` <- renderUI({
#         p(title, class = "tab-title")
#       })
#     }
#   )
# }
