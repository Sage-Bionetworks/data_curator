# dbInfoBoxUI <- function(id) {
  
#   ns <- NS(id)
#   div(class="dbInfoBox-container",
#     HTML(paste0(
#       '
#       <div class="header-container">
#         <span class="title">TEST</span>
#         <span class="subtitle">per dataset</span>
#       </div>
#       <div class="content-container">
#         <div class="content-top">
#           <div class="description-container">',
#             infoBoxOutput(ns("info1")),
#             infoBoxOutput(ns("info2")),
#             infoBoxOutput(ns("info3")), '
#           </div>
#           <div class="plot-container">',
#             uiOutput(ns("plotly")), '
#           </div>
#         </div>
#         <div class="content-bottom">
#           <div class="progress-container">
#           </div>
#         </div>
#       </div>
#       '
#     ))
#   )
# }

# dbInfoBox <- function(id, uploadData) {
#   moduleServer(
#     id,
#     function(input, output, session) {

#       ns <- session$ns

#        output$info1 <- renderInfoBox({
#     infoBox(
#       "Progress", paste0(30, "%"), icon = icon("list"),
#       color = "purple"
#     )     })

#      output$info2 <- renderInfoBox({
#     infoBox(
#       "Progress", paste0(35, "%"), icon = icon("list"),
#       color = "purple"
#     )
#      })
#      output$info3 <- renderInfoBox({
#     infoBox(
#       "Progress", paste0(40, "%"), icon = icon("list"),
#       color = "purple"
#     )     })

#       # add one more uiOutput layer, since plotly not working in HTML()
#       output$plotly  <- renderUI({
#         plotlyOutput(ns("frequency"))
#       })
#       output$frequency <- renderPlotly({

#         p <- uploadData %>% 
#           add_count(createdOn) %>%
#           group_by(createdOn, n) %>%
#           summarise(schema = str_c(schema, collapse = "<br>")) %>%
#           ggplot(aes(factor(createdOn), n, text = paste0("createOn: ", createdOn, "<br>manifestCreated ", n))) + 
#           geom_bar(stat="identity", width = 0.5, fill = "#605ca8") +
#           theme_void() +
#           theme(
#             axis.line = element_blank(),
#             panel.grid.major = element_blank()
#           )

#         # Turn it interactive with ggplotly
#         ggplotly(p, tooltip="text", width = 200, height = 300)
#       })
#     }
#   )
# }