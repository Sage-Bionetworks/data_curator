dbStatsBoxUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("dbStatsBox-box"))
  )
}

dbStatsBox <- function(id, metadata, nodes, source.tab = "tabs", target.tab = "db-tab3", parent.session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # collect all required data type including selected data type
      requirements <- union(nodes$from, nodes$to)
      requirements <- requirements[!grepl("f:", requirements)] # exclude datasets
      # get all missing required data types
      not_uploaded <- setdiff(requirements, metadata$Component)
      # get number of manifests are required but not yet uploaded
      n_not_uploaded <- length(not_uploaded)
      # get number of manifests uploaded to synapse
      n_uploaded <- length(intersect(requirements, metadata$Component))
      # get number of manifests that are out of date
      n_outdated <- sum(metadata$ErrorType %in% c("Wrong Schema", "Out of Date"))


      # render summary stats boxes
      output$`dbStatsBox-box` <- renderUI({
        div(
          class = "dbStatsBox-box",
          div(
            class = "dbStatsBox-item-container",
            div(
              class = "dbStatsBox-item completed",
              tagList(
                icon("smile-wink", "fa-3x"),
                div(
                  class = "dbStatsBox-text",
                  h4("Completed"), span(n_uploaded)
                )
              )
            ),
            div(
              class = "dbStatsBox-item missing",
              tagList(
                icon("frown", "fa-3x"),
                div(
                  class = "dbStatsBox-text",
                  h4("Missing"), span(n_not_uploaded)
                )
              )
            ),
            div(
              class = "dbStatsBox-item outdated",
              tagList(
                icon("surprise", "fa-3x"),
                div(
                  class = "dbStatsBox-text",
                  h4("Out of Date"), span(n_outdated)
                )
              )
            )
          ),
          div(
            class = "dbStatsBox-btn-container",
            actionButton(ns("view-btn"), "View More", class = "btn-primary-color"),
            span("Click to see the Out-of-Date manifests")
          )
        )
      })

      # redirect to validation tab of dashboard once the btn clicked
      observeEvent(input$`view-btn`, {
        req(input$`view-btn` != 0)
        updateTabsetPanel(parent.session, source.tab, selected = target.tab)
      })
    }
  )
}