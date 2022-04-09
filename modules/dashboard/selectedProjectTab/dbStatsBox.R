dbStatsBoxUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("dbStatsBox-box"))
  )
}

dbStatsBox <- function(id, up.data, req.data, source.tab, target.tab, parent) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # number of total dataset
      uniq_ds <- req.data %>% distinct(folderSynId, .keep_all = TRUE)
      n_ds <- nrow(uniq_ds)
      # number of completed dataset
      n_completed <- sum(uniq_ds$nMiss == 0)
      # number of incompleted dataset
      n_miss <- sum(uniq_ds$nMiss > 0)
      #
      progress_value <- round(n_completed / n_ds * 100)

      # collect all required datatype including selected datatype
      all_req <- union(req.data$from, req.data$to)
      all_req <- all_req[!grepl("f:", all_req)]
      # get all missing required datatypes
      not_up <- setdiff(all_req, up.data$schema)
      # get number of manifests are required but not yet uploaded
      n_not_up <- length(not_up)
      # get number of manifests uploaded to synapse
      n_up <- length(intersect(all_req, up.data$schema))
      # get number of manifests that are out of date
      n_outdated <- sum(up.data$errorType %in% c("Wrong Schema", "Out of Date"))


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
                  h4("Completed"), span(n_up)
                )
              )
            ),
            div(
              class = "dbStatsBox-item missing",
              tagList(
                icon("frown", "fa-3x"),
                div(
                  class = "dbStatsBox-text",
                  h4("Missing"), span(n_not_up)
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
        updateTabsetPanel(parent, source.tab, selected = target.tab)
      })
    }
  )
}