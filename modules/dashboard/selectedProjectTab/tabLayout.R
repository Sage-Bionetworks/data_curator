selectedProjectTabUI <- function(id) {
  ns <- NS(id)
  div(
    class = "selectedProjectTab-container",
    tagList(
      setTabTitleUI(ns("title")),
      fluidRow(
        column(12,
          class = "banner-container",
          column(6, dbRaterUI(ns("summary"))),
          column(6, dbStatsBoxUI(ns("stats")))
        ),
        column(
          3,
          fluidRow(
            column(12, class = "section-title", span("Each dataset progress")),
            column(12, align = "center", uiOutput(ns("complete-dataset-pb"))),
            column(12, align = "center", uiOutput(ns("incomplete-dataset-pb"))),
            column(12, class = "section-title", span("Evaluate Submission"))
            # column(12, uiOutput(ns("evaluate-submit"))),
            # column(12, uiOutput(ns("evaluate-res")))
          )
        ),
        column(
          width = 9, class = "tree-box",
          fluidRow(
            column(12, class = "section-title", span("Requirment Relationship Tree")),
            column(12, align = "center", dbTreeUI(ns("requirement-tree")))
          )
        )
      )
    )
  )
}

selectedProjectTab <- function(id, userName, uploadData, reqData, selectedProject, tabId, validationTab, parent) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      # new_reqData <- eventReactive(input$`checkbox-evaluate`, {
      #   # add selected datatypes and update data
      #   evaluate_datatypes <- input$`checkbox-evaluate`
      #   evaluate_ds <- reqData$folderSynId[reqData$to %in% evaluate_datatypes]
      #   inx <- which(reqData$folderSynId %in% evaluate_ds)
      #   reqData$nMiss[inx] <- reqData$nMiss[inx] - length(evaluate_datatypes)
      # })

      # new_uploadData <- eventReactive(input$`checkbox-evaluate`, {
      #   # add selected datatypes and update data
      #   c(uploadData$schema, input$`checkbox-evaluate`)
      # })

      # render tab title
      setTabTitle("title", paste0("Completion of Requirements for Project: ", sQuote(selectedProject)))
      # render rator system
      dbRater("summary", uploadData, reqData, userName)
      # render summary stats boxes
      dbStatsBox("stats", uploadData, reqData, tabId, validationTab, parent)

      # unique folder names
      folder_list <- sort(unique(uploadData$folder))

      # progress values
      ds_pb_values <- sapply(folder_list, function(f) {
        tmp <- reqData[reqData$folder == f, ]
        round(sum(tmp$to %in% uploadData$schema) / nrow(tmp) * 100, 0)
      })

      # # render ui for evaluation of submission
      # output$`evaluate-submit` <- renderUI({
      #   checkboxGroupInput(ns("checkbox-evaluate"), "Try to submit below missing data types:",
      #     choiceNames = as.list(not_up),
      #     choiceValues = as.list(not_up)
      #   )
      # })

      # # render evaluated new progress by selecting missing datatypes in checkbox
      # observeEvent(input$`checkbox-evaluate`, {
      #   # add selected datatypes and update data
      #   evaluate_datatypes <- input$`checkbox-evaluate`
      #   evaluate_ds <- reqData$folderSynId[reqData$to %in% evaluate_datatypes]
      #   evaluate_ds <- unique(evaluate_ds)
      #   inx <- which(uniq_ds$folderSynId %in% evaluate_ds)
      #   new_unique_ds <- uniq_ds
      #   new_unique_ds$nMiss[inx] <- new_unique_ds$nMiss[inx] - length(evaluate_datatypes)

      #   # get new progress percentage
      #   new_n_completed <- sum(new_unique_ds$nMiss == 0)
      #   new_progress_value <- round(new_n_completed / n_ds * 100)
      #   # output the results
      #   items <- paste(input$`checkbox-evaluate`, collapse = ", ")
      #   output$`evaluate-res` <- renderUI({
      #     span(HTML(paste0("By submitting ", items, " you completion progress will go up to ", progress_value)))
      #   })
      # })


      # render (multiple) progress bar for each dataset
      output$`complete-dataset-pb` <- renderUI({
        inx <- which(ds_pb_values != 100)
        fluidRow(
          # !important: add ns to pb's id, otherwise pb server will not be able to find
          lapply(inx, function(i) {
            column(
              6,
              progressBarUI(ns(folder_list[i])) %>%
                addTooltip(HTML(paste0(folder_list[i], ": ", ds_pb_values[i], "%")), "top")
            )
          })
        )
      })

      output$`incomplete-dataset-pb` <- renderUI({
        inx <- which(ds_pb_values == 100)
        fluidRow(
          # !important: add ns to pb's id, otherwise pb server will not be able to find
          lapply(inx, function(i) {
            column(
              6,
              progressBarUI(ns(folder_list[i])) %>%
                addTooltip(HTML(paste0(folder_list[i], ": ", ds_pb_values[i], "%")), "top")
            )
          })
        )
      })

      lapply(seq_along(folder_list), function(i) {
        progressBar(
          id = folder_list[i],
          value = ds_pb_values[i],
          display_pct = FALSE,
          height = "10px",
          color = "#28a745",
          backgoundCol = "#e53935",
          subtitle = folder_list[i]
        )
      })

      # render collasiple tree
      dbTree("requirement-tree", uploadData, reqData, selectedProject)
    }
  )
}