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
            column(12, class = "section-title", span("Evaluate Submission")),
            column(12, checkboxGroupInput(ns("checkbox-evaluate"), NULL)),
            column(12, uiOutput(ns("evaluate-res"))),
            column(12, class = "section-title", span("Each dataset progress")),
            column(12, align = "center", uiOutput(ns("dataset-pb")))
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

      ## Summary banner
      # render tab title
      setTabTitle("title", paste0("Completion of Requirements for Project: ", sQuote(selectedProject)))
      # render rator system
      dbRater("summary", uploadData, reqData, userName)
      # render summary stats boxes
      dbStatsBox("stats", uploadData, reqData, tabId, validationTab, parent)

      ## Evaluate submission
      # collect all required datatype including selected datatype
      all_req <- union(reqData$from, reqData$to)
      all_req <- all_req[!grepl("f:", all_req)]
      # get all missing required datatypes
      not_up <- setdiff(all_req, uploadData$schema)

      # render checkbox options for evaluation of submission
      updateCheckboxGroupInput(
        session,
        "checkbox-evaluate",
        label = "It looks like you are missing these data types:",
        choices = as.list(not_up)
      )

      ## Dataset progress bars
      folder_list <- unique(reqData$folder)
      ds_pb_values <- sapply(folder_list, function(f) {
        tmp <- reqData[reqData$folder == f, ]
        round(sum(tmp$to %in% uploadData$schema) / nrow(tmp) * 100, 0)
      }) %>% set_names(folder_list)

      # reorder based on completion progress
      folder_list <- folder_list[order(ds_pb_values)]


      observeEvent(input$`checkbox-evaluate`, ignoreNULL = FALSE, {
        # clean the previous ui to avoid duplicate progress ids
        output$`dataset-pb` <- renderUI(NULL)

        # update required data frame
        evaluate_datatypes <- input$`checkbox-evaluate`

        if (is.null(evaluate_datatypes)) {
          # reset progress values
          ds_pb_values <- ds_pb_values
          output$`evaluate-res` <- renderUI({
            span(
              class = "warn_msg",
              "Trying to select one or all of above data types to evaulate
              how your progress is going to change"
            )
          })
        } else {
          evaluate_ds <- reqData$folderSynId[reqData$to %in% evaluate_datatypes]
          inx <- which(reqData$folderSynId %in% evaluate_ds)
          new_req <- reqData
          new_req$nMiss[inx] <- new_req$nMiss[inx] - length(evaluate_datatypes)

          # update uploaded data frame
          new_up_schema <- c(uploadData$schema, input$`checkbox-evaluate`)

          # update progress values
          ds_pb_values <- sapply(folder_list, function(f) {
            tmp <- reqData[new_req$folder == f, ]
            round(sum(tmp$to %in% new_up_schema) / nrow(tmp) * 100, 0)
          })

          output$`evaluate-res` <- renderUI({
            # update new total progress of datasets
            uniq_ds <- new_req %>% distinct(folderSynId, .keep_all = TRUE)
            # number of completed dataset
            n_completed <- sum(uniq_ds$nMiss == 0)
            new_progress <- round(n_completed / nrow(uniq_ds) * 100)
            items <- paste0(sQuote(evaluate_datatypes), collapse = ", ")
            span(class = "warn_msg", HTML(paste0(
              "By submitting: ", items, ", your total progress will become ",
              strong(new_progress), "% !!!"
            )))
          })
        }

        # render (multiple) progress bar for each dataset
        output$`dataset-pb` <- renderUI({
          fluidRow(
            # !important: add ns to pb's id, otherwise pb server will not be able to find
            lapply(folder_list, function(f) {
              # currently, need to use attr on tag object to add tooltip
              # runjs somehow not working in module?
              column(
                6,
                progressBarUI(ns(f)) %>%
                  addTooltip(paste0(f, ": ", ds_pb_values[f], "%"), "top")
              )
            })
          )
        })

        lapply(folder_list, function(f) {
          progressBar(
            id = f,
            value = ds_pb_values[f],
            display_pct = FALSE,
            height = "10px",
            color = "#28a745",
            backgoundCol = "#e53935",
            subtitle = f
          )
        })
      })

      # render collasiple tree
      dbTree("requirement-tree", uploadData, reqData, selectedProject)
    }
  )
}