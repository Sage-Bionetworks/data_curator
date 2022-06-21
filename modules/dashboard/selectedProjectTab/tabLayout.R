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
            column(12, align = "center", uiOutput(ns("tree-container")))
          )
        )
      )
    )
  )
}

selectedProjectTab <- function(id, username, up.data, req.data, selected.project, source.tab, target.tab, parent.session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      ## Summary banner
      # render tab title
      setTabTitle("title", paste0("Completion of Requirements for Project: ", sQuote(selected.project)))
      # render rator system
      dbRater("summary", up.data, req.data, username)
      # render summary stats boxes
      dbStatsBox("stats", up.data, req.data, source.tab, target.tab, parent.session)

      ## Evaluate submission
      # get all uploaded data types
      up_schema <- up.data$schema
      # collect all required datatype including selected datatype
      all_req <- union(req.data$from, req.data$to)
      all_req <- all_req[!grepl("f:", all_req)]
      # get all missing requirements
      not_up_schema <- setdiff(all_req, up_schema)

      # render checkbox options for evaluation of submission
      updateCheckboxGroupInput(
        session,
        "checkbox-evaluate",
        label = ifelse(length(not_up_schema) > 0,
          "It looks like you are missing these data types:",
          "It looks like you completed all requirements !!!"
        ),
        choices = as.list(not_up_schema)
      )

      ## Dataset progress bars
      folder_list <- unique(req.data$folder)
      ds_pb_values <- sapply(folder_list, function(f) {
        tmp <- req.data[req.data$folder == f, ]
        round(sum(tmp$to %in% up_schema) / nrow(tmp) * 100, 0)
      }) %>% set_names(folder_list)

      # reorder based on completion progress
      folder_list <- folder_list[order(ds_pb_values)]

      # change d3 tree height based on how many nodes
      output$`tree-container` <- renderUI({
        dbTreeUI(ns("requirement-tree"), n.nodes = length(folder_list))
      })

      observeEvent(input$`checkbox-evaluate`, ignoreNULL = FALSE, {
        # clean the previous ui to avoid duplicate progress ids
        output$`dataset-pb` <- renderUI(NULL)

        # update required data frame
        evaluate_datatypes <- input$`checkbox-evaluate`

        if (is.null(evaluate_datatypes)) {
          # reset progress values if no evaluated data type selected
          ds_pb_values <- ds_pb_values

          if (length(evaluate_datatypes) > 0) {
            output$`evaluate-res` <- renderUI({
              span(
                class = "warn_msg",
                "Trying to select one or all of above data types to evaulate
              how your progress is going to change"
              )
            })
          }
        } else {
          # substrate # of evaluated datatypes for nMiss
          # note, it will not change the req.data outside of observeEvent
          for (d in evaluate_datatypes) {
            # get which folder contains evaluated datatypes
            evaluate_ds <- unique(req.data$folderSynId[req.data$to == d])
            loc <- which(req.data$folderSynId %in% evaluate_ds)
            req.data$nMiss[loc] <- req.data$nMiss[loc] - 1
          }
          # add evaluated datatypes to update uploaded data
          up_schema <- c(up_schema, input$`checkbox-evaluate`)

          # update progress value each dataset
          ds_pb_values <- sapply(folder_list, function(f) {
            tmp <- req.data[req.data$folder == f, ]
            round(sum(tmp$to %in% up_schema) / nrow(tmp) * 100, 0)
          })

          output$`evaluate-res` <- renderUI({
            # update new total progress
            uniq_ds <- req.data %>% distinct(folderSynId, .keep_all = TRUE)
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

        # render progress bar ui for each dataset
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

        # assign values to progress bar server for each dataset
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

        # render collasiple tree
        dbTree("requirement-tree", up_schema, req.data, selected.project)
      })
    }
  )
}