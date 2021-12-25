selectedProjectTabUI <- function(id) {

  ns <- NS(id)
  div(class="selectedProjectTab-container",
    tagList(
      setTabTitleUI(ns("title")),
      fluidRow(
        column(3, class = "progress-box",
          fluidRow(
            column(12, class = "section-title", span("Overall progress")),
            column(12, align = "center", 
              tagList(
                progressBarUI(ns("all-pb")),
                uiOutput(ns("dataset-stats"))
              )
            ),
            column(12, class = "section-title", span("Each dataset progress")),
            column(12, align = "center", uiOutput(ns("dataset-pb")))
          )
        ),
        column(width = 9, class = "tree-box",
          fluidRow(
            column(12, class = "section-title", span("Requirment Relationship Tree")),
            column(12, align = "center", dbTreeUI(ns("requirement-tree")))
          )
        )
      )
    )
  )
}

selectedProjectTab <- function(id, uploadData, reqData, selectedProject) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns

      # number of total dataset
      uniq_ds <- reqData %>% distinct(folderSynId, .keep_all = TRUE)
      n_ds <- nrow(uniq_ds)
      # number of completed dataset
      n_completed <- sum(uniq_ds$nMiss == 0)
      # number of incompleted dataset
      n_miss <- sum(uniq_ds$nMiss > 0)

      # unique folder names
      folder_list <- unique(uploadData$folder)

      # set colors that can be used for dataset progress bars
      col_list <- rep(c("#5B008C", "#B4007A", "#EA3360", "#FF794A", "#FFBB49",
                    "#004BC3", "#0076E2", "#009AE8", "#00BADA", "#00D8C3"), 2)
      # progress values
      ds_pb_values <- sapply(folder_list, function(f) {
        tmp <- reqData[reqData$folder == f, ]
        sum(tmp$to %in% uploadData$schema) / nrow(tmp)  * 100
      })

      # render tab title
      setTabTitle("title", paste0("Completion of Requirements for Project: ", sQuote(selectedProject)))
      # render circular progress bar: total number of completed dataset / total number of datasets
      progressBar("all-pb", value = n_completed / n_ds * 100, circular = TRUE)   

      # render (multiple) progress bar for each dataset 
      output$`dataset-pb` <- renderUI({
        fluidRow(
          # !important: add ns to pb's id, otherwise pb server will not be able to find
          lapply(folder_list, function(f) column(6, progressBarUI(ns(f))))
        )
      })

      lapply(seq_along(folder_list), function(i) {
        progressBar(
          id = folder_list[i],
          value = ds_pb_values[i], 
          display_pct = FALSE,
          height = "10px",
          color = col_list[i],
          subtitle = folder_list[i]
        )
      })

      # display stats below the progress
      output$`dataset-stats` <- renderUI({
        div(class = "dataset-stats-container",
          div(class = "dataset-stats",
            div(class = "dataset-stats-title", span("Dataset")),
            div(class = "dataset-stats-number", span(n_ds))
          ),
          div(class = "dataset-stats",
            div(class = "dataset-stats-title", span("Completed")),
            div(class = "dataset-stats-number", span(n_completed))
          ),
          div(class = "dataset-stats",
            div(class = "dataset-stats-title", span("Missing")),
            div(class = "dataset-stats-number", span(n_miss))
          )
        )
      })
    
      # render collasiple tree
      dbTree("requirement-tree", uploadData, reqData, selectedProject)
    }
  )
}
