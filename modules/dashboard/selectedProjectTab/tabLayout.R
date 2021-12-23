selectedProjectTabUI <- function(id) {

  ns <- NS(id)
  div(class="selectedProjectTab-container",
    tagList(
      setTabTitleUI(ns("title")),
      fluidRow(
        column(3,
          fluidRow(
            column(12, class = "section-title", span("Overall progress")),
            column(12, align = "center", 
              tagList(
                progressBarUI(ns("all-pb")),
                uiOutput(ns("dataset-stats"))
              )
            ),
            column(12, class = "section-title", span("Each dataset progress")),
            column(12, align = "center", class = "progress-container", uiOutput(ns("dataset-pb")))
          )
        ),
        column(width = 9,
          fluidRow(
            column(12, class = "section-title", span("Requirment Relationship Tree")),
            column(12, dbTreeUI(ns("requirement-tree")))
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

      # get number of total dataset
      uniq_ds <- reqData %>% group_by(folder, has_miss) %>% distinct(folder)
      n_ds <- nrow(uniq_ds)
      # get number of completed dataset
      n_completed <- sum(!uniq_ds$has_miss)
      # get number of incompleted dataset
      n_miss <- sum(uniq_ds$has_miss)

      # set colors that can be used for dataset progress bars
      col_list <- c("#5B008C", "#B4007A", "#EA3360", "#FF794A", "#FFBB49",
                    "#004BC3", "#0076E2", "#009AE8", "#00BADA", "#00D8C3")
      # set reactive value for folder lists
      folder_list <- reactive(unique(uploadData$folder))

      # render tab title
      setTabTitle("title", paste0("Completion of Requirements for Project: ", sQuote(selectedProject)))

      # render circular progress bar: total number of completed dataset / total number of datasets
      progressBar("all-pb", value = n_completed / n_ds * 100, circular = TRUE)   

      # render (multiple) progress bar for each dataset 
      output$`dataset-pb` <- renderUI({
        fluidRow(
          # !important: add ns to pb's id, otherwise pb server will not be able to find
          lapply(folder_list(), function(f) column(6, progressBarUI(ns(f))))
        )
      })

      observeEvent(folder_list(), {
        lapply(folder_list(), function(f) {
          progressBar(
            id = f,
            value = sample.int(1e2, 1), 
            display_pct = FALSE,
            height = "10px",
            color = sample(col_list, 1),
            subtitle = f
          )
        })
      })
    
      # display stats below the progress
      output$`dataset-stats` <- renderUI({
        div(class = "dataset-stats-container",
          div(class = "dataset-stats",
            column(12, class = "dataset-stats-title", span("Dataset")),
            column(12, class = "dataset-stats-number", span(n_ds))
          ),
          div(class = "dataset-stats",
            column(12, class = "dataset-stats-title", span("Completed")),
            column(12, class = "dataset-stats-number", span(n_completed))
          ),
          div(class = "dataset-stats",
            column(12, class = "dataset-stats-title", span("Missing")),
            column(12, class = "dataset-stats-number", span(n_miss))
          )
        )
      })
      
      # render collasiple tree
      dbTree("requirement-tree", uploadData, reqData, selectedProject)
    }
  )
}
