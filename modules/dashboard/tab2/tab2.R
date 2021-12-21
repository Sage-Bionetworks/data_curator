allUploadManifestsTabUI <- function(id) {

  ns <- NS(id)
  div(class="tab2-container",
    fluidRow(
      column(3,
        fluidRow(
          column(12, class = "section-title", span("Each dataset progress")),
          column(12, align = "center", class = "progress-container", uiOutput(ns("dataset-pb"))),
          column(12, class = "section-title", span("Overall progress")),
          column(12, align = "center", 
            tagList(
              progressBarUI(ns("all-pb")),
              uiOutput(ns("dataset-stats"))
            )
          )
        )
      ),
      column(width = 9,
        fluidRow(
          column(12, class = "section-title", span("Requirment Tree")),
          column(12, dbTreeUI(ns("requirement-tree")))
        )
      )
    )
  )
}

allUploadManifestsTab <- function(id, uploadData, reqData, project) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      col_list <- c("#5B008C", "#B4007A", "#EA3360", "#FF794A", "#FFBB49",
                    "#004BC3", "#0076E2", "#009AE8", "#00BADA", "#00D8C3")
      folder_list <- reactive(unique(uploadData$folder))

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

      progressBar("all-pb", value = 50, circular = TRUE)

      output$`dataset-stats` <- renderUI({
        div(class = "dataset-stats-container",
          div(class = "dataset-stats",
            column(12, class = "dataset-stats-title", span("Dataset")),
            column(12, class = "dataset-stats-number", span("10"))
          ),
          div(class = "dataset-stats",
            column(12, class = "dataset-stats-title", span("Completed")),
            column(12, class = "dataset-stats-number", span("5"))
          ),
          div(class = "dataset-stats",
            column(12, class = "dataset-stats-title", span("Missing")),
            column(12, class = "dataset-stats-number", span("5"))
          )
        )
      })
    
      dbTree("requirement-tree", uploadData, reqData, project)
    }
  )
}
