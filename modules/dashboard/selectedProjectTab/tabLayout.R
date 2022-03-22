selectedProjectTabUI <- function(id) {
  ns <- NS(id)
  div(
    class = "selectedProjectTab-container",
    tagList(
      setTabTitleUI(ns("title")),
      fluidRow(
        column(12,
          class = "banner-container",
          column(6, uiOutput(ns("summary-box"))),
          column(6, uiOutput(ns("stats-box")))
        ),
        column(
          3,
          fluidRow(
            # column(12, class = "section-title", span("Placehold for a dropdown")),
            # column(
            #   12,
            #   selectInput(
            #     inputId = ns("test123"),
            #     label = "placeholder",
            #     choices = c(10, 50, 75, 90)
            #   )
            # ),
            column(12, class = "section-title", span("Each dataset progress")),
            column(12, align = "center", uiOutput(ns("complete-dataset-pb"))),
            column(12, align = "center", uiOutput(ns("incomplete-dataset-pb")))
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

selectedProjectTab <- function(id, userName, uploadData, reqData, selectedProject) {
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
      #
      progress_value <- round(n_completed / n_ds * 100)

      # collect all required datatype including selected datatype
      all_req <- union(reqData$from, reqData$to)
      # get number of manifests are required but not yet uploaded
      n_not_up <- length(setdiff(all_req, uploadData$schema))
      # get number of manifests uploaded to synapse
      n_up <- length(intersect(all_req, uploadData$schema))
      # get number of manifests that are out of date
      n_outdated <- sum(uploadData$errorType %in% c("Wrong Schema", "Out of Date"))

      # unique folder names
      folder_list <- sort(unique(uploadData$folder))

      # set colors that can be used for dataset progress bars
      # col_list <- c("#5B008C", "#B4007A", "#EA3360", "#FF794A", "#FFBB49",
      #               "#004BC3", "#0076E2", "#009AE8", "#00BADA", "#00D8C3")
      # progress values
      ds_pb_values <- sapply(folder_list, function(f) {
        tmp <- reqData[reqData$folder == f, ]
        round(sum(tmp$to %in% uploadData$schema) / nrow(tmp) * 100, 0)
      })

      # render tab title
      setTabTitle("title", paste0("Completion of Requirements for Project: ", sQuote(selectedProject)))

      # rater system
      completion_icon <- ifelse(progress_value >= 90, "crown", "medal")
      completion_icon_col <- case_when(
        progress_value < 50 ~ "#A77044",
        progress_value >= 50 & progress_value < 75 ~ "#A7A7AD",
        progress_value >= 75 & progress_value < 90 ~ "#FEE101",
        TRUE ~ "#F5BD02"
      )
      # render banner contents
      output$`summary-box` <- renderUI({
        div(
          class = "summary-box", align = "center",
          div(
            div(
              class = "summary-icon",
              icon(completion_icon, "fa-3x",
                style = paste0(
                  "color: ", completion_icon_col, ";
                  border: 2.5px solid ", completion_icon_col, ";"
                )
              )
            ),
            div(class = "summary-header", h3(paste0("Congratulations ", userName, "!"))),
            div(
              class = "summary-body",
              paste0("you have made ", progress_value, "% progress")
            )
          ),
          progressBarUI(ns("progress-box"))
        )
      })

      # render summary stats boxes
      output$`stats-box` <- renderUI({
        div(
          class = "stats-box",
          div(
            class = "stats-item-container",
            div(
              class = "stats-item completed",
              tagList(
                icon("smile-wink", "fa-3x"),
                div(
                  class = "stat-text",
                  h4("Completed"), span(n_up)
                )
              )
            ),
            div(
              class = "stats-item missing",
              tagList(
                icon("frown", "fa-3x"),
                div(
                  class = "stat-text",
                  h4("Missing"), span(n_not_up)
                )
              )
            ),
            div(
              class = "stats-item outdated",
              tagList(
                icon("surprise", "fa-3x"),
                div(
                  class = "stat-text",
                  h4("Out of Date"), span(n_outdated)
                )
              )
            )
          ),
          div(
            class = "stats-btn-container",
            actionButton(ns("view-btn"), "View More", class = "btn-primary-color"),
            span("Click to see the Out-of-Date manifests")
          )
        )
      })

      # render circular progress bar: total number of completed dataset / total number of datasets
      progressBar("progress-box", value = progress_value, circular = TRUE)

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

      # display stats below the progress
      output$`dataset-stats` <- renderUI({
        div(
          class = "dataset-stats-container",
          div(
            class = "dataset-stats",
            div(class = "dataset-stats-title", span("Dataset")),
            div(class = "dataset-stats-number", span(n_ds))
          ),
          div(
            class = "dataset-stats",
            div(class = "dataset-stats-title", span("Completed")),
            div(class = "dataset-stats-number", span(n_completed))
          ),
          div(
            class = "dataset-stats",
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