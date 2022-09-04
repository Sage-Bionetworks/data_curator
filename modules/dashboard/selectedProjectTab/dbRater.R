dbRaterUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("dbRater-box"))
  )
}

dbRater <- function(id, progress.value, username, project.name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # rater system
      completion_icon <- case_when(
        progress.value == 0 ~ "street-view",
        progress.value > 0 & progress.value < 60 ~ "running",
        progress.value >= 60 & progress.value < 90 ~ "medal",
        TRUE ~ "crown"
      )
      completion_icon_col <- case_when(
        progress.value < 60 ~ "#5c5ca899",
        progress.value >= 60 & progress.value < 70 ~ "#A77044",
        progress.value >= 70 & progress.value < 80 ~ "#A7A7AD",
        TRUE ~ "#FEE101"
      )
      completion_msg <- case_when(
        progress.value == 0 ~ "Hi",
        progress.value > 0 & progress.value < 60 ~ "Good job",
        TRUE ~ "Congratulations"
      )

      # render banner contents
      output$`dbRater-box` <- renderUI({
        div(
          class = "dbRater-box", align = "center",
          div(
            div(
              class = "dbRater-icon",
              icon(completion_icon, "fa-3x fa-rater-icon",
                style = paste0("color: ", completion_icon_col, ";")
              )
            ),
            div(class = "dbRater-header", h3(class = "bold-normal", paste0(completion_msg, ", ", username, "!!!"))),
            div(
              class = "dbRater-body font-italic",
              paste0("you have made ", progress.value, "% progress for ", sQuote(project.name))
            )
          ),
          progressBarUI(ns("progress-box"))
        )
      })

      # render circular progress bar
      progressBar("progress-box", value = progress.value, circular = TRUE)
    }
  )
}