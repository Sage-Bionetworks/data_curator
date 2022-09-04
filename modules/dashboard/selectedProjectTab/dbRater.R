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
      completion_icon <- if_else(progress.value >= 90, "crown", "medal")
      completion_icon_col <- case_when(
        progress.value < 50 ~ "#A77044",
        progress.value >= 50 & progress.value < 75 ~ "#A7A7AD",
        progress.value >= 75 & progress.value < 90 ~ "#FEE101",
        TRUE ~ "#F5BD02"
      )

      # render banner contents
      output$`dbRater-box` <- renderUI({
        div(
          class = "dbRater-box", align = "center",
          div(
            div(
              class = "dbRater-icon",
              icon(completion_icon, "fa-3x",
                style = paste0(
                  "color: ", completion_icon_col, ";
                  border: 2.5px solid ", completion_icon_col, ";"
                )
              )
            ),
            div(class = "dbRater-header", h3(paste0("Congratulations ", username, "!"))),
            div(
              class = "dbRater-body",
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