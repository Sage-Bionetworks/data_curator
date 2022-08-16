dbRaterUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("dbRater-box"))
  )
}

dbRater <- function(id, up.data, req.data, user.name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # number of total dataset
      # AW 10AUG22 had to change folderSynId to folder_id in req.data
      uniq_ds <- req.data %>% distinct(folder_id, .keep_all = TRUE)
      n_ds <- nrow(uniq_ds)
      # number of completed dataset
      n_completed <- sum(uniq_ds$nMiss == 0)
      # number of incompleted dataset
      n_miss <- sum(uniq_ds$nMiss > 0)
      # completion progress percentage: complete datasets / total datasets
      progress_value <- round(n_completed / n_ds * 100)

      # rater system
      completion_icon <- ifelse(progress_value >= 90, "crown", "medal")
      completion_icon_col <- case_when(
        progress_value < 50 ~ "#A77044",
        progress_value >= 50 & progress_value < 75 ~ "#A7A7AD",
        progress_value >= 75 & progress_value < 90 ~ "#FEE101",
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
            div(class = "dbRater-header", h3(paste0("Congratulations ", user.name, "!"))),
            div(
              class = "dbRater-body",
              paste0("you have made ", progress_value, "% progress")
            )
          ),
          progressBarUI(ns("progress-box"))
        )
      })

      # render circular progress bar
      progressBar("progress-box", value = progress_value, circular = TRUE)
    }
  )
}