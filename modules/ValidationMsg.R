
# format and process validation results from schematic

ValidationMsgUI <- function(id) {
  ns <- NS(id)
  htmlOutput(ns("results"))
}

ValidationMsgServer <- function(id, result) {
  moduleServer(
    id,
    function(input, output, session) {
      output$results <- renderUI({
        tagList(
          if (length(result$error_msg) > 0) {
            div(
              class = "validation-card",
              div(
                class = "validation-card-header error",
                icon("times-circle"),
                span(paste0("Oops, looks like you have ", length(result$error_msg), " errors !!!"))
              ),
              if (!is.null(result$error_help_msg)) helpText(class = "validation-card-help-msg", HTML(result$error_help_msg)),
              div(
                class = "validation-card-content",
                lapply(result$error_msg, function(msg) div(class = "validation-card-msg error", span(HTML(msg)))),
              )
            )
          },
          if (length(result$warning_msg) > 0) {
            div(
              class = "validation-card",
              div(
                class = "validation-card-header warning",
                icon("exclamation-circle"),
                span(paste0("Oops, looks like you have ", length(result$warning_msg), " warnings !!!"))
              ),
              if (!is.null(result$warning_help_msg)) helpText(class = "validation-card-help-msg", HTML(result$warning_help_msg)),
              div(
                class = "validation-card-content",
                lapply(result$warning_msg, function(msg) div(class = "validation-card-msg warning", span(HTML(msg)))),
              )
            )
          }
        )
      })
    }
  )
}