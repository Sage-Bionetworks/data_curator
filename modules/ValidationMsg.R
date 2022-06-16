
# format and process validation results from schematic

ValidationMsgUI <- function(id) {
  ns <- NS(id)
  htmlOutput(ns("results"))
}

ValidationMsgServer <- function(id, valRes, template, inFile) {
  moduleServer(
    id,
    function(input, output, session) {
      output$results <- renderUI({
        text_class <-
          ifelse(!is.null(valRes$validationRes) && valRes$validationRes == "valid" && length(valRes$validationRes) != 0,
            "success_msg", "error_msg"
          )

        tagList(
          if (length(valRes$error_msg) > 0) {
            div(
              class = "validation-card",
              div(
                class = "validation-card-header error",
                icon("times-circle"),
                span(paste0("Oops, looks like you have ", length(valRes$error_msg), " errors !!!"))
              ),
              helpText(HTML(valRes$error_help_msg)),
              div(
                class = "validation-card-content",
                lapply(1:10, function(i) div(class = "validation-card-msg error", span(HTML(valRes$error_msg)))),
              )
            )
          },
          if (length(valRes$warning_msg) > 0) {
            div(
              class = "validation-card",
              div(
                class = "validation-card-header warning",
                icon("exclamation-circle"),
                span(paste0("Oops, looks like you have ", length(valRes$warning_msg), " warnings !!!"))
              ),
              helpText(HTML(valRes$warning_help_msg)),
              div(
                class = "validation-card-content",
                lapply(1:10, function(i) div(class = "validation-card-msg warning", span(HTML(valRes$warning_msg)))),
              )
            )
          }
        )

        # tagList(
        #   # TODO: remove first two checking once we set dropdown value as 1st selection by default
        #   if (is.null(template)) {
        #     span(class = text_class, HTML("Please <b>select a template</b> from the 'Select your Dataset' tab !<br><br>"))
        #   },
        #   if (is.null(inFile)) {
        #     span(class = text_class, HTML("Please <b>upload</b> a filled template !"))
        #   },
        #   if (!is.null(inFile) & length(inFile) == 0) {
        #     span(class = text_class, HTML("File is empty. Please <b>upload</b> a filled template !"))
        #   },
        #   span(class = text_class, HTML(valRes$outMsg)),
        #   span(class = "error_col", HTML(valRes$outMsg2))
        # )
      })
    }
  )
}