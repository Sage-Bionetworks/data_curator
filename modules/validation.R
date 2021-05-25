
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
          # TODO: remove first two checking once we set dropdown value as 1st selection by default
          if (is.null(template)) {
            span(class = text_class, HTML("Please <b>select a template</b> from the 'Select your Dataset' tab !<br><br>"))
          },
          if (is.null(inFile)) {
            span(class = text_class, HTML("Please <b>upload</b> a filled template !"))
          },
          if (length(inFile) == 0) {
            span(class = text_class, HTML("File is empty. Please <b>upload</b> a filled template !"))
          },
          span(class = text_class, HTML(valRes$outMsg))
        )
      })
    }
  )
}
