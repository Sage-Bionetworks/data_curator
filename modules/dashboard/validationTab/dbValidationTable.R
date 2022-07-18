dbValidationTableUI <- function(id) {
  ns <- NS(id)

  DT::DTOutput(ns("validation-table"))
}

dbValidationTable <- function(id, data, columns = "Validation") {
  moduleServer(
    id,
    function(input, output, session) {
      output$`validation-table` <- renderDT({
        shiny::validate(
          need(nrow(data) != 0, "It seems like you do not have uploaded files !!!")
        )

        DT::datatable(
          data,
          escape = FALSE,
          options = list(scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = "_all")))
        ) %>%
          formatStyle(columns, backgroundColor = styleEqual(c("Pass", "Warning", "Fail"), c("#82E0AA", "#F7DC6F", "#ffcccb")))
      })
    }
  )
}
