dbValidationUI <- function(id) {
  
  ns <- NS(id)
  
  DT::DTOutput(ns("validation-table"))
}

dbValidation <- function(id, data, columns = "Status") {
  moduleServer(
    id,
    function(input, output, session) {

      output$`validation-table` <- renderDT({

        shiny::validate(
          need(nrow(data) != 0, "It seems like you do not have uploaded files !!!")
        )

        datatable(
          data,
          caption = tags$caption(HTML(
            paste0(
              "Invalid Results: <b>", sum(data$Status == "invalid"), "</b>", "<br>",
              "Schematic Version: <code>v1.0.0</code> ",
              tags$a(icon("github"), style = "color:#000;", href = "https://github.com/Sage-Bionetworks/schematic", target = "_blank"),
              "</b>", "<br><br>",
              "Click on the data type name to download your existing data."
            )
          )),
          escape = FALSE,
          options = list(dom = "t", columnDefs = list(list(className = "dt-center", targets = "_all")))
        ) %>%
        formatStyle(columns, backgroundColor = styleEqual(c("Pass", "Fail"), c("#82E0AA", "#F7DC6F")))
      })
    }
  )
}