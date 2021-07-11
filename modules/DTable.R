# This moduel is to peformrender DT table function for preview/highlight

DTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("table"))
}

DTableServer <- function(id, data, highlight = NULL,
                         rownames = FALSE, caption = NULL, escape = TRUE,
                         options = list(lengthChange = FALSE, scrollX = TRUE),
                         ht.color = NULL, ht.column = NULL, ht.value = NULL) {
  
  df <- datatable(data,
    caption = caption,
    escape = escape,
    rownames = rownames,
    options = options
  )

  if (!is.null(highlight)) {

    match.arg(highlight, c("row", "column"))

    if (is.null(ht.color)) {
      ht.color <- rep("yellow", length(ht.value))
    }

    if (highlight == "row") {
      df <- df %>%
        formatStyle(ht.column, target = "row", backgroundColor = styleEqual(ht.value, ht.color))

    } else {
      df <- df %>%
        formatStyle(ht.column, backgroundColor = styleEqual(ht.value, ht.color))
    }
  }

  moduleServer(
    id,
    function(input, output, session) {
      output$table <- renderDT(df)
    }
  )
}