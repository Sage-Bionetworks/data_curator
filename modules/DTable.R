# This moduel is to peformrender DT table function for preview/highlight

DTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("table"))
}

DTableServer <- function(id, data, escape = TRUE, highlight = NULL,
                         rownames = TRUE, caption = NULL, filter = "top",
                         selection = "none", cell_border = FALSE,
                         options = list(lengthChange = FALSE, scrollX = TRUE),
                         ht.color = NULL, ht.column = NULL, ht.value = NULL) {
  df <- datatable(data,
    caption = caption,
    escape = escape,
    rownames = rownames,
    selection = selection,
    filter = filter,
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

    if (cell_border) {
      df <- df %>% formatStyle(1:ncol(data), border = "1px solid #ddd")
    }
  }

  moduleServer(
    id,
    function(input, output, session) {
      output$table <- renderDT(df)
    }
  )
}
