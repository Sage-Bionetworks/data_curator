# This moduel is to peformrender DT table function for preview/highlight

DTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("table"))
}

DTableServer <- function(id, data,
                         rownames = TRUE, caption = NULL,
                         options = list(lengthChange = FALSE, scrollX = TRUE),
                         highlight = NULL, hightlight.col = NULL, hightlight.value = NULL) {
  if (!is.null(highlight)) {
    if (!highlight %in% c("full", "partial")) {
      Stop("Please choose a value for highlight: 'full', 'partial'.")
    }

    column <- hightlight.col
    value <- hightlight.value
  }

  df <- datatable(data,
    caption = caption,
    rownames = rownames,
    options = options
  )

  if (!is.null(highlight)) {
    if (highlight == "full") {
      df <- df %>%
        formatStyle(1, target = "row", backgroundColor = "yellow")
    } else if (highlight == "partial") {
      df <- df %>%
        formatStyle(column, backgroundColor = styleEqual(
          value, rep("yellow", length(value))
        ))
    }
  }

  moduleServer(
    id,
    function(input, output, session) {
      output$table <- renderDT(df)
    }
  )
}
