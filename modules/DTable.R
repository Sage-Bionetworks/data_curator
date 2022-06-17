#' This moduel is to wrap some of DT table functions, especially for hightlight functions
#'
#' @param id id name of this module
#' @param data a data object (either a matrix or a data frame)
#' @param rownames TRUE (show row names) or FALSE (hide row names)
#' @param caption a character vector or a tag object generated from \code{htmltools::tags$caption()}
#' @param filter whether/where to use column filters, either "none", "top" or "bottom"
#' @param options a list of initialization options (see https://datatables.net/reference/option/)
#' @param highlight whether to highlight cells in the table, either "full" (highlight entired table) or "partial" (highlight certain cells based on \code{highlightValues})
#' @param highlightValues a list of values to be highlighted; each element of list following by key (column names) and value (a vector of values)
#' @return a reactive table
DTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("table"))
}

DTableServer <- function(id, data,
                         rownames = TRUE, caption = NULL, filter = "top",
                         options = list(lengthChange = FALSE, scrollX = TRUE),
                         highlight = NULL, highlightValues = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      df <- datatable(data,
        caption = caption,
        rownames = rownames,
        filter = filter,
        options = options
      )

      if (!is.null(highlight)) {
        match.arg(highlight, c("full", "partial"))

        if (highlight == "full") {
          df <- df %>% formatStyle(1, target = "row", backgroundColor = "yellow")
        } else if (highlight == "partial") {

          # iterate each col to avoid messing around same value in multiple columns
          for (col in names(highlightValues)) {
            values <- highlightValues[[col]]
            # if NULL is provided for values, it will highlight entire columns
            if ("ht_entire_column" %in% values) style <- "yellow" else style <- styleEqual(values, rep("yellow", length(values)))
            df <- df %>% formatStyle(col, backgroundColor = style)
          }
        }
      }

      output$table <- renderDT(df)
    }
  )
}