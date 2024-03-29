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

DTableServer <- function(id, data, escape = TRUE,
                         rownames = TRUE, caption = NULL, filter = "top",
                         selection = "none", cell_border = FALSE,
                         options = list(lengthChange = FALSE, scrollX = TRUE),
                         highlight = NULL, highlightValues = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      df <- datatable(data,
        caption = caption,
        escape = escape,
        rownames = rownames,
        selection = selection,
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
            # This check prevents errors of the following form 
            #   Warning: Error in name2int: You specified the columns: 0,
            #   but the column names of the data are  , Component, ...
            if (! col %in% names(data)) next()
            values <- highlightValues[[col]]
            if (length(values) == 0) next()
            # if NULL is provided for values, it will highlight entire columns
            if ("ht_entire_column" %in% values) style <- "yellow" else style <- styleEqual(values, rep("yellow", length(values)))
            df <- df %>% formatStyle(col, backgroundColor = style)
          }
        }
      }

      if (cell_border) {
        df <- df %>% formatStyle(1:ncol(data), border = "1px solid #ddd")
      }

      output$table <- renderDT(df, future = TRUE)
    }
  )
}
