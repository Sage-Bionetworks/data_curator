# This module is to read csv file

csvInfileUI <- function(id) {
  ns <- NS(id)
  tagList(
    # renders fileInput ui
    fileInput(ns("file"), "Upload CSV File", accept = c(
      "text/csv", "text/comma-separated-values",
      ".csv"
    ))
  )
}

csvInfileServer <- function(id, na = c("", "NA"), colsAsCharacters = FALSE, keepBlank = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      usrFile <- eventReactive(ignoreNULL = FALSE, input$file, {
        # if no file uploaded, return null
        if (is.null(input$file)) {
          return(NULL)
        }

        if (colsAsCharacters) {
          infile <- read_csv(input$file$datapath, na = na, col_types = cols(.default = "c"))
        } else {
          infile <- read_csv(input$file$datapath, na = na)
        }

        if (keepBlank) {
          # change NA to blank to match schema output)
          infile <- infile %>% replace(., is.na(.), "")
        }

        # remove empty rows/columns where readr called it 'X'[digit] for unnamed col
        infile <- infile[, !grepl("^X", colnames(infile))]
        infile <- infile[rowSums(is.na(infile)) != ncol(infile), ]
      })
      return(list(
        raw = reactive({
          input$file
        }),
        data = usrFile
      ))
    }
  )
}
