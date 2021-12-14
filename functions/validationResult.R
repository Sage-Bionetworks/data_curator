validationResult <- function(valRes, template, inFile) {
  validation_res <- NULL
  error_msg <- NULL
  help_msg <- NULL
  outMsg <- NULL
  errorDT <- NULL
  errorType <- NULL

  if (!is.null(inFile) && !is.null(template) && length(inFile) != 0) {
    if (length(valRes) != 0) {
      validation_res <- "invalid"
      # mismatched template index
      inx_mt <- which(sapply(valRes, function(x) {
        grepl(
          "Component value provided is: .*, whereas the Template Type is: .*",
          x[[3]]
        )
      }))
      # missing column index
      inx_ws <- which(sapply(valRes, function(x) {
        grepl(
          "Wrong schema",
          x[[2]]
        )
      }))

      if (length(inx_mt) > 0) {
        # mismatched error(s): selected template mismatched with validating template
        errorType <- "Mismatched Template"
        # get all mismatched components
        error_values <- sapply(valRes[inx_mt], function(x) x[[4]][[1]]) %>%
          unique()

        # error messages for mismatch
        mismatch_c <- error_values %>%
          sQuote() %>%
          paste(collapse = ", ")
        error_msg <- paste0(
          "The submitted metadata contains << <b>",
          mismatch_c, "</b> >> in the Component column, but requested validation for << <b>",
          template, "</b> >>."
        )
        help_msg <- paste0(
          "Please check that you have selected the correct template in the <b>Select your Dataset</b> tab and
									ensure your metadata contains <b>only</b> one template, e.g. ",
          template, "."
        )
      } else if (length(inx_ws) > 0) {
        # wrong schema error(s): validating metadata miss any required columns
        errorType <- "Wrong Schema"
        error_msg <- "The submitted metadata does not contain all required column(s)."
        help_msg <- "Please check that you used the correct template in the <b>'Get Metadata Template'</b> tab and
						ensure your metadata contains all required columns."
      } else {
        errorType <- "Invalid Value"
        error_msg <- paste0(
          "The submitted metadata have ", length(valRes),
          " errors."
        )
      }

      errorDT <- lapply(valRes, function(i) {
        error_row <- i[[1]] 
        error_col <- i[[2]]
        # get invalid value
        error_val <- inFile[as.numeric(error_row) - 1, error_col]
        tibble(
          Row = error_row,
          Column = error_col,
          Value = error_val,
          Error = i[[3]]
        ) %>% mutate(across(everything(), as.character))
      }) %>% bind_rows()

      # collapse similiar errors into one row
      errorDT <- errorDT %>%
        mutate(Error = gsub(".*(not.*)\\.?$", "\\1", Error)) %>%
        group_by(Column, Error) %>%
        summarise(
          Row = str_c(unique(Row), collapse = ", ") %>% TruncateEllipsis(10, ", "),
          Value = str_c(unique(Value), collapse = ", ") %>% TruncateEllipsis(10, ", "),
          .groups = "drop"
        ) %>%
        ungroup() %>%
        dplyr::select(Row, Column, Value, Error)

      # sort rows based on input column names
      errorDT <- errorDT[order(match(errorDT$Column, colnames(inFile))), ]

    } else {
      validation_res <- "valid"
      errorType <- "No Error"
    }

    # combine all error messages into one, add an extra empty line to bottom
    outMsg <- paste0(c(
      paste0("Your metadata is <b>", validation_res, "</b> !!!"),
      error_msg, help_msg
    ), collapse = "<br><br>")
  }

  return(list(
    validationRes = validation_res,
    outMsg = outMsg,
    errorDT = errorDT,
    errorType = errorType
  ))
}
