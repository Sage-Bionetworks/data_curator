validationResult <- function(anno.res, template, manifest) {
  validation_res <- NULL
  error_msg <- NULL
  help_msg <- NULL
  out_msg <- NULL
  error_table <- NULL
  error_type <- NULL
  highlight_values <- list()

  # get erorrs only for now, [[2]] is warning
  errors <- anno.res[[1]]

  if (!is.null(manifest) && !is.null(template) && nrow(manifest) != 0) {
    if (length(errors) != 0) {
      validation_res <- "invalid"
      # mismatched template index
      inx_mt <- which(sapply(errors, function(x) {
        grepl(
          "Component value provided is: .*, whereas the Template Type is: .*",
          x[[3]]
        )
      }))
      # missing column index
      inx_ws <- which(sapply(errors, function(x) {
        grepl(
          "Wrong schema",
          x[[2]]
        )
      }))

      # cross-validation error index
      inx_cv <- which(sapply(errors, function(x) {
        grepl(
          "placeholder",
          x[[3]]
        )
      }))

      if (length(inx_mt) > 0) {
        # mismatched error(s): selected template mismatched with validating template
        error_type <- "Mismatched Template"
        # get all mismatched components
        error_values <- sapply(errors[inx_mt], function(x) x[[4]][[1]]) %>%
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
        error_type <- "Wrong Schema"
        error_msg <- "The submitted metadata does not contain all required column(s)."
        help_msg <- "Please check that you used the correct template in the <b>'Get Metadata Template'</b> tab and
						ensure your metadata contains all required columns."
      } else if (length(inx_cv) > 0) {
        # cross-manifest errors: not pass the cross-manifest validation rules
        error_type <- ""
        error_msg <- ""
        help_msg <- ""
      } else {
        error_type <- "Invalid Value"
        error_msg <- paste0(
          "The submitted metadata have ", length(errors),
          " errors."
        )
      }

      # create table to display errors for users
      error_table <- lapply(errors, function(i) {
        data.frame(Row = as.numeric(i[[1]]), Column = i[[2]], Value = i[[4]][[1]], Error = i[[3]])
      }) %>% bind_rows()

      # create list for hightlight function; key: error_column, value: error_value
      lapply(unique(error_table$Column), function(col) {
        highlight_values[[col]] <<- error_table$Value[error_table$Column == col]
      })

      # collapse similiar errors into one row
      error_table <- error_table %>%
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
      error_table <- error_table[order(match(error_table$Column, colnames(manifest))), ]
    } else {
      validation_res <- "valid"
      error_type <- "No Error"
    }

    # combine all error messages into one, add an extra empty line to bottom
    out_msg <- paste0(c(
      paste0("Your metadata is <b>", validation_res, "</b> !!!"),
      error_msg, help_msg
    ), collapse = "<br><br>")
  }

  return(list(
    validationRes = validation_res,
    outMsg = out_msg,
    errorDT = error_table,
    errorHighlight = highlight_values,
    errorType = error_type
  ))
}