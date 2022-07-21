validationResult <- function(anno.res, template, manifest) {
  result <- "invalid"
  error_msg <- NULL
  error_help_msg <- NULL
  warning_msg <- NULL
  warning_help_msg <- NULL
  error_table <- NULL
  error_type <- NULL
  highlight_values <- list()

  # if no uploaded manifest or empty manifest
  if (is.null(manifest) || nrow(manifest) == 0) {
    return(list(
      result = "invalid",
      error_type = "Empty File",
      error_msg = "Please <b>upload</b> a filled in template !"
    ))
  }

  # in case there are some errors we haven't captured
  if (is.null(anno.res)) {
    return(list(
      result = "invalid",
      error_type = "Out of Date",
      error_msg = "Something went wrong while validating your manifest.",
      error_help_msg = "Please contact DCC staff for assistance."
    ))
  }

  errors <- anno.res[[1]]
  warns <- anno.res[[2]]

  # format the errors
  if (length(errors) != 0) {
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
      error_help_msg <- paste0(
        "Please check that you have selected the correct template in the <b>Select your Dataset</b> tab and
									ensure your metadata contains <b>only</b> one template, e.g. ",
        template, "."
      )
    } else if (length(inx_ws) > 0) {
      # wrong schema error(s): validating metadata miss any required columns
      error_type <- "Wrong Schema"
      error_msg <- "The submitted metadata does not contain all required column(s)."
      error_help_msg <- "Please check that you used the correct template in the <b>'Get Metadata Template'</b> tab and
						ensure your metadata contains all required columns."
    } else {
      error_type <- "Invalid Value"
      # create table to display errors for users
      error_table <- purrr::map_dfr(
        errors,
        ~ tibble(Row = .x[[1]], Column = .x[[2]], Value = .x[[4]][[1]], Error = .x[[3]]) %>%
          mutate(across(everything(), as.character)) # avoid mismatched types
      )

      # create list for hightlight function; key: error_column, value: error_value
      lapply(unique(error_table$Column), function(col) {
        highlight_values[[col]] <<- error_table$Value[error_table$Column == col]
      })

      # concatenated similar errors into one error message
      # TODO: remove below code chunck if the backend error messages are standardized
      error_table <- error_table %>%
        mutate(Error = gsub(".*(not.*)\\.?$", "\\1", Error)) %>%
        group_by(Column, Error) %>%
        summarise(
          Row = str_c(unique(Row), collapse = ", ") %>% truncate_ellipsis(3, ", "),
          Value = str_c(sQuote(unique(Value)), collapse = ", ") %>% truncate_ellipsis(3, ", "),
          .groups = "drop"
        ) %>%
        ungroup() %>%
        mutate(new_error = paste0("<b>", Value, "</b> from row(s) ", Row, " in column <b>'", Column, "'</b>: ", Error))

      # sort rows based on input column names
      error_table <- error_table[order(match(error_table$Column, colnames(manifest))), ]
      error_msg <- error_table$new_error
      error_help_msg <- "View all the error(s) highlighted in the preview table above"
    }
  } else {
    result <- "valid"
    error_type <- "No Error"
  }

  # format the warnings
  if (length(warns) != 0) {
    # add warning values to highlight
    # return which warning needed to highlight entire column
    lapply(warns, function(warn) {
      # matchExactOne (set) will output NULL for values
      # similar warnings in the same column should be concatenated from backend, like "['value1', 'value2', ...]"
      # extract the single quoted values from the warning string
      if (!is.null(warn[[4]])) {
        warn_values <- gsub("^[^']*'|'\\],?$", "", strsplit(warn[[4]], "'(?=,)", perl = TRUE)[[1]])
      } else {
        # if matchExactOne (set) warning exist, highlight entire column
        warn_values <- "ht_entire_column"
      }
      # append the values in case the column has multiple warnings or already has errors
      highlight_values[[warn[[2]]]] <<- append(highlight_values[[warn[[2]]]], warn_values)
      warning_msg <<- append(warning_msg, warn[[3]])
    })

    warning_help_msg <- "View all the warning(s) highlighted in the preview table above"
  }

  return(list(
    result = result,
    error_msg = error_msg,
    error_help_msg = error_help_msg,
    error_type = error_type,
    warning_msg = warning_msg,
    warning_help_msg = warning_help_msg,
    preview_highlight = highlight_values
  ))
}