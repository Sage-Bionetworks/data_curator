
# format and process validation results from schematic

validationResult <- function(valRes, template, inFile) {
  validation_res <- NULL
  error_msg <- NULL
  help_msg <- NULL
  errorDT <- NULL
  waiter_msg <- NULL
  errorType <- NULL

  if (!is.null(inFile) & !is.null(template)) {
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
        waiter_msg <- "Mismatched Template Found !"
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

        # get wrong columns and values for updating preview table
        errorDT <- data.frame(
          Column = sapply(valRes[inx_mt], function(i) i[[2]]),
          Value = sapply(valRes[inx_mt], function(i) i[[4]][[1]]),
          Error = ""
        )
      } else if (length(inx_ws) > 0) {
        # wrong schema error(s): validating metadata miss any required columns
        waiter_msg <- "Wrong Schema Used !"
        error_msg <- "The submitted metadata does not contain all required column(s)."
        help_msg <- "Please check that you used the correct template in the <b>'Get Metadata Template'</b> tab and
						ensure your metadata contains all required columns."
      } else {
        waiter_msg <- sprintf("%d errors found", length(valRes))
        error_msg <- paste0(
          "The submitted metadata have ", length(valRes),
          " errors."
        )

        errorDT <- data.frame(
          Column = sapply(valRes, function(i) i[[2]]),
          Value = sapply(valRes, function(i) i[[4]][[1]]),
          Error = sapply(valRes, function(i) i[[3]])
        )
        # sort rows based on input column names
        errorDT <- errorDT[order(match(errorDT$Column, colnames(inFile))), ]
        # TODO: to reduce parameter, sort just based on alphabetic
        # errorDT <- errorDT[order(errorDT$Column),]
      }


      errorType <- ifelse(length(inx_ws) > 0, "Wrong Schema",
        ifelse(length(inx_mt) > 0, "Mismatch Template", "Invalid Value")
      )
    } else {
      validation_res <- "valid"
    }

    outMsg <- paste0(c(
      paste0("Your metadata is <b>", validation_res, "</b> !!!"),
      error_msg, help_msg
    ), collapse = "<br><br>")
  }

  return(list(
    validationRes = validation_res,
    outMsg = outMsg,
    errorDT = errorDT,
    errorType = errorType,
    waiterMsg = waiter_msg
  ))
}

ValidationMsgUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("results"))
  )
}

ValidationMsgServer <- function(id, valRes, template, inFile) {
  moduleServer(
    id,
    function(input, output, session) {
      output$results <- renderUI({
        text_class <- ifelse(!is.null(valRes$validationRes) && valRes$validationRes == "valid",
          "success_msg", "error_msg"
        )

        tagList(
          if (is.null(template)) {
            span(class = text_class, HTML("Please <b>select a template</b> from the 'Select your Dataset' tab !<br><br>"))
          },
          if (is.null(inFile)) {
            span(class = text_class, HTML("Please <b>upload</b> a filled template !"))
          },
          span(class = text_class, HTML(valRes$outMsg))
        )
      })
    }
  )
}
