validationTabUI <- function(id) {

  ns <- NS(id)
  div(class="tab3-container",
    tagList(
      setTabTitleUI(ns("title")),
      dbValidationUI(ns("validation-table")),
      helpText("If there is any validation error, 
        please re-validate the corresponding metadata to see detailed errors and re-submit once you have corrected metadata.")
    )
  )
}

validationTab <- function(id, uploadData, selectedProject) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns

      # render tab title
      setTabTitle("title", paste0("Validate your uploaded data in the project: ", sQuote(selectedProject)))

      internalLink <- sample(c("Pass", "Fail"), nrow(uploadData), replace = TRUE)
      internalLinkRes <- ifelse(internalLink == "Pass", TRUE, FALSE)
      valRes <- ifelse(uploadData$errorType == "Wrong Schema", "Out of Date", 
        ifelse(uploadData$errorType == "No Error", "Valid", uploadData$errorType))

      # process validation result
      validation_df <- data.frame(
        DataType = paste0(
          '<a href="https://www.synapse.org/#!Synapse:',
          uploadData$synID, '" target="_blank">', uploadData$schema, "</a>"
        ),
        Dataset = uploadData$folder,
        Status = ifelse(uploadData$isValid & internalLinkRes, "Pass", "Fail"),
        valRes = valRes,
        InternalLinks = internalLinkRes,
        CreatedOn = uploadData$createdOn,
        LastModified = uploadData$modifiedOn,
        UserModified = uploadData$modifiedUser
      ) %>% arrange(Status)
      
      # render the validation result table
      dbValidation("validation-table", validation_df)
    }
  )
}
