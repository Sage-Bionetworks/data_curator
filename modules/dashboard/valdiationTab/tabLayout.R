validationTabUI <- function(id) {

  ns <- NS(id)
  div(class="validationTab-container",
    tagList(
      setTabTitleUI(ns("title")),
      dbValidationTableUI(ns("validation-table")),
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
      setTabTitle("title", paste0("Validation of Uploaded Data for Project: ", sQuote(selectedProject)))

      # TODO: add real internal links results
      internalLink <- sample(c("valid", "invalid"), nrow(uploadData), replace = TRUE, prob = c(4, 1))
      isPassInternalLink <- ifelse(internalLink == "valid", TRUE, FALSE)
      modelValidationRes <- ifelse(uploadData$errorType == "Wrong Schema", "Out of Date", 
        ifelse(uploadData$errorType == "No Error", "Valid", uploadData$errorType))

      # process validation result
      validation_df <- tibble(
        `Data Type` = paste0(
          '<a href="https://www.synapse.org/#!Synapse:',
          uploadData$synID, '" target="_blank">', uploadData$schema, "</a>"
        ),
        Dataset= uploadData$folder,
        Validation = ifelse(uploadData$isValid & isPassInternalLink, "Pass", "Fail"),
        `Schema Check` = modelValidationRes,
        `Internal Links Check` = isPassInternalLink,
        `Created On` = uploadData$createdOn,
        `Last Modified` = uploadData$modifiedOn,
        `User Modified` = uploadData$modifiedUser
      ) %>% arrange(Validation)
      
      # render the validation result table
      dbValidationTable("validation-table", validation_df)
    }
  )
}
