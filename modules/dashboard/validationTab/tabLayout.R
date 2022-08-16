validationTabUI <- function(id) {
  ns <- NS(id)
  div(
    class = "validationTab-container",
    tagList(
      setTabTitleUI(ns("title")),
      dbValidationTableUI(ns("validation-table")),
      helpText("If there is any validation error,
        please re-validate the corresponding metadata to see detailed errors and re-submit once you have corrected metadata.")
    )
  )
}

validationTab <- function(id, uploadData, selectedProject, config=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # render tab title
      setTabTitle("title", paste0("Validation of Uploaded Data for Project: ", sQuote(selectedProject)))

      # change wrong schema to out-of-date type
      schema_status <- case_when(
        uploadData$ErrorType == "Wrong Schema" ~ "Out of Date",
        uploadData$ErrorType == "No Error" ~ "Valid",
        TRUE ~ uploadData$ErrorType
      )
      # process validation result
      validation_df <- tibble(
        `Data Type` = paste0(
          '<a href="https://www.synapse.org/#!Synapse:',
          uploadData$SynapseID, '" target="_blank">', uploadData$Component, "</a>"
        ),
        Dataset = uploadData$Folder,
        Validation = ifelse(uploadData$Result == "valid" & uploadData$WarnMsg == "Valid", "Pass", "Fail"),
        # change wrong schema to out-of-date type
        `Schema Check` = schema_status,
        `Internal Links Check` = uploadData$WarnMsg,
        `Created On` = uploadData$CreatedOn,
        `Last Modified` = uploadData$ModifiedOn,
        `User Modified` = uploadData$ModifiedUser
      ) %>% arrange(Validation)

      # render the validation result table
      dbValidationTable("validation-table", validation_df, config = config)
    }
  )
}