validationTabUI <- function(id) {
  ns <- NS(id)
  div(
    class = "validationTab-container",
    tagList(
      # setTabTitleUI(ns("title")),
      br(),
      dbValidationTableUI(ns("validation-table")),
      helpText("If there is any validation error,
        please re-validate the corresponding metadata to see detailed errors and re-submit once you have corrected metadata.")
    )
  )
}

validationTab <- function(id, metadata, project.name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # render tab title
      # setTabTitle("title", paste0("Validation of Uploaded Data for Project: ", sQuote(project.name)))

      validation_status <- case_when(
        metadata$Result == "valid" & metadata$WarnMsg == "Valid" ~ "Pass",
        metadata$Result == "valid" & metadata$WarnMsg != "Valid" ~ "Warning",
        metadata$ErrorType == "Out of Date" ~ "Warning",
        TRUE ~ "Fail"
      )
      # process validation result
      validation_df <- tibble(
        `Data Type` = paste0(
          '<a href="https://www.synapse.org/#!Synapse:',
          metadata$SynapseID, '" target="_blank">', metadata$Component, "</a>"
        ),
        Dataset = metadata$Folder,
        Validation = validation_status,
        # change wrong schema to out-of-date type
        `Schema Check` = metadata$errorMsg, # should return only first error
        `Internal Links Check` = metadata$WarnMsg,
        `Created On` = metadata$CreatedOn,
        `Last Modified` = metadata$ModifiedOn,
        `User Modified` = metadata$ModifiedUser
      ) %>% arrange(Validation, `Data Type`, Dataset)

      # render the validation result table
      dbValidationTable("validation-table", validation_df)
    }
  )
}