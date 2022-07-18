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

validationTab <- function(id, metadata, project.name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # render tab title
      setTabTitle("title", paste0("Validation of Uploaded Data for Project: ", sQuote(project.name)))

      # change wrong schema to out-of-date type
      schema_status <- case_when(
        metadata$ErrorType == "Wrong Schema" ~ "Out of Date",
        metadata$ErrorType == "No Error" ~ "Valid",
        TRUE ~ metadata$ErrorType
      )
      # process validation result
      validation_df <- tibble(
        `Data Type` = paste0(
          '<a href="https://www.synapse.org/#!Synapse:',
          metadata$SynID, '" target="_blank">', metadata$Component, "</a>"
        ),
        Dataset = metadata$Folder,
        Validation = ifelse(metadata$Result == "valid" & metadata$WarnMsg == "Valid", "Pass", "Fail"),
        # change wrong schema to out-of-date type
        `Schema Check` = schema_status,
        `Internal Links Check` = metadata$WarnMsg,
        `Created On` = metadata$CreatedOn,
        `Last Modified` = metadata$ModifiedOn,
        `User Modified` = metadata$ModifiedUser
      ) %>% arrange(Validation)

      # render the validation result table
      dbValidationTable("validation-table", validation_df)
    }
  )
}