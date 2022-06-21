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

validationTab <- function(id, uploadData, selectedProject, config) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # render tab title
      setTabTitle("title", paste0("Validation of Uploaded Data for Project: ", sQuote(selectedProject)))

      # change wrong schema to out-of-date type
      schema_status <- case_when(
        uploadData$errorType == "Wrong Schema" ~ "Out of Date",
        uploadData$errorType == "No Error" ~ "Valid",
        TRUE ~ uploadData$errorType
      )
      # process validation result
      validation_df <- tibble(
        `Data Type` = paste0(
          '<a href="https://www.synapse.org/#!Synapse:',
          uploadData$synID, '" target="_blank">', uploadData$schema, "</a>"
        ),
        Dataset = uploadData$folder,
        Validation = ifelse(uploadData$result == "valid" & uploadData$warnMsg == "Valid", "Pass", "Fail"),
        # change wrong schema to out-of-date type
        `Schema Check` = schema_status,
        `Internal Links Check` = uploadData$warnMsg,
        `Created On` = uploadData$createdOn,
        `Last Modified` = uploadData$modifiedOn,
        `User Modified` = uploadData$modifiedUser
      ) %>% arrange(Validation)

      # render the validation result table
      dbValidationTable("validation-table", validation_df, config = config)
    }
  )
}