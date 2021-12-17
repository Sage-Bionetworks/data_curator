# This is the module to

dbChecklistUI <- function(id) {

  # namespace
  ns <- NS(id)
  tagList(
    uiOutput(ns("checklist"))
  )
}

dbChecklist <- function(id, uploadData, reqData, config) {
  moduleServer(
    id,
    function(input, output, session) {
      output$checklist <- renderUI({

        all_req <- union(reqData, names(reqData))
        not_up <- setdiff(all_req, uploadData$schema)
        up <- intersect(all_req, uploadData$schema)

        div(
          class = "checklist-container",
          div(
            class = "checklist-header",
            p(class = "checklist-title", "Required Datasets"),
          ),
          div(
            class = "checklist-content",
            div(
              class = "checklist-item",
              span(class = "checklist-subtitle upload", "Uploaded"),
              div(
                class = "checklist-data",
                if (length(up) > 0) {
                  lapply(up, function(name) {
                    synID <- uploadData$synID[uploadData$schema == name]
                    if (length(synID) > 1) dup_icon <- icon("lightbulb") else dup_icon <- NULL
                    div(
                      class = "checklist-icon",
                      name, lapply(synID, function(id) {
                        tags$a("", icon("link"), target = "_blank", href = paste0("https://www.synapse.org/#!Synapse:", id))
                      }),
                      tags$span(dup_icon, tags$span(class = "success_msg", icon("check-circle")))
                    )
                  })
                }
              )
            ),
            div(
              class = "checklist-item",
              span(class = "checklist-subtitle", "Not Yet"),
              div(
                class = "checklist-data",
                if (length(not_up) > 0) {
                  lapply(not_up, function(name) {
                    # type <- config$type[match(name, config$schema_name)]
                    # if (type == "assay") type_icon <- icon("lightbulb") else datatype_icon <- NULL
                    div(
                      class = "checklist-icon",
                      name, tags$span(class = "error_msg", icon("circle-o"))
                    )
                  })
                }
              )
            )
          ),
          # helpText("Please upload the data and metadata")
          if (anyDuplicated(uploadData$schema) != 0) helpText(icon("lightbulb"), "multiple datasets with the same data type are detected")
        )
      })
    }
  )
}
