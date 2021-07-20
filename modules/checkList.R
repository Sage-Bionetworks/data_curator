# This is the module to

checkListUI <- function(id) {

  # namespace
  ns <- NS(id)
  tagList(
    uiOutput(ns("checklist"))
  )
}

checkListServer <- function(id, upload_data, req_data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$checklist <- renderUI({

        reqData <- isolate(req_data)
        upData <- isolate(upload_data)
        all_req <- union(reqData, names(reqData))
        not_up <- setdiff(all_req, upData$schema)
        up <- intersect(all_req, upData$schema)
        
        div(class = "checklist-container",
          div(class = "checklist-header",
            p(class = "checklist-title", "Requirement List"),
          ),
          div(class = "checklist-content",
            div(class = "checklist-item",
              span(class = "checklist-subtitle", "Not Yet"),
              div(class = "checklist-data",
                if(length(not_up) > 0) {
                  lapply(not_up, function(name) {
                    div(class = "checklist-icon",
                      name, tags$span(class = "error_msg", icon("circle-o"))
                    )
                  })
                }
              )
            ),
            div(class = "checklist-item",
              span(class = "checklist-subtitle", "Uploaded"),
              div(class = "checklist-data",
                if(length(up) > 0) {
                  lapply(up, function(name) {
                    synID <- upData$synID[upData$schema == name]

                    if (length(synID) > 1) notif_icon <- icon("lightbulb") else notif_icon <- NULL
                    div(class = "checklist-icon",
                      name, lapply(synID, function(id) 
                      tags$a("", icon("link"), target = '_blank', href = paste0("https://www.synapse.org/#!Synapse:", id))),
                      tags$span(notif_icon, tags$span(class = "success_msg", icon("check-circle")))
                    )
                  })
                }
              )
            )
          ),
          if (anyDuplicated(upData$schema) != 0) helpText(icon("lightbulb"), "multiple manifests with the same component are detected")
        )
      })
    }
  )
}