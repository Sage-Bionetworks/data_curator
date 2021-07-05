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
                    span(name, tags$i(class = "error_msg", icon("circle-o")))
                  })
                }
              )
            ),
            div(class = "checklist-item",
              span(class = "checklist-subtitle", "Uploaded"),
              div(class = "checklist-data",
                if(length(up) > 0) {
                  lapply(up, function(name) {
                    # some projects have multiple manifest, takes 1st one just in case
                    synID <- upData$synID[upData$schema == name][1]
                    tags$a(
                      name, target = '_blank', href = paste0("https://www.synapse.org/#!Synapse:", synID),
                      tags$i(class = "success_msg", icon("check-circle"))
                    )
                  })
                }
              )
            )
          )
        )
      })
    }
  )
}
