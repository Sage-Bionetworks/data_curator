# This is the module to

progressChecklistUI <- function(id) {

  # namespace
  ns <- NS(id)
  uiOutput(ns("checklist"))
}

progressChecklistServer <- function(id, upload_data, req_data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$checklist <- renderUI({

        reqData <- isolate(req_data)
        upData <- isolate(upload_data)
        
        all_req <- union(reqData, names(reqData))
        not_up <- setdiff(all_req, upData$schema)
        up <- intersect(all_req, upData$schema)

        div(
          class = "checklist",
          div(
            class = "checklist-container",
            tagList(
              p(class = "checklist-title", "Requirement List"),
              div(
                class = "checklist-content",
                div(
                  class = "checklist-item",
                  span(class = "checklist-subtitle", "Not Yet"),
                  div(
                    class = "checklist-data",
                    if(length(not_up) > 0) {
                      lapply(not_up, function(name) {
                        tags$span(class = "far fa-circle error_msg", HTML(name))
                      })
                    }
                  ),
                ),
                div(
                  class = "checklist-item",
                  span(class = "checklist-subtitle", "Uploaded"),
                  div(
                    class = "checklist-data",
                    if(length(up) > 0) {
                      lapply(up, function(name) {
                        # some projects have multiple manifest, takes 1st one just in case
                        synID <- upData$synID[upData$schema == name][1]
                        # if (name %in% outdate) {
                        #   tags$a(
                        #     name, class = "far exclamation-circle process_msg", target='_blank',
                        #     href=paste0("https://www.synapse.org/#!Synapse:", synID)
                        #     )
                        # } else {
                        tags$a(
                          name, class = "far fa-check-circle success_msg", target='_blank',
                          href=paste0("https://www.synapse.org/#!Synapse:", synID)
                        )
                        # }
                      })
                    }
                  )
                )
              ),
              shinyWidgets::progressBar(
                id = NS(id, "pb"), status = "danger", striped = TRUE, display_pct = TRUE,
                value = round(length(intersect(upData$schema, all_req)) / length(all_req), 2) * 100
              )
            )
          )
        )
      })
    }
  )
}
