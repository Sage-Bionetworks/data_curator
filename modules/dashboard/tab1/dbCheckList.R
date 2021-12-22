dbCheckListUI <- function(id) {

  ns <- NS(id)
  
  uiOutput(ns("checklist"))
}

dbCheckList <- function(id, uploadData, reqData) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      all_req <- union(reqData, names(reqData))
      not_up <- setdiff(all_req, uploadData$schema)
      up <- intersect(all_req, uploadData$schema)

      output$checklist <- renderUI({

        tagList(
          helpText(align = "center", 
            HTML(paste0("Click ", icon("folder-open"), " to see the dataset(s) containing the data type"))
          ),
          div(class = "checklist-container",
            div(class = "checklist-item-container",
              if (length(not_up) > 0) {
                  lapply(not_up, function(name) {
                    div(class = "checklist-item", 
                      tagList(
                        icon("circle", class = "missing"),
                        span(name)
                      )
                    )
                  })
              },
              if (length(up) > 0) {
                lapply(up, function(name) {
                  dups_inx <- which(uploadData$schema == name)
                  div(class = "checklist-item",
                    tagList(
                      icon("circle", class = "completed"),
                      span(name),
                      actionButton(class = "icon-btn", ns(paste0(name, "-dropdown")),
                        tagList(
                          # create custom drop down to show all available datasets that have this datatype
                          icon("folder-open"),
                          div(class = "dropdown-list", id = ns(paste0(name, "-dropdown-item")),
                            lapply(dups_inx, function(i) {
                              tags$a(uploadData$folder[i], target = "_blank", 
                                href = paste0("https://www.synapse.org/#!Synapse:", uploadData$synID[i]))
                            })
                          )
                        )
                      )
                    )
                  )
                })
              }
            )
          )
        )
      })
      
      # detect folder icon btn changes to show/hide dropdown
      lapply(up, function(name) {
        dropdown_id <- paste0(name, "-dropdown")
        dropdown_item_id <- paste0(name, "-dropdown-item")
        observeEvent(input[[dropdown_id]], {
          addClass(dropdown_item_id, "open-dropdown")
          onevent("mouseleave", dropdown_id, removeClass(dropdown_item_id, "open-dropdown"))
        })
      })
    }
  )
}
