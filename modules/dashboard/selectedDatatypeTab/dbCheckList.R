dbCheckListUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("checklist"))
}

dbCheckList <- function(id, metadata, nodes) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      all_req <- union(nodes, names(nodes))
      not_up <- setdiff(all_req, metadata$Component)
      up <- intersect(all_req, metadata$Component)

      output$checklist <- renderUI({
        tagList(
          helpText(
            align = "center",
            HTML(paste0("Click ", icon("folder-open"), " to see the dataset(s) containing the data type"))
          ),
          div(
            class = "checklist-container",
            div(
              class = "checklist-item-container",
              if (length(not_up) > 0) {
                lapply(not_up, function(name) {
                  div(
                    class = "checklist-item",
                    tagList(
                      icon("circle", class = "missing"),
                      span(name)
                    )
                  )
                })
              },
              if (length(up) > 0) {
                lapply(up, function(name) {
                  dups_inx <- which(metadata$Component == name)
                  div(
                    class = "checklist-item",
                    tagList(
                      icon("circle", class = "completed"),
                      span(name),
                      actionButton(
                        class = "icon-btn", ns(paste0(name, "-dropdown")),
                        tagList(
                          # create custom drop down to show all available datasets that have this datatype
                          icon("folder-open"),
                          div(
                            class = "dropdown-list", id = ns(paste0(name, "-dropdown-item")),
                            lapply(dups_inx, function(i) {
                              tags$a(metadata$Folder[i],
                                target = "_blank",
                                href = paste0("https://www.synapse.org/#!Synapse:", metadata$SynapseID[i])
                              )
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