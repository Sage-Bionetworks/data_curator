#' create reactive network to show relationship between selected datatype and its requirements
#' create progress bar under the network to show ingress progress percentage
#' 
#' @param id id name of this module
#' @param height height in px of network container, 500px by default
#' @param uploadData output from \code{getManifests}
#' @param reqData output from \code{getDatatypeRequirement}
#' @param selectedDataType the selected data type/schema name of template dropdown
#' @return reactive network and progress bar
dbNetworkUI <- function(id, height = 500) {

  ns <- NS(id)
  tagList(
    uiOutput(ns("instruction")),
    forceNetworkOutput(ns("network"), height = height),
    progressBarUI(ns("pb"))
  )
}

dbNetwork <- function(id, uploadData, reqData, selectedDataType) {
  moduleServer(
    id,
    function(input, output, session) {

      # add extra legends manually
      output$instruction <- renderUI({
        tagList(
          div(
            style = "display: flex; flex-direction: column; margin-left: 15px; font-family: serif; font-size: 14px",
            span("Usage: mouse over nodes to see the data types, drag nodes to change layout"),
            span(HTML(paste0("A ", icon("long-arrow-right"), " B: Data type A requires Data type B")))
          )
        )
      })
 
      output$network <- renderForceNetwork({

        # create input data for network function, forceNetwork
        if (is.null(names(reqData))) {
          # if reqData is a single string of datatype without name, aka no requirements
          # still create data frame for network to prevent breaking the app
          links <- data.frame(source = reqData, target = reqData, value = 5)
          nodes <- data.frame(name = selectedDataType, group = "Selected Data Type", size = c(20))
        } else {
          links <- data.frame(
            source = reqData, target = names(reqData),
            value = ifelse(reqData == selectedDataType, 5, 1) # make selected datatype node bigger than others
          )
          nodes <- data.frame(
            name = c(selectedDataType, links$target),
            group = c("Selected Data Type", ifelse(links$target %in% uploadData$schema, "Uploaded Data", "Missing")),
            size = c(20)
          )
        }
        # convert to numbers starting from 0
        links$IDsource <- match(links$source, nodes$name) - 1
        links$IDtarget <- match(links$target, nodes$name) - 1
        # assign colors to groups
        cols <- 'd3.scaleOrdinal()
                .domain(["Selected Data Type", "Uploaded Data", "Missing"])
                .range(["#694489", "#28a745", "#E53935"]);'

        # render network
        forceNetwork(
          Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget",
          Group = "group", Value = "value", NodeID = "name", Nodesize = "size",
          linkColour = ifelse(links$IDsource == 5, "#694489", "black"),
          colourScale = JS(cols), legend = TRUE, linkDistance = 40,
          zoom = FALSE, bounded = TRUE, arrows = TRUE,
          opacity = 0.9, fontSize = 16, charge = -500
        )
      })

      # progress bar section
      all_req <- union(reqData, names(reqData)) # number of total requirements
      pb_pct <- round(length(intersect(all_req, uploadData$schema)) / length(all_req), 2) * 100
      progressBarServer(
        id = "pb",
        value = pb_pct, 
        title = "Uploading progress for required data",
        subtitle = "( progress % = # of total uploaded data / # of total required data )"
      )
    }
  )
}
