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
    helpText(align = "center", "Usage: mouse over nodes to see the data types, drag nodes to change layout"),
    uiOutput(ns("instruction")),
    forceNetworkOutput(ns("network"), height = height),
    helpText(align = "center", HTML(paste0("A ", icon("long-arrow-alt-right"), " B: Data type A requires Data type B")))
  )
}

dbNetwork <- function(id, uploadData, reqData, selectedDataType) {
  moduleServer(
    id,
    function(input, output, session) {
      output$network <- renderForceNetwork({

        # create input data for network function, forceNetwork
        if (is.null(names(reqData))) {
          # if reqData is a single string of datatype without name, aka no requirements
          # still create data frame for network to prevent breaking the app
          links <- data.frame(source = reqData, target = reqData, value = 5)
          nodes <- data.frame(name = selectedDataType, group = "Selected", size = c(20))
        } else {
          links <- data.frame(
            source = reqData, target = names(reqData),
            value = ifelse(reqData == selectedDataType, 5, 1) # make selected datatype node bigger than others
          )
          nodes <- data.frame(
            name = c(selectedDataType, names(reqData)),
            group = c("Selected", ifelse(links$target %in% uploadData$schema, "Completed", "Missing")),
            size = c(20)
          )
        }

        # convert to numbers starting from 0
        links$IDsource <- match(links$source, nodes$name) - 1
        links$IDtarget <- match(links$target, nodes$name) - 1
        # assign colors to groups
        cols <- 'd3.scaleOrdinal()
                .domain(["Selected", "Completed", "Missing"])
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
    }
  )
}