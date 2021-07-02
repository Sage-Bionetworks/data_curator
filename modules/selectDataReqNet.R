# This is the module to

selectDataReqNetUI <- function(id, height = 500) {

  # namespace
  ns <- NS(id)
  forceNetworkOutput(ns("network"), height = height)
}

selectDataReqNetServer <- function(id, upload_data, req_data, selected_manifest) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$network <- renderForceNetwork({

        # wait input schema name finish updating !important
        reqData <- isolate(req_data)
        upData <- isolate(upload_data)
        selected <- isolate(selected_manifest)

        # reorder to make selected manifest always on the left in plot
        inx <- which(reqData == selected)
        reqData <- c(reqData[inx], reqData[-inx])

        if (is.null(names(reqData))) {
          links <- data.frame(source = reqData, target = reqData, value = 5)
          nodes <- data.frame(name = selected, group = "Selected Template", size = c(20))
        } else {
          links <- data.frame(source = reqData, target = names(reqData), 
                              value = ifelse(reqData == selected, 5, 1))
          nodes <- data.frame(name = c(selected, links$target),
                              group = c("Selected Template", ifelse(links$target %in% upData$schema, "Uploaded", "Not Uploaded")),
                              size = c(20))
        }
        links$IDsource <- match(links$source, nodes$name)-1 
        links$IDtarget <- match(links$target, nodes$name)-1
        cols <- 'd3.scaleOrdinal()
                .domain(["Selected Template", "Uploaded", "Not Uploaded"])
                .range(["#694489", "#28a745", "#E53935"]);'

        forceNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget",
                    Group = "group", Value = "value", NodeID = "name", Nodesize = "size",
                    linkColour = ifelse(links$IDsource == 5, "#694489", "black"),
                    colourScale = JS(cols), legend = TRUE, linkDistance = 40,
                    zoom = FALSE, bounded=TRUE, arrows = TRUE, 
                    opacity = 0.9, fontSize=16, charge = -500)
      })
    }
  )
}
