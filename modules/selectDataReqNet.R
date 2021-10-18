# This is the module to

selectDataReqNetUI <- function(id, height = 500) {

  # namespace
  ns <- NS(id)
  tagList(
    uiOutput(ns("arrow")),
    forceNetworkOutput(ns("network"), height = height),
    uiOutput(ns("pbOut"))
  )
}

selectDataReqNetServer <- function(id, upload_data, req_data, selected_manifest) {
  moduleServer(
    id,
    function(input, output, session) {
      reqData <- isolate(req_data)
      upData <- isolate(upload_data)
      selected <- isolate(selected_manifest)

      output$arrow <- renderUI({
        span(
          HTML(str_c("A", icon("long-arrow-right"), "B: Metadata A requires Metadata B"), collapse = " "), 
          style = "margin-left: 15px; font-family: serif; font-size: 14px"
        )
      })
 
      output$network <- renderForceNetwork({

        # reorder to make selected manifest always on the left in plot
        inx <- which(reqData == selected)
        reqData <- c(reqData[inx], reqData[-inx])

        if (is.null(names(reqData))) {
          links <- data.frame(source = reqData, target = reqData, value = 5)
          nodes <- data.frame(name = selected, group = "Selected Datatype", size = c(20))
        } else {
          links <- data.frame(
            source = reqData, target = names(reqData),
            value = ifelse(reqData == selected, 5, 1)
          )
          nodes <- data.frame(
            name = c(selected, links$target),
            group = c("Selected Datatype", ifelse(links$target %in% upData$schema, "Uploaded Metadata", "Missing")),
            size = c(20)
          )
        }
        links$IDsource <- match(links$source, nodes$name) - 1
        links$IDtarget <- match(links$target, nodes$name) - 1
        cols <- 'd3.scaleOrdinal()
                .domain(["Selected Datatype", "Uploaded Metadata", "Missing"])
                .range(["#694489", "#28a745", "#E53935"]);'

        forceNetwork(
          Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget",
          Group = "group", Value = "value", NodeID = "name", Nodesize = "size",
          linkColour = ifelse(links$IDsource == 5, "#694489", "black"),
          colourScale = JS(cols), legend = TRUE, linkDistance = 40,
          zoom = FALSE, bounded = TRUE, arrows = TRUE,
          opacity = 0.9, fontSize = 16, charge = -500
        )
      })

      all_req <- union(reqData, names(reqData))
      pb_pct <- round(length(intersect(all_req, upData$schema)) / length(all_req), 2) * 100
      output$pbOut <- renderUI(
        shinyWidgets::progressBar(
          id = NS(id, "pb"), status = "danger", title = "uploading progress of required metadata",
          striped = TRUE, display_pct = TRUE, value = pb_pct
        )
      )
    }
  )
}
