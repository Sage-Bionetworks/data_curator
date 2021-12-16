dbNetworkUI <- function(id, height = 500) {

  # namespace
  ns <- NS(id)
  tagList(
    uiOutput(ns("instruction")),
    forceNetworkOutput(ns("network"), height = height),
    progressBarUI(ns("pb"))
  )
}

dbNetwork <- function(id, upload_data, req_data, selected_manifest) {
  moduleServer(
    id,
    function(input, output, session) {
      reqData <- isolate(req_data)
      upData <- isolate(upload_data)
      selected <- isolate(selected_manifest)

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

        # reorder to make selected manifest always on the left in plot
        inx <- which(reqData == selected)
        reqData <- c(reqData[inx], reqData[-inx])

        if (is.null(names(reqData))) {
          links <- data.frame(source = reqData, target = reqData, value = 5)
          nodes <- data.frame(name = selected, group = "Selected Data Type", size = c(20))
        } else {
          links <- data.frame(
            source = reqData, target = names(reqData),
            value = ifelse(reqData == selected, 5, 1)
          )
          nodes <- data.frame(
            name = c(selected, links$target),
            group = c("Selected Data Type", ifelse(links$target %in% upData$schema, "Uploaded Data", "Missing")),
            size = c(20)
          )
        }
        links$IDsource <- match(links$source, nodes$name) - 1
        links$IDtarget <- match(links$target, nodes$name) - 1
        cols <- 'd3.scaleOrdinal()
                .domain(["Selected Data Type", "Uploaded Data", "Missing"])
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
      progressBarServer(
        id = "pb",
        value = pb_pct, 
        title = "Uploading progress for required data",
        subtitle = "( progress % = the total number of uploaded data / the total number of required data )"
      )
    }
  )
}
