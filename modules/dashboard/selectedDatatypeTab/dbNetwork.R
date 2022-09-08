dbNetworkUI <- function(id, height = 500) {
  ns <- NS(id)
  tagList(
    uiOutput(class = "bold-normal", ns("no-requirement-text")),
    forceNetworkOutput(ns("network"), height = height)
  )
}

dbNetwork <- function(id, metadata, nodes, schema) {
  moduleServer(
    id,
    function(input, output, session) {
      if (is.null(names(nodes))) {
        # if nodes is a single string of datatype without name, aka no requirements
        output$`no-requirement-text` <- renderUI(
          h3(paste0("It looks like ", sQuote(toString(nodes)), " has no requirements"))
        )
      } else {
        output$network <- renderForceNetwork({
          # create input data for network function, forceNetwork
          links <- data.frame(
            source = nodes, target = names(nodes),
            value = ifelse(nodes == schema, 5, 1) # make selected datatype node bigger than others
          )
          nodes_df <- data.frame(
            name = c(schema, names(nodes)),
            group = c("Selected", ifelse(links$target %in% metadata$Component, "Uploaded", "Missing")),
            size = c(20)
          ) %>%
            distinct() # remove duplicates requirement


          # convert to numbers starting from 0
          links$IDsource <- match(links$source, nodes_df$name) - 1
          links$IDtarget <- match(links$target, nodes_df$name) - 1
          # assign colors to groups
          cols <- 'd3.scaleOrdinal()
                .domain(["Selected", "Uploaded", "Missing"])
                .range(["#694489", "#28a745", "#E53935"]);'
          # render network
          forceNetwork(
            Links = links, Nodes = nodes_df, Source = "IDsource", Target = "IDtarget",
            Group = "group", Value = "value", NodeID = "name", Nodesize = "size",
            linkColour = if_else(links$IDsource == 5, "#694489", "black"),
            colourScale = JS(cols), legend = TRUE, linkDistance = 40,
            zoom = FALSE, bounded = TRUE, arrows = TRUE,
            opacity = 0.9, fontFamily = "inherit", charge = -500
          )
          # htmlwidgets::onRender(
          #   network,
          #   '
          #   function(el) {
          #     d3.select(el).selectAll(".node text").attr("font-family", "inherit");
          #   }
          #   '
          # )
        })
      }
    }
  )
}