#' create reactive network to show relationship between selected datatype and its requirements
#' create progress bar under the network to show ingress progress percentage
#'
#' @param id id name of this module
#' @param height height in px of network container, 500px by default
dbNetworkUI <- function(id, height = 500) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("instruction")),
    forceNetworkOutput(ns("network"), height = height)
  )
}

#' dbNetwork Module - Server
#'
#' @param id id name of this module
#' @param metadata output from \code{get_dataset_metadata}
#' @param nodes output from \code{get_schema_nodes_df}
#' @param schema the selected data type/schema name of template dropdown
#' @return reactive network and progress bar
dbNetwork <- function(id, metadata, nodes, schema) {
  moduleServer(
    id,
    function(input, output, session) {
      output$network <- renderForceNetwork({

        # create input data for network function, forceNetwork
        if (is.null(names(nodes))) {
          # if nodes is a single string of datatype without name, aka no requirements
          # still create data frame for network to prevent breaking the app
          links <- data.frame(source = nodes, target = nodes, value = 5)
          nodes_df <- data.frame(name = schema, group = "Selected", size = c(20))
        } else {
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
        }

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
  )
}