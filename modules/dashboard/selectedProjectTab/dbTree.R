nodeSVG <- function(color) {
  HTML(paste0(
    '
    <svg width="22" height="22">
      <circle class="node" cx="11" cy="11" r="6" cursor="pointer"
       style="fill: #fff; stroke: ', color, '; stroke-width: 5px;"></circle>
    </svg>
    '
  ))
}


dbTreeUI <- function(id, n.nodes = NULL) {
  ns <- NS(id)

  legend_cols <- c("#694489", "#A287AF", "#FF794A", "#28a745", "#E53935")
  legend_names <- c("Selected Project", "Completed Datasets", "Incompleted Datasets", "Uploaded Data", "Missing")
  height <- paste0(max(200, 36 * n.nodes), "px")
  div(
    class = "collapsibleTree-container",
    tagList(
      div(
        class = "legend-container",
        lapply(seq_along(legend_cols), function(i) {
          tags$i(class = "legend", HTML(nodeSVG(legend_cols[i])), span(legend_names[i]))
        })
      ),
      # r2d3 height can only be adjusted in d3output after the number of folders is known
      d3Output(ns("tree"), width = "100%", height = height),
      helpText(HTML(paste0(
        "
        Click nodes to expand the data requirements for the selected project.
        Click ", nodeSVG("#FF794A"), "nodes to see which data requirement is missing.
        If datasets have been uploaded, you will see ", nodeSVG("#A287AF"), " nodes.
        "
      )))
    )
  )
}

dbTree <- function(id, metadata, nodes, project.name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tree_list <- NULL

      if (length(metadata$Component) != 0) {

        # get folder list
        folder_list <- sort(unique(nodes$folder))
        # change d3 tree height based on how many nodes
        height <- paste0(36 * length(folder_list), "px")
        # remove project name in children to trim long names
        pattern <- paste0(str_replace(project.name, " ", "_"), "_")
        # get dataset contains any missing requirement children
        incompleted_ds <- nodes %>%
          distinct(folder_id, .keep_all = TRUE) %>%
          filter(n_miss > 0)

        # make nodes data frame: from project to dataset to required data type
        project_to_dataset <- data.frame(
          from = c(project.name),
          to = paste0("f:", folder_list),
          font_opacity = c(0),
          node_color = ifelse(folder_list %in% incompleted_ds$folder, "#FF794A", "#A287AF")
        )
        # make nodes data from datasets to their requirements
        dataset_to_req <- nodes %>%
          mutate(
            font_opacity = if_else(nodes$to %in% nodes$from, 0, 1),
            node_color = if_else(nodes$to %in% metadata$Component, "#28a745", "#E53935")
          ) %>%
          select(from, to, font_opacity, node_color)
        tree_df <- rbind(project_to_dataset, dataset_to_req) %>%
          mutate_at(1:2, ~ gsub(pattern, "", .)) %>%
          distinct() # remove duplicated rows to save conversion time
        # convert to tree list using `data.tree`
        tree_list <- data.tree::FromDataFrameNetwork(tree_df)
        # tree_list$Set(group = ifelse(tree_list$Get("name") %in% c(upData$folder, upData$schema), "upload", "not_load"))
        tree_list$font_opacity <- 1
        tree_list$node_color <- "#694489"
        tree_list <- as.list(tree_list, mode = "explicit", unname = TRUE)
      }


      output$tree <- renderD3({
        r2d3(
          # if need css, use e.g. css = sass(sass_import("www/scss/basic/collapsibleTree"))
          data = tree_list, d3_version = "4",
          options = list(height = height),
          script = "www/js/collapsibleTree.js"
        )
      })
    }
  )
}