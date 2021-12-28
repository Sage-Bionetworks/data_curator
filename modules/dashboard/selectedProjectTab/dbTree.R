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


dbTreeUI <- function(id) {

  ns <- NS(id)

  legend_cols <- c("#694489", "#A287AF", "#FF794A", "#28a745", "#E53935")
  legend_names <- c("Selected Project", "Completed Datasets", "Incompleted Datasets", "Uploaded Data", "Missing")

  div(class="collapsibleTree-container",
    tagList(
      div(class = "legend-container",
        lapply(seq_along(legend_cols), function(i) {
          tags$i(class = "legend", HTML(nodeSVG(legend_cols[i])), span(legend_names[i]))
        })
      ),
      # r2d3 height can only be adjusted in d3output
      # but the height needs to be adjusted after the number of folders is known
      # so move d3output to server function
      uiOutput(ns("tree-container")),
      helpText(HTML(paste0(
        '
        Click nodes to expand the data requirements for the selected project. 
        Click ', nodeSVG("#FF794A"), 'nodes to see which data requirement is missing. 
        If datasets have been uploaded, you will see ', nodeSVG("#A287AF"), ' nodes.
        '
      )))
    )
  )
}

dbTree <- function(id, uploadData, reqData, selectedProject) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      height <- "200px"
      tree_list <- NULL

      if (length(uploadData) != 0) {
        
        # get folder list
        folder_list <- sort(unique(reqData$folder))
        # change d3 tree height based on how many nodes
        height <- paste0(36 * length(folder_list), "px")
        # remove project name in children to trim long names
        pattern <- paste0(str_replace(selectedProject, " ", "_"), "_")
        # get dataset contains any missing requirement children
        incompleted_ds <- reqData %>%
          distinct(folderSynId, .keep_all = TRUE) %>%
          filter(nMiss > 0)
        # make nodes data from project to datasets
        project_to_dataset <- data.frame(
          from = c(selectedProject),
          to = paste0("f:", folder_list),
          node_opacity = c(0),
          node_color = ifelse(folder_list %in% incompleted_ds$folder, "#FF794A", "#A287AF")
        )
        # make nodes data from datasets to their requirements
        dataset_to_req <- reqData %>%
          mutate(
            node_opacity = if_else(reqData$to %in% reqData$from, 0, 1),
            node_color = if_else(reqData$to %in% uploadData$schema, "#28a745", "#E53935")
          ) %>%
          select(from, to, node_opacity, node_color)
        tree_df <- rbind(project_to_dataset, dataset_to_req) %>% 
          mutate_at(1:2, ~ gsub(pattern, "", .)) %>% 
          distinct() # remove duplicated rows to save conversion time
        # convert to tree list using `data.tree`
        tree_list <- data.tree::FromDataFrameNetwork(tree_df)
        # tree_list$Set(group = ifelse(tree_list$Get("name") %in% c(upData$folder, upData$schema), "upload", "not_load"))
        tree_list$node_opacity <- 1
        tree_list$node_color <- "#694489"
        tree_list <- as.list(tree_list, mode = "explicit", unname = TRUE)
      }

      output$`tree-container` <- renderUI({
        d3Output(ns("tree"), width = "100%", height = height)
      })

      output$tree <- renderD3({
        r2d3(
          # if need css, use e.g. css = sass(sass_import("www/scss/basic/collapsibleTree"))
          data = tree_list, d3_version = "4", script = "www/js/collapsibleTree.js"
        )
      })
    }
  )
}
