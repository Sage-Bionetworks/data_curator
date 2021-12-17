
dbTreeUI <- function(id, width = "100%", height = "500px") {

  ns <- NS(id)
  tagList(
    helpText(
      HTML(paste0(
        "Click nodes to expand the data requirements for the selected project. If a node is yellow, click it to see which data requirement is missing.<br>
        If a dataset has been uploaded, the node is green. Please see validation details in the <code>Data Validation</code> tab to check if your uploaded data meets the validation requirements."
      ))
    ),
    d3Output(ns("tree"), width = width, height = height)
  )
}

dbTree <- function(id, upload_data, upload_require_data, project_name) {
  moduleServer(
    id,
    function(input, output, session) {
      output$tree <- renderD3({
        upData <- isolate(upload_data)
        reqData <- isolate(upload_require_data)

        if (length(upData) == 0) {
          tree_list <- NULL
        } else {

          # remove project name in children to trim long names
          pattern <- paste0(str_replace(project_name, " ", "_"), "_")
          n_file <- length(upData$folder)

          # has_miss: dataset contains any missing requirement children
          file_has_miss <- reqData[, c("folder", "has_miss")] %>%
            distinct() %>%
            filter(has_miss)
          project_df <- data.frame(
            from = rep(project_name, n_file),
            to = upData$folder,
            node_color = ifelse(upData$folder %in% file_has_miss$folder, "#FF794A", "#A287AF")
          )

          reqData <-
            reqData %>%
            mutate(node_color = if_else(reqData$to %in% upData$schema, "#28a745", "#E53935")) %>%
            select(from, to, node_color)

          tree_df <- rbind(project_df, reqData) %>% mutate_at(1:2, ~ gsub(pattern, "", .))
          # convert to list (name; children) using `data.tree`
          tree_list <- data.tree::FromDataFrameNetwork(tree_df)
          # tree_list$Set(group = ifelse(tree_list$Get("name") %in% c(upData$folder, upData$schema), "upload", "not_load"))
          tree_list$node_color <- "#694489"
          tree_list <- as.list(tree_list, mode = "explicit", unname = TRUE)
        }

        # make color list for legends
        legend <- data.frame(
          col = c("#694489", "#A287AF", "#FF794A", "#28a745", "#E53935"), 
          name = c("Selected Project", "Completed Datasets", "Incompleted Datasets", "Uploaded Data", "Missing")
        )

        r2d3(
          # if need css, use e.g. css = sass(sass_import("www/scss/basic/collapsibleTree"))
          data = tree_list, options = list(legend = legend), d3_version = "4", script = "www/js/collapsibleTree.js"
        )
      })
    }
  )
}
