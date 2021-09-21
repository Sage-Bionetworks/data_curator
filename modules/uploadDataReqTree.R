# This is the module to
# , width = "100%", height = "100%"
uploadDataReqTreeUI <- function(id, width = "100%", height = "400px") {

  # namespace
  ns <- NS(id)
  tagList(
    d3Output(ns("tree"), width = width, height = height),
    helpText(
      tags$i(style = "color: grey; margin-right: 5px", icon("circle"), "Selected Project"),
      tags$i(style = "color: #694489; margin-right: 5px", icon("circle"), "Datasets complete all metadata"),
      tags$i(style = "color: #F7DC6F; margin-right: 5px", icon("circle"), "Datasets not complete all metadata"),
      tags$i(style = "color: #28a745; margin-right: 5px", icon("circle"), "Uploaded Metadata"),
      tags$i(style = "color: #E53935;", icon("circle"), "Missing"),
      br(),
      "Click nodes to expand the required manifests"
    )
  )
}

uploadDataReqTreeServer <- function(id, upload_data, upload_require_data, project_name) {
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
            node_color = ifelse(upData$folder %in% file_has_miss$folder, "#F7DC6F", "#694489")
          )

          reqData <-
            reqData %>%
            mutate(node_color = if_else(reqData$to %in% upData$schema, "#28a745", "#E53935")) %>%
            select(from, to, node_color)

          tree_df <- rbind(project_df, reqData) %>% mutate_at(1:2, ~ gsub(pattern, "", .))
          # convert to list (name; children) using `data.tree`
          tree_list <- data.tree::FromDataFrameNetwork(tree_df)
          # tree_list$Set(group = ifelse(tree_list$Get("name") %in% c(upData$folder, upData$schema), "upload", "not_load"))
          tree_list$node_color <- "grey"
          tree_list <- as.list(tree_list, mode = "explicit", unname = TRUE)
        }

        r2d3(
          data = tree_list, d3_version = "4",
          script = "www/js/collapsibleTree.js", css = sass(sass_import("www/scss/basic/collapsibleTree"))
        )
      })
    }
  )
}
