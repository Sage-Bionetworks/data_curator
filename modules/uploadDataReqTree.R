# This is the module to
# , width = "100%", height = "100%"
uploadDataReqTreeUI <- function(id, width="100%", height="400px") {

  # namespace
  ns <- NS(id)
  tagList(
    d3Output(ns("tree"), width = width, height = height),
    helpText(
      tags$i(style = "color: #694489; margin-right: 5px", icon("circle"), " Project"),
      tags$i(style = "color: #28a745; margin-right: 5px", icon("circle"), " Uploaded"),
      tags$i(style = "color: #E53935;", icon("circle"), " Not Uploaded"), 
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
        tree_list <- NULL

        if (nrow(upData) > 0) {
          
          
          project_df <- data.frame(from = rep(project_name, length(upData$schema)), to = upData$folder)
          tree_df <- rbind(project_df, reqData)
          # convert to list (name; children) using `data.tree`
          tree_list <- data.tree::FromDataFrameNetwork(tree_df)
          tree_list$Set(group = ifelse(tree_list$Get("name") %in% c(upData$folder, upData$schema), "upload", "not_load"))
          tree_list$group <- "project"
          tree_list <- as.list(tree_list, mode = 'explicit', unname = TRUE)
        }

        r2d3(data=tree_list, d3_version = "4",
          options = list(project_fill = "#694489", yes_fill= "#28a745", no_fill= "#E53935"),
          script = "www/js/collapsibleTree.js", css = sass(sass_import("www/scss/basic/collapsibleTree")))
      })
    }
  )
}
