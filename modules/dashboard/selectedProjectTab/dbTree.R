node_svg <- function(color, x = 2, y = 6) {
  HTML(paste0(
    '
    <svg width="22" height="28">
      <circle class="node" cx="11" cy="11" r="6"
       transform="translate(', x, ", ", y, ')" style="fill: ', color, '; stroke: rgb(169, 169, 169); stroke-width: 0.5px;"></circle>
    </svg>
    '
  ))
}

project_svg <- function(color, x = 2, y = 8) {
  HTML(paste0(
    '
    <svg width="22" height="28">
      <path d="M4 4a4 4 0 1 1 4.5 3.969V13.5a.5.5 0 0 1-1 0V7.97A4 4 0 0 1 4 3.999zm2.493 8.574a.5.5 0 0 1-.411.575c-.712.118-1.28.295-1.655.493a1.319 1.319 0 0 0-.37.265.301.301 0 0 0-.057.09V14l.002.008a.147.147 0 0 0 .016.033.617.617 0 0 0 .145.15c.165.13.435.27.813.395.751.25 1.82.414 3.024.414s2.273-.163 3.024-.414c.378-.126.648-.265.813-.395a.619.619 0 0 0 .146-.15.148.148 0 0 0 .015-.033L12 14v-.004a.301.301 0 0 0-.057-.09 1.318 1.318 0 0 0-.37-.264c-.376-.198-.943-.375-1.655-.493a.5.5 0 1 1 .164-.986c.77.127 1.452.328 1.957.594C12.5 13 13 13.4 13 14c0 .426-.26.752-.544.977-.29.228-.68.413-1.116.558-.878.293-2.059.465-3.34.465-1.281 0-2.462-.172-3.34-.465-.436-.145-.826-.33-1.116-.558C3.26 14.752 3 14.426 3 14c0-.599.5-1 .961-1.243.505-.266 1.187-.467 1.957-.594a.5.5 0 0 1 .575.411z"
        transform="translate(', x, ", ", y, ')" style="fill: ', color, '; stroke: rgb(169, 169, 169); stroke-width: 0.5px;">
      </path>
    </svg>
    '
  ))
}

folder_svg <- function(color, x = 2, y = 6, cross = FALSE) {
  path <- paste0(
    '<path d="M9.828 3h3.982a2 2 0 0 1 1.992 2.181l-.637 7A2 2 0 0 1 13.174 14H2.825a2 2 0 0 1-1.991-1.819l-.637-7a1.99 1.99 0 0 1 .342-1.31L.5 3a2 2 0 0 1 2-2h3.672a2 2 0 0 1 1.414.586l.828.828A2 2 0 0 0 9.828 3zm-8.322.12C1.72 3.042 1.95 3 2.19 3h5.396l-.707-.707A1 1 0 0 0 6.172 2H2.5a1 1 0 0 0-1 .981l.006.139z"
      transform="translate(', x, ", ", y, ')" style="cursor: pointer; fill:', color, '; stroke: rgb(169, 169, 169); stroke-width: 0.5px;"></path>'
  )
  if (cross) {
    path <- paste0(
      path,
      '<path d="M11.854 10.146a.5.5 0 0 0-.707.708L12.293 12l-1.146 1.146a.5.5 0 0 0 .707.708L13 12.707l1.146 1.147a.5.5 0 0 0 .708-.708L13.707 12l1.147-1.146a.5.5 0 0 0-.707-.708L13 11.293l-1.146-1.147z"
        transform="translate(-2.5, 2.5)" style="stroke: #E53935; stroke-width: 1.2px;"></path>'
    )
  }

  HTML(paste0('<svg width="22" height="22">', path, "</svg>"))
}

table_svg <- function(color, x = 2, y = 6) {
  HTML(paste0(
    '
    <svg width="22" height="22">
      <path d="M0 2a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V2zm15 2h-4v3h4V4zm0 4h-4v3h4V8zm0 4h-4v3h3a1 1 0 0 0 1-1v-2zm-5 3v-3H6v3h4zm-5 0v-3H1v2a1 1 0 0 0 1 1h3zm-4-4h4V8H1v3zm0-4h4V4H1v3zm5-3v3h4V4H6zm4 4H6v3h4V8z"
        transform="translate(', x, ", ", y, ')" style="cursor: pointer; fill: ', color, "; stroke: ", color, '; stroke-width: 0.5px;">
      </path>
    </svg>
    '
  ))
}

dbTreeUI <- function(id, n.nodes = NULL) {
  ns <- NS(id)

  # manually create a legend
  legend_icons <- list(
    project_svg("#694489", 4, 4), folder_svg("#f8d775", 4, 4),
    table_svg("#28a745", 4, 4), table_svg("#E53935", 4, 4)
  )
  legend_names <- c("Selected Project", "Dataset/Folder", "Uploaded Data", "Missing Data")
  # default height is 400px, we would need to manually set height for large
  height <- paste0(max(400, 48 * n.nodes), "px")

  div(
    class = "collapsibleTree-container",
    fluidRow(
      column(
        class = "top-section",
        12,
        column(
          7,
          align = "left",
          tagList(
            span(
              class = "black-msg bold-sm",
              HTML(paste0(
                "
                If you see ", folder_svg("#f8d775", cross = TRUE),
                " datasets, please keep clicking the icons until the missing data (",
                table_svg("#E53935"), ") is shown.
                "
              ))
            )
          )
        ),
        column(
          4,
          align = "left",
          div(
            class = "legend-container",
            lapply(seq_along(legend_icons), function(i) {
              tags$span(class = "legend", legend_icons[[i]], span(legend_names[i]))
            })
          )
        )
      ),
      # r2d3 height can only be adjusted in d3output after the number of folders is known
      column(
        12, d3Output(ns("tree"), width = "100%", height = height)
      )
    )
  )
}

dbTree <- function(id, upload.data, nodes, project.name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tree_list <- NULL

      if (length(upload.data) != 0) {

        # get folder list
        folder_list <- sort(unique(nodes$folder))
        # change d3 tree height based on how many nodes, 48 per node is optimal
        height <- paste0(48 * length(folder_list), "px")
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
          node_color = if_else(folder_list %in% incompleted_ds$folder, "#FF794A", "#A287AF"),
          node_icon = c("folder")
        )
        # make nodes data from datasets to their requirements
        dataset_to_req <- nodes %>%
          mutate(
            font_opacity = if_else(nodes$to %in% nodes$from, 0, 1),
            node_color = if_else(nodes$to %in% upload.data, "#28a745", "#E53935"),
            node_icon = c("table")
          ) %>%
          select(from, to, font_opacity, node_color, node_icon)
        tree_df <- rbind(project_to_dataset, dataset_to_req) %>%
          mutate_at(1:2, ~ gsub(pattern, "", .)) %>%
          distinct() # remove duplicated rows to save conversion time
        # convert to tree list using `data.tree`
        tree_list <- data.tree::FromDataFrameNetwork(tree_df)
        # tree_list$Set(group = ifelse(tree_list$Get("name") %in% c(upData$folder, upData$schema), "upload", "not_load"))
        tree_list$font_opacity <- 1
        tree_list$node_color <- "#694489"
        tree_list$node_icon <- "geo"
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