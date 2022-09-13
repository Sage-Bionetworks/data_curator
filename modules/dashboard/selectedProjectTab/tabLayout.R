selectedProjectTabUI <- function(id) {
  ns <- NS(id)
  div(
    class = "selectedProjectTab-container",
    tagList(
      br(),
      fluidRow(
        column(
          12,
          class = "banner-container",
          column(6, dbRaterUI(ns("summary"))),
          column(6, dbStatsBoxUI(ns("stats")))
        ),
        column(
          12,
          fluidRow(
            column(12, uiOutput(ns("evaluate-container")))
          )
        ),
        column(
          12,
          fluidRow(
            column(12, class = "section-title", NULL),
            column(12, uiOutput(ns("tree-container")))
          )
        )
      )
    )
  )
}

selectedProjectTab <- function(id, username, metadata, nodes, project.name, parent.session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      ## **************** Clean data ****************
      # get all uploaded data types
      uploaded <- metadata$Component
      # collect all required datatype including selected datatype
      requirements <- union(nodes$from, nodes$to)
      requirements <- requirements[!grepl("f:", requirements)]
      # get all missing requirements
      not_uploaded <- setdiff(requirements, uploaded)
      # folder list
      folder_list <- unique(nodes$folder)
      # completion progress percentage: complete datasets / total datasets
      progress_value <- nodes %>%
        group_by(folder) %>%
        mutate(perc = sum(to %in% uploaded) / n() * 100) %>%
        pull(perc) %>%
        mean() %>%
        round(0)

      ## **************** Summary banner ****************
      # render banner
      dbRater("summary", progress_value, username, project.name)
      # render summary stats boxes
      dbStatsBox("stats", metadata, nodes, parent.session = parent.session)

      ## **************** Tree plot ****************
      # change d3 tree height based on how many nodes
      output$`tree-container` <- renderUI({
        dbTreeUI(ns("requirement-tree"), n.nodes = length(folder_list))
      })
      # render collasiple tree
      dbTree("requirement-tree", uploaded, nodes, project.name)

      ## **************** Evaluate submission ****************
      if (length(not_uploaded) > 0) {
        # render UI section for evaluation of submission
        output$`evaluate-container` <- renderUI({
          tagList(
            column(12, class = "section-title", NULL),
            column(12, tagList(
              h3("What data you need to upload next?", class = "bold-normal"),
              span(
                class = "black-msg bold-sm",
                "Select one or more of your missing data types
                below to evaluate how your submission would progress. If unsure, start with uploading data types that get you closer to completion:"
              ),
              checkboxGroupInput(ns("checkbox-evaluate"), NULL, choices = as.list(not_uploaded), inline = TRUE),
              uiOutput(ns("evaluate-res"))
            ))
          )
        })

        observeEvent(input$`checkbox-evaluate`, ignoreNULL = FALSE, {
          # update required data frame
          evaluate_datatypes <- input$`checkbox-evaluate`

          if (is.null(evaluate_datatypes)) {
            # reset progress values if no evaluated data type selected
            output$`evaluate-res` <- renderUI(NULL)
          } else {
            # substrate # of evaluated datatypes for nMiss
            # note, it will not change the nodes outside of observeEvent
            for (d in evaluate_datatypes) {
              # get which folder contains evaluated datatypes
              evaluate_ds <- unique(nodes$folder_id[nodes$to == d])
              loc <- which(nodes$folder_id %in% evaluate_ds)
              nodes$n_miss[loc] <- nodes$n_miss[loc] - 1
            }
            # add evaluated datatypes to update uploaded data
            uploaded <- c(uploaded, input$`checkbox-evaluate`)

            # calculate new progress value
            progress_value <- nodes %>%
              group_by(folder) %>%
              mutate(perc = sum(to %in% uploaded) / n() * 100) %>%
              pull(perc) %>%
              mean() %>%
              round(0)

            # render new total progress result
            output$`evaluate-res` <- renderUI({
              items <- paste0(sQuote(evaluate_datatypes), collapse = ", ")
              span(class = "warn-msg", HTML(paste0(
                "By submitting: ", items, ", your total progress will become ",
                strong(progress_value), "% !!!"
              )))
            })
          }
          # update banner
          dbRater("summary", progress_value, username, project.name)
          # update collasiple tree
          dbTree("requirement-tree", uploaded, nodes, project.name)
        })
      } else {
        # reset container
        output$`evaluate-container` <- renderUI(NULL)
      }
    }
  )
}
