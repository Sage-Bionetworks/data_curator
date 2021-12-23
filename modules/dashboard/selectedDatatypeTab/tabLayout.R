selectedDataTypeTabUI <- function(id) {

  ns <- NS(id)
  div(class = "tab1-container",
    tagList(
      setTabTitleUI(ns("title")),
      fluidRow(
        column(12,
          column(6, class = "top-container", uiOutput(ns("summary-box"))),
          column(6, uiOutput(ns("stats-box")))
        ),
        column(12, class = "bottom-title-container",
          column(6, class = "section-title", span("Required Data Types")),
          column(6, class = "section-title", span("Requirment Relationship Network"))
        ),
        column(12, class = "legend-container",
          span(icon("circle"), "Selected Data Type", class = "selected"),
          span(icon("circle"), "Uploaded Data Type", class = "completed"),
          span(icon("circle"), "Missing Data Type", class = "missing")
        ),
        column(12, class = "bottom-container",
          column(6, column(12, dbCheckListUI(ns("checklist")))),
          column(6, column(12, dbNetworkUI(ns("network"), height = "300px")))
        )
      ),
      helpText(HTML(
        "If there is a data requirement you have not yet completed, please generate its data type template and submit the validated metadata via the process of this app.<br>
        For file-based data types (scRNA-seq, Bulk WES, etc.), please upload the data files before submitting the metadata. 
        Visit <a href='https://ncihtan.github.io/HTAN-Data-Ingress-Docs/organize-your-data-upload.html' target='_blank'>HTAN-Data-Ingress-Docs</a> 
        to know more details about the types of data."
      ))
    )
  )
}

selectedDataTypeTab <- function(id, userName, uploadData, reqData, selectedDataType, tabId, validationTab, parent) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # collect all required datatype including selected datatype
      all_req <- union(reqData, names(reqData))
      # get number of total requirements
      n_req <- length(all_req)
      # get number of manifests are required but not yet uploaded
      n_not_up <- length(setdiff(all_req, uploadData$schema))
      # get number of manifests uploaded to synapse
      n_up <- length(intersect(all_req, uploadData$schema))
      # get number of manifests that are out of date
      n_outdate <- sum(uploadData$errorType %in% c("Wrong Schema", "Out of Date"))
      # calculate the values for progress bar: number of uploaded / number of total requirements
      progress_value <- round(n_up/n_req * 100, 0)

      # render tab title
      setTabTitle("title", paste0("Completion of Requirements for Data Type: ", sQuote(selectedDataType)))

      # render banner contents
      output$`summary-box` <- renderUI({
        div(class = "summary-box", align = "center",
          div(
            div(class = "summary-icon", icon("crown", "fa-2x")),
            div(class = "summary-header", h3(paste0("Congratulations ", userName, "!"))),
            div(class = "summary-body", paste0("you have made ", progress_value, "% progress"))
          ),
          progressBarUI(ns("progress-box"))
        )
      })

      # render summary stats boxes
      output$`stats-box` <- renderUI({
        div(class = "stats-box",
          column(12, 
            column(4, class = "stats-item",
              tagList(
                icon("smile-wink", "fa-3x completed"),
                div(class = "stat-text", 
                  h4("Completed", style = "border-bottom: 0.6px solid #28a745;"), span(n_up)
                )
              )
            ),
            column(4, class = "stats-item",
              tagList(
                icon("frown", "fa-3x missing"),
                div(class = "stat-text", 
                  h4("Missing", style = "border-bottom: 0.6px solid #E53935;"), span(n_not_up)
                )
              )
            ),
            column(4, class = "stats-item",
              tagList(
                icon("surprise", "fa-3x outdate"),
                div(class = "stat-text", 
                  h4("Outdate", style = "border-bottom: 0.6px solid #ff9900;"), span(n_outdate)
                )
              )
            )
          ),
          column(12, class = "stats-box-btn", align = "center",
            actionButton(ns("view-btn"), "View More", class = "btn-primary-color"),
            span("Click to know more about 'Outdate' manifests")
          )
        )
      })

      # render circular progress bar
      progressBar("progress-box", value = progress_value, r = 65, circular = TRUE)

      # render check list of requirments for selected datatype
      dbCheckList("checklist", uploadData, reqData)
      # render network plot for requirements of selected datatype
      dbNetwork("network", uploadData, reqData, selectedDataType)

      # redirect to validation tab of dashboard once the btn clicked
      observeEvent(input$`view-btn`, {
        req(input$`view-btn` != 0)
        updateTabsetPanel(parent, tabId, selected = validationTab)
      })
    }
  )
}
