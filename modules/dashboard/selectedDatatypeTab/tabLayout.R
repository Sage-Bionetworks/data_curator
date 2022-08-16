selectedDataTypeTabUI <- function(id) {
  ns <- NS(id)
  div(
    class = "selectedDataTypeTab-container",
    tagList(
      setTabTitleUI(ns("title")),
      fluidRow(
        column(12,
          class = "bottom-title-container",
          column(12, class = "section-title", span("Required Data Types"))
        ),
        column(12,
          class = "bottom-container",
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

selectedDataTypeTab <- function(id, metadata, nodes, schema) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # collect all required datatype including selected datatype
      all_req <- union(nodes, names(nodes))
      # get number of total requirements
      n_req <- length(all_req)
      # remove manifests with invalid component name
      metadata <- metadata[!is.na(metadata$Component), ]
      # render tab title
      setTabTitle("title", paste0("Completion of Requirements for Data Type: ", sQuote(schema)))

      # render check list of requirments for selected datatype
      dbCheckList("checklist", metadata, nodes)
      # render network plot for requirements of selected datatype
      dbNetwork("network", metadata, nodes, schema)
    }
  )
}