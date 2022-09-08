selectedDataTypeTabUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    class = "selectedDataTypeTab-container",
    br(),
    column(5,
      class = "instruction-container",
      h3("How to explore data requirements?", class = "bold-normal", align = "center"),
      tags$ol(
        tags$li("Hover over the nodes to view the data type labels"),
        tags$li(HTML(paste0(
          strong(
            "node A ", icon("long-arrow-alt-right"), " node B ",
            icon("long-arrow-alt-right"), " node C"
          ),
          "illustrates that ",
          strong("Data type A"), " requires to upload ",
          strong("Data type B"), " and ", strong("Data type B"),
          " requires to upload ", strong("Data type C")
        ))),
        tags$li(
          "Please upload all missing data types to complete the requirements for the selected data type.
           To upload a missing requirement, please generate its template and
           submit the validated metadata via the process of this app."
        ),
        tags$li("Try to check the requirements for other data types by changing the selection of data type dropdown above")
      )
    ),
    column(7, class = "network-container", dbNetworkUI(ns("network"), height = "300px")),
    column(
      12,
      helpText(HTML(
        "
        For file-based data types (scRNA-seq, Bulk WES, etc.), please upload the data files before submitting the metadata.<br>
        Visit <a href='https://ncihtan.github.io/HTAN-Data-Ingress-Docs/organize-your-data-upload.html' target='_blank'>HTAN-Data-Ingress-Docs</a>
         to know more details about the types of data.
        "
      ))
    )
  )
}

selectedDataTypeTab <- function(id, metadata, nodes, schema, schema.display.name) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # collect all required datatype including selected datatype
      all_req <- union(nodes, names(nodes))
      # get number of total requirements
      n_req <- length(all_req)
      # render network plot for requirements of selected datatype
      dbNetwork("network", metadata, nodes, schema)
    }
  )
}