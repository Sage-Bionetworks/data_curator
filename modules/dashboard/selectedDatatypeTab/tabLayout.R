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
          "illustrates ",
          strong("Data type A"), " is required to upload all upstream data types, ",
          strong("Data type B"), " and ", strong("Data type C")
        ))),
        tags$li("To check the requirements for other data types, you can change the selection of data type dropdown above")
      )
    ),
    column(7, dbNetworkUI(ns("network"), height = "300px")),
    column(
      12,
      helpText(HTML(
        "If there is a data requirement you have not yet completed,
        please generate its data type template and submit the validated metadata via the process of this app.<br>
        For file-based data types (scRNA-seq, Bulk WES, etc.), please upload the data files before submitting the metadata.<br>
        Visit <a href='https://ncihtan.github.io/HTAN-Data-Ingress-Docs/organize-your-data-upload.html' target='_blank'>HTAN-Data-Ingress-Docs</a>
        to know more details about the types of data."
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
      # render tab title
      # setTabTitle("title", paste0("Completion of Requirements for Data Type: ", sQuote(schema.display.name)))

      # render check list of requirments for selected datatype
      # dbCheckList("checklist", metadata, nodes)
      # render network plot for requirements of selected datatype
      dbNetwork("network", metadata, nodes, schema)
    }
  )
}