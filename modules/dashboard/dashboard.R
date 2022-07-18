dashboardUI <- function(id) {
  ns <- NS(id)

  tagList(
    # dashboard section
    column(
      12,
      div(
        id = ns("toggle-btn-container"),
        actionButton(ns("toggle-btn"), div(span(), p("Show Data Dashboard")), class = "scroll-down"),
        helpText("check your data ingress status and data compliance")
      )
    ),
    box(
      status = "primary",
      id = ns("box"),
      width = 12,
      closable = TRUE,
      title = "Track your Data Status",
      div(
        id = ns("tab-container"),
        tabsetPanel(
          id = ns("tabs"),
          tabPanel(
            "Selected Project",
            value = "db-tab1",
            selectedProjectTabUI(ns("tab-selected-project"))
          ),
          tabPanel(
            "Selected Data Type",
            value = "db-tab2",
            selectedDataTypeTabUI(ns("tab-selected-datatype"))
          ),
          tabPanel(
            "Data Validation",
            value = "db-tab3",
            validationTabUI(ns("tab-validation"))
          )
        )
      )
    )
  )
}

#' Dashboard Module - Server
#'
#' @param id id of dashboard module
#' @param syn.store synapse storage object
#' @param project.scope selected project syn ID named with project name
#' @param schema selected schema name
#' @param disable_ids selector ids to be disable during the process of dashboard
#'
dashboard <- function(id, syn.store, project.scope, schema, disable.ids = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      # do not need to use ns() for shinyjs functions, which is supported in module
      ns <- session$ns

      # set up inital reactive value for all uploaded manifests
      uploaded_manifests <- reactiveVal(NULL)

      # show toggle btn when dashboard is closed
      observeEvent(input$box$visible, {
        req(!input$box$visible)
        Sys.sleep(0.3) # 0.3 is optimal
        show("toggle-btn-container")
      })

      # once toggle btn is clicked, hide the btn and show dashboard
      observeEvent(input$`toggle-btn`, {
        hide("toggle-btn-container")
        shinydashboardPlus::updateBox("box", action = "restore")
      })

      # retrieving data progress for dashboard should not be executed until dashboard visiable
      # get all uploaded manifests once the project/folder changed
      observeEvent(c(project.scope(), input$box$visible), {
        req(input$box$visible)
        # initiate partial loading screen for generating plot
        dcWaiter(
          "show",
          id = ns("tab-container"), url = "www/img/logo.svg", custom_spinner = TRUE,
          msg = "Loading, please wait...", style = "color: #000;", color = transparent(0.95)
        )

        # disable selection to prevent changes until all uploaded manifests are queried
        # make sure to use asis, otherwise it will add module's namespaces
        lapply(disable.ids, FUN = disable, asis = TRUE)

        # get all datasets from selected project
        folder_list <- synapse_driver$getStorageDatasetsInProject(syn.store, project.scope())
        folder_list <- list2Vector(folder_list)

        # get all uploaded manifests for selected project
        metadata <- get_dataset_metadata(
          syn.store = syn.store,
          datasets = folder_list
        )

        metadata <- validate_metadata(metadata, project.scope = list(project.scope()))
        # update reactive value
        uploaded_manifests(metadata)
      })

      # get requirements for selected data type
      selected_datatype_requirement <- eventReactive(c(schema(), input$box$visible), {
        req(input$box$visible)
        get_schema_nodes(schema())
      })

      # get requirements for all uploaded manifests
      uploaded_manifests_requirement <- eventReactive(uploaded_manifests(), {
        req(input$box$visible)
        get_metadata_nodes(uploaded_manifests())
      })

      # render info/plots for selected datatype
      observeEvent(c(uploaded_manifests(), selected_datatype_requirement(), input$dashboard$visible), {
        req(input$box$visible)
        selectedDataTypeTab(
          "tab-selected-datatype",
          uploaded_manifests(),
          selected_datatype_requirement(),
          schema()
        )
      })

      # render info/plots for all uploaded manifests and validation
      # to reduce running time, selected template updates should not initiate this event
      observeEvent(c(uploaded_manifests_requirement(), input$box$visible), {
        req(input$box$visible)
        user_name <- syn$getUserProfile()$userName
        selectedProjectTab(
          "tab-selected-project",
          user_name,
          uploaded_manifests(),
          uploaded_manifests_requirement(),
          names(project.scope()),
          parent.session = session
        )
        # validation table for all uploaded data
        validationTab("tab-validation", uploaded_manifests(), names(project.scope()))
        # force switch tabs to solve tabs content not rendered initially
        if (input$`toggle-btn` == 1) {
          updateTabsetPanel(session, "tabs", selected = "db-tab2")
          updateTabsetPanel(session, "tabs", selected = "db-tab1")
        }

        lapply(disable.ids, FUN = enable, asis = TRUE)
        # update and hide the partial loading screen
        dcWaiter(id = ns("tab-container"), "hide")
      })
    }
  )
}
