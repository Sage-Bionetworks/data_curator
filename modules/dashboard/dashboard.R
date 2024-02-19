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
            "Data Overview",
            value = "db-tab1",
            selectedProjectTabUI(ns("tab-selected-project"))
          ),
          tabPanel(
            textOutput(ns("db-tab2-name")),
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
#' @param schema.display.name display name for selected schema name
#' @param disable_ids selector ids to be disable during the process of dashboard
#' @param ncores number of cpu to run parallelization
#'
dashboard <- function(id, syn.store, project.scope, schema, schema.display.name,
                      disable.ids = NULL, ncores = 1, access_token, fileview,
                      folder, schematic_api="reticulate", schema_url) {
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
      # retrieving data progress for dashboard should not be executed until dashboard visible
      # get all uploaded manifests once the project/folder changed
      observeEvent(c(project.scope(), input$box$visible), {
        req(input$box$visible)
        # initiate partial loading screen for generating plot
        dcWaiter(
         "show",
         id = ns("tab-container"), url = "www/img/logo.svg", custom_spinner = TRUE,
         msg = "Loading, please wait..."
        )

        # disable selection to prevent changes until all uploaded manifests are queried
        # make sure to use asis, otherwise it will add module's namespaces
        lapply(disable.ids, FUN = disable, asis = TRUE)

        # get all datasets from selected project
        folder_list <- switch(schematic_api,
                              "rest" = storage_project_datasets(url=file.path("https://schematic-dev.api.sagebionetworks.org/v1/storage/project/datasets"),
                                                                asset_view = fileview,
                                                                project_id=folder,
                                                                access_token=access_token),
                              "reticulate" = storage_projects_datasets_py(syn.store, project.scope())
        )
        folder_list <- list2Vector(folder_list)

        # get all uploaded manifests for selected project
        metadata <- get_dataset_metadata(
          syn.store = syn.store,
          datasets = folder_list,
          ncores = ncores,
          schematic_api = schematic_api,
          access_token = access_token,
          fileview = fileview
        )

        metadata <- validate_metadata(metadata, project.scope = list(project.scope()),
                                     schematic_api = schematic_api, schema_url=schema_url,
                                     access_token=access_token)
        # update reactive value
        uploaded_manifests(metadata)
      })

      # get requirements for selected data type
      selected_datatype_requirement <- eventReactive(c(schema(), input$box$visible), {
        req(input$box$visible)
        get_schema_nodes(schema(), schematic_api = schematic_api,
                    url=file.path("https://schematic-dev.api.sagebionetworks.org/v1/model/component-requirements"),
                    schema_url = schema_url)
      })

      # get requirements for all uploaded manifests
      uploaded_manifests_requirement <- eventReactive(uploaded_manifests(), {
        req(input$box$visible)
        req(uploaded_manifests())
        # remove rows with invalid component name
        metadata <- uploaded_manifests() %>% filter(!is.na(Component), Component != "Unknown")
        get_metadata_nodes(metadata, ncores = ncores, schematic_api=schematic_api,
                          schema_url = schema_url, url = file.path("https://schematic-dev.api.sagebionetworks.org/v1/model/component-requirements"))
      })

      # render info/plots for selected datatype
      observeEvent(c(uploaded_manifests(), selected_datatype_requirement(), input$dashboard$visible), {
        req(input$box$visible)
        req(uploaded_manifests())
        # update tab 2 name
        output$`db-tab2-name` <- renderText(sprintf("'%s' Requirements", schema.display.name()))
        # remove rows with invalid component name
        metadata <- uploaded_manifests() %>% filter(!is.na(Component), Component != "Unknown")
        selectedDataTypeTab(
          "tab-selected-datatype",
          metadata,
          selected_datatype_requirement(),
          schema(),
          schema.display.name = schema.display.name()
        )
      })

      # render info/plots for all uploaded manifests and validation
      # to reduce running time, selected template updates should not initiate this event
      observeEvent(c(uploaded_manifests_requirement(), input$box$visible), {
        req(input$box$visible)
        req(uploaded_manifests())
        user_name <- switch(schematic_api,
                            reticulate = synapse_user_profile_py(),
                            rest = synapse_user_profile(auth=access_token)[["userName"]])
        # remove rows with invalid component name
        metadata <- uploaded_manifests() %>% filter(!is.na(Component), Component != "Unknown")
        selectedProjectTab(
          "tab-selected-project",
          user_name,
          metadata,
          uploaded_manifests_requirement(),
          names(project.scope()),
          parent.session = session
        )
        # validation table for all uploaded data
        # use all metadata including invalid components
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
