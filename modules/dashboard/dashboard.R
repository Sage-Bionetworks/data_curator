dashboardUI <- function(id) {

  ns <- NS(id)

  tagList(
    # dashboard section
    column(12, 
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
      tabsetPanel(
        id = ns("dashboard-tabs"),
        tabPanel(
          "Selected Data Type",
          value = "db-tab1",
          setTabTitleUI(ns("tab1")),
          selectedDataTypeTabUI(ns("dashboard-tab1"))
        ),
        tabPanel(
          "Selected Project",
          value = "db-tab2",
          setTabTitleUI(ns("tab2")),
          # dbTreeUI(ns("tree"))
          allUploadManifestsTabUI(ns("dashboard-tab2"))
        ),
        tabPanel(
          "Data Validation",
          value = "db-tab3",
          setTabTitleUI(ns("tab3")),
          tagList(
            dbValidationUI(ns("validation-table")),
            helpText("If there is any validation error, 
              please re-validate the corresponding metadata to see detailed errors and re-submit once you have corrected metadata.")
          )
        )
      )
    )
  )
}

dashboard <- function(id, syn, project, foldeList, template, downloadFolder, config, userName, disableIds=NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      # do not need to use ns() for shinyjs functions, which is supported in module
      ns <- session$ns
      
      # set up variables
      uploaded_manifests <- reactiveVal(NULL)
      all_component_requirements <- reactiveVal(NULL)
      selected_component_requirement <- reactiveVal(NULL)
      quick_val <- reactiveVal(NULL)
      # get template display name
      templateName <- reactive(config$display_name[match(template(), config$schema_name)])
      # all functions should not be executed until dashboard visiable, except initial one
      dashboardOnChange <- reactive(input$`toggle-btn` != 0 & input$box$visible)

      observeEvent(input$box$visible, {
        req(!input$box$visible)
        Sys.sleep(0.3) # 0.3 is optimal
        show("toggle-btn-container")
      })

      observeEvent(input$`toggle-btn`, {
        hide("toggle-btn-container")
        logjs(dashboardOnChange())
        shinydashboardPlus::updateBox("box", action = "restore")
      })

      observeEvent(c(foldeList(), input$box$visible), {
        req(dashboardOnChange())
        # initiate partial loading screen for generating plot
        dcWaiter("show", id = ns("box"), msg = "Loading, please wait...", spin = spin_google(), style = "color: #000", color = transparent(0.2))
        dbValidation("validation-table", data.frame(NULL)) # reset validation table

        # disable selection to prevent changes until all uploaded manifests are queried
        lapply(disableIds, FUN = disable) 
        
        # get all uploaded manifests for selected project
        all_manifests <- getManifests(syn, foldeList(), downloadFolder = downloadFolder)
        uploaded_manifests(all_manifests)
        # get all data type requirements for uploaded manifests
        all_component_requirements(getManifestRequirements(all_manifests))

        lapply(disableIds, FUN = enable)
      })

      # get requirements for selected template
      observeEvent(c(template(), input$box$visible), {
        req(dashboardOnChange())
        selected_component_requirement(getDatatypeRequirement(template()))
      })

      # render dashboard plots
      observeEvent(c(uploaded_manifests(), selected_component_requirement(), input$dashboard$visible), {
        req(dashboardOnChange())
        setTabTitle("tab1", paste0("Completion of requirements for data type: ", sQuote(templateName())))
        selectedDataTypeTab(
          "dashboard-tab1", userName = userName,
          uploaded_manifests(), selected_component_requirement(), template(),
          tabId = "dashboard-tabs", validationTab = "db-tab3", parent = session
        )
      })

      observeEvent(c(all_component_requirements(), input$box$visible), {
        req(dashboardOnChange())
        setTabTitle("tab2", paste0("Completion of requirements for project: ", sQuote(project())))
        allUploadManifestsTab("dashboard-tab2", uploaded_manifests(), all_component_requirements(), project())
      })

      # validation table for all uploaded data
      observeEvent(c(uploaded_manifests()), {
        manifest <- isolate(uploaded_manifests())
        setTabTitle("tab3", paste0("Validate your uploaded data in the project: ", sQuote(project())))
        validation_res <- getManifestValidation(manifest)
        dbValidation("validation-table", validation_res)
        # update and hide the partial loading screen
        dcWaiter(id = ns("box"), "hide")
      })
    }
  )
}
