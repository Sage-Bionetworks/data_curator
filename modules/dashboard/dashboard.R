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
      div(id = ns("tab-container"),
        tabsetPanel(
          id = ns("dashboard-tabs"),
          tabPanel(
            "Selected Data Type",
            value = "db-tab1",
            selectedDataTypeTabUI(ns("dashboard-tab1"))
          ),
          tabPanel(
            "Selected Project",
            value = "db-tab2",
            allUploadManifestsTabUI(ns("dashboard-tab2"))
          ),
          tabPanel(
            "Data Validation",
            value = "db-tab3",
            validationTabUI(ns("dashboard-tab3"))
          )
        )
      )
    )
  )
}

dashboard <- function(id, syn, selectedProject, folderList, selectedDataType, downloadFolder, userName, disableIds=NULL) {
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

      # all functions should not be executed until dashboard visiable, except initial one
      dashboardOnChange <- reactive(input$`toggle-btn` != 0 & input$box$visible)

      observeEvent(input$box$visible, {
        req(!input$box$visible)
        Sys.sleep(0.3) # 0.3 is optimal
        show("toggle-btn-container")
      })

      observeEvent(input$`toggle-btn`, {
        hide("toggle-btn-container")
        shinydashboardPlus::updateBox("box", action = "restore")
      })

      observeEvent(c(folderList(), input$box$visible), {
        req(dashboardOnChange())
        # initiate partial loading screen for generating plot
        dcWaiter("show", id = ns("tab-container"), msg = "Loading, please wait...", spin = spin_google(), style = "color: #000", color = transparent(0.9))
        dbValidation("validation-table", data.frame(NULL)) # reset validation table

        # disable selection to prevent changes until all uploaded manifests are queried
        lapply(disableIds, FUN = disable) 
        
        # get all uploaded manifests for selected project
        all_manifests <- getManifests(syn, folderList(), downloadFolder = downloadFolder)
        uploaded_manifests(all_manifests)
        # get all data type requirements for uploaded manifests
        all_component_requirements(getManifestRequirements(all_manifests))

        lapply(disableIds, FUN = enable)
      })

      # get requirements for selected data type
      observeEvent(c(selectedDataType(), input$box$visible), {
        req(dashboardOnChange())
        selected_component_requirement(getDatatypeRequirement(selectedDataType()))
      })

      # render dashboard plots
      observeEvent(c(uploaded_manifests(), selected_component_requirement(), input$dashboard$visible), {
        req(dashboardOnChange())
        setTabTitle("tab1", paste0("Completion of requirements for data type: ", sQuote(selectedDataType())))
        selectedDataTypeTab(
          "dashboard-tab1", userName = userName,
          uploaded_manifests(), selected_component_requirement(), selectedDataType(),
          tabId = "dashboard-tabs", validationTab = "db-tab3", parent = session
        )
      })

      observeEvent(c(all_component_requirements(), input$box$visible), {
        req(dashboardOnChange())
        allUploadManifestsTab("dashboard-tab2", uploaded_manifests(), all_component_requirements(), selectedProject())
      })

      # validation table for all uploaded data
      observeEvent(c(uploaded_manifests()), {
        # validationTab("dashboard-tab3", uploaded_manifests(), selectedProject())
        # update and hide the partial loading screen
        dcWaiter(id = ns("tab-container"), "hide")
      })
    }
  )
}
