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
          id = ns("tabs"),
          tabPanel(
            "Selected Data Type",
            value = "db-tab1",
            selectedDataTypeTabUI(ns("tab-selected-datatype"))
          ),
          tabPanel(
            "Selected Project",
            value = "db-tab2",
            selectedProjectTabUI(ns("tab-selected-project"))
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

dashboard <- function(id, synStoreObj, selectedProject, folderList, selectedDataType, downloadFolder, userName, disableIds=NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      # do not need to use ns() for shinyjs functions, which is supported in module
      ns <- session$ns
      
      # set up inital reactive value for all uploaded manifests
      uploaded_manifests <- reactiveVal(NULL)

      # all functions should not be executed until dashboard visiable, except initial one
      isDashboardOpen <- reactive(input$`toggle-btn` != 0 & input$box$visible)
      
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

      # get all uploaded manifests once the project/folder changed
      observeEvent(c(folderList(), input$box$visible), {
        req(isDashboardOpen())
        # initiate partial loading screen for generating plot
        dcWaiter("show", id = ns("tab-container"), msg = "Loading, please wait...", spin = spin_google(), style = "color: #000", color = transparent(0.9))
        dbValidationTable("validation-table", data.frame(NULL)) # reset validation table

        # disable selection to prevent changes until all uploaded manifests are queried
        lapply(disableIds, FUN = disable) 
        
        # get all uploaded manifests for selected project
        all_manifests <- getManifests(synStoreObj, folderList())
        # update reactive value
        uploaded_manifests(all_manifests)

        lapply(disableIds, FUN = enable)
      })
      
      # get requirements for selected data type
      selected_datatype_requirement <- eventReactive(c(selectedDataType(), input$box$visible), {
        req(isDashboardOpen())
        getDatatypeRequirement(selectedDataType())
      })

      # get requirements for all uploaded manifests 
      uploaded_manifests_requirement <- eventReactive(uploaded_manifests(), {
        req(isDashboardOpen())
        getManifestRequirements(uploaded_manifests())
      })

      # render info/plots for selected datatype
      observeEvent(c(uploaded_manifests(), selected_datatype_requirement(), input$dashboard$visible), {
        req(isDashboardOpen())
        selectedDataTypeTab(
          "tab-selected-datatype", userName = userName,
          uploaded_manifests(), selected_datatype_requirement(), selectedDataType(),
          tabId = "tabs", validationTab = "db-tab3", parent = session
        )
      })

      # render info/plots for all uploaded manifests and validation
      # to reduce running time, selected template updates should not initiate this event
      observeEvent(c(uploaded_manifests_requirement(), input$box$visible), {
        req(isDashboardOpen())
      
        selectedProjectTab("tab-selected-project", uploaded_manifests(), uploaded_manifests_requirement(), selectedProject())
        # validation table for all uploaded data
        validationTab("tab-validation", uploaded_manifests(), selectedProject())

        # update and hide the partial loading screen
        dcWaiter(id = ns("tab-container"), "hide")
      })
    }
  )
}
