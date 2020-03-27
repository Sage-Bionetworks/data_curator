library(shiny)
library(shinyjs)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(stringr)
library(DT)
library(jsonlite)
library(reticulate)
library(ggplot2)
library(purrr)
library(plotly)

#########global
use_condaenv('py3.5', required = TRUE )
reticulate::import("sys")
reticulate::import_from_path("MetadataModel", path = "HTAN-data-pipeline")
reticulate::import_from_path("ManifestGenerator", path = "HTAN-data-pipeline")

source_python("synLoginFun.py")
source_python("metadataModelFuns.py")

source("functions.R")

#########

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    titleWidth = 250,
    title = "Data Curator",
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 50px}"),
            tags$style(".main-header .logo {height: 70px; font-size: 28px; padding-top: 10px}"),
            tags$style(".sidebar-toggle {height: 15px; padding-top: 25px !important;}"),
            tags$style(".navbar {min-height:50px !important}"),
            tags$style(".messages-menu {padding-top :5px}" ),
            tags$a(href="https://humantumoratlas.org/", target = "_blank", 
                   tags$img(height = "40px", alt = "HTAN LOGO", 
                            src = "HTAN_text_logo.png"))) #,
    # dropdownMenu(type = "messages", icon = icon("user", "fa-2x")) ### dummy user icon
    ),
  dashboardSidebar( width = 250, 
    tags$style(".left-side, .main-sidebar {padding-top: 80px; font-weight: bold; font-size: 1.1em } "),
    sidebarMenu(
    # menuItem("Dataset Dashboard", tabName = "dashboard", icon = icon("home")),
    menuItem("Select your Dataset", tabName = "data", icon = icon("mouse-pointer")),
    menuItem("Get Metadata Template", tabName = "template", icon = icon("table")),
    menuItem("Submit & Validate Metadata", tabName = "upload", icon = icon("upload")) #, 
    # menuItem("Annotation Status Summaries", tabName = "anno_summaries", icon = icon("tasks") )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style("#shiny-notification-error {height: 500px; padding :20px; display: table-cell}
                 #shiny-notification-processing {background-color: #F7DC6F}
                 #shiny-notification-success {background-color : #82E0AA}"
      ),
      singleton(
        includeScript("www/readCookie.js")
      )),
    uiOutput("title"),
    tabItems(
      # First tab content
      tabItem(tabName = "data",
              h2("Set Dataset and Template for Curation"),
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width= 6,
                  title = "Choose a Project and Dataset: ",
                  selectizeInput(inputId = "var", label = "Project:",
                                 choices = "Generating..." ) , #names(projects_namedList) ),
                  uiOutput('folders')
                ),
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  title = "Choose a Template Type: ",
                  selectInput(
                    inputId = "template_type",
                    label = "Template:",
                    choices = list("ScRNA-seqAssay", "Demographics", "FamilyHistory", "Exposure", "FollowUp", "Treatment") 
                    ## add mapping step from string to input when I have more time
                  ) 
                )
              )
              ),
      # Second tab item
      tabItem(tabName = "template",
              h2("Download Template for Selected Dataset"),
              fluidRow(
                box(
                  title = "Get Link, Annotate, and Download Template as CSV",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("download", "Link to Google Sheets Template"),
                  hidden(
                    div(
                      id = 'text_div',
                      height = "100%",
                      htmlOutput("text"),
                      style = "font-size:18px; background-color: white; border: 1px solid #ccc; border-radius: 3px; margin: 10px 0; padding: 10px"
                    )
                  )
                ),
                box(
                  title = "Have Previously Submitted Metadata?",
                  status = "primary",
                  solidHeader = TRUE, 
                  width = 12,
                  actionButton("link", "Link to Google Sheets"),
                  hidden(
                    div(
                      id = 'text_div3',
                      height = "100%",
                      htmlOutput("text3"),
                      style = "font-size:18px; background-color: white; border: 1px solid #ccc; border-radius: 3px; margin: 10px 0; padding: 10px"
                    )
                  )
                )
                
              )
      ), 
      
      # Third tab content
      tabItem(tabName = "upload",
              # useShinyjs(),
              h2("Submit & Validate a Filled Metadata Template"),
              fluidRow(
                box(
                  title = "Upload Filled Metadata as a CSV",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12,
                  # fileInput("file1", "Upload CSV File",
                  #           accept=c('text/csv', 
                  #                    'text/comma-separated-values,text/plain', 
                  #                    '.csv'))
                  uiOutput('fileInput_ui')
                ),
                box(
                  title = "Metadata Preview",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12, 
                  DT::DTOutput("tbl") #,
                  # helpText("Google spreadsheet row numbers are incremented from this table by 1")
                ),
                box(
                  title = "Validate Filled Metadata",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("validate", "Validate Metadata"),
                  hidden(
                    div(id='text_div2',
                        # width = 12,
                        height = "100%",
                        htmlOutput("text2"),
                        style = "font-size:18px; background-color: white; border: 1px solid #ccc; border-radius: 3px; margin: 10px 0; padding: 10px"
                    )
                  )
                ),
                box(    title = "Submit Validated Metadata to Synapse",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        # actionButton("submit", "Submit to Synapse")
                        uiOutput("submit")
                )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  ########### session global 
  reticulate::source_python("synStore_Session.py")

  ### logs in and gets list of projects they have access to
  synStore_obj <- NULL
  # get_projects_list(synStore_obj)
  projects_list <- c()

  projects_namedList <- c()

  proj_folder_manifest_cells <- c()
  ############

  ### synapse cookies
  session$sendCustomMessage(type = "readCookie", message = list())
  
  ## Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(
      modalDialog(
        title = "Not logged in",
        HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.")
      )
    )
  })

  observeEvent(input$cookie, {  
    showNotification(id="processing", "Please wait while we log you in...", duration = NULL, type = "warning" )
    ### logs in 
    syn_login(sessionToken=input$cookie, rememberMe = FALSE)

    ### welcome message
    output$title <- renderUI({
      titlePanel(sprintf("Welcome, %s", syn_getUserProfile()$userName))
    })

    ### updating global vars with values for projects
    synStore_obj <<- syn_store("syn20446927", token = input$cookie)
    # get_projects_list(synStore_obj)
    projects_list <<- get_projects_list(synStore_obj)

    for (i in seq_along(projects_list)) {
      projects_namedList[projects_list[[i]][[2]]] <<- projects_list[[i]][[1]]
    }
    
    ### updates project dropdown
    updateSelectizeInput(session, 'var', choices = names(projects_namedList))
    removeNotification(id="processing",)
    
  })


  ### rename the input template type to scRNA-seq
  # in_template_type <- "ScRNA-seq"

  ### folder datasets if value in project
observeEvent( ignoreNULL = TRUE, ignoreInit = TRUE,
  input$var, { 
  output$folders = renderUI({                         
    selected_project <- input$var

    # if selected_project not empty
    if (!is.null(selected_project)) { 
    project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
    # project_synID <- str(project_synID)

    ### gets folders per project
    folder_list <- get_folder_list(synStore_obj, project_synID)
    folders_namedList <- c()
    for (i in seq_along(folder_list)) {
      folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
    }
    folderNames <- names(folders_namedList)

    ### updates foldernames
    selectInput( inputId = "dataset", label = "Dataset:", choices = folderNames)
    }
  })
})

  ###toggles link when download button pressed
  observeEvent(
    input$download, {

      selected_folder <- input$dataset
      selected_project <- input$var

      ### progess notif
      showNotification(id = "processing",  "Generating link...", duration = NULL, type = "warning" )

      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
      folder_list <- get_folder_list(synStore_obj, project_synID)
      folders_namedList <- c()
      for (i in seq_along(folder_list)) {
        folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
      }

      folder_synID <- folders_namedList[[selected_folder]]

      file_list <- get_file_list(synStore_obj, folder_synID)

      file_namedList <- c()
      for (i in seq_along(file_list)) {
        file_namedList[file_list[[i]][[2]]] <- file_list[[i]][[1]]
      }
      filename_list <- names(file_namedList)

      manifest_url <- getModelManifest(paste0("HTAN ", input$template_type), input$template_type, filenames = filename_list )
      ### add in the step to convert names later
      toggle('text_div')

      ### if want a progress bar need more feedback from API to know how to increment progress bar

      output$text <- renderUI({
        tags$a(href = manifest_url, manifest_url, target="_blank") ### add link to data dictionary
      })

      ### when done remove progress notif
      removeNotification(id = "processing")
    }
)

  ###toggles link to previous manifest when pressed
  observeEvent(
    input$link, {
      selected_project <- input$var
      selected_folder <- input$dataset

      showNotification(id="processing", "Processing...", duration = NULL, type = "default" )

      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
      folder_list <- get_folder_list(synStore_obj, project_synID)
      folders_namedList <- c()
      for (i in seq_along(folder_list)) {
        folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
      }

      folder_synID <- folders_namedList[[selected_folder]]

      fpath <- get_storage_manifest_path(input$cookie, folder_synID)
      if ( is.null(fpath)) {
        toggle('text_div3')
        output$text3 <- renderUI({
          tags$b("No previously uploaded manifest was found")
        })
        removeNotification(id = "processing")
      } else {
        manifest_url <- populateModelManifest(paste0("HTAN_", input$template_type), fpath, input$template_type )
        toggle('text_div3')

        output$text3 <- renderUI({
        tags$a(href = manifest_url, manifest_url, target="_blank")
        })
        removeNotification(id = "processing")
      }
    }
  )

  ### renders fileInput ui
  output$fileInput_ui <- renderUI({

    fileInput("file1", "Upload CSV File",
                          accept=c('text/csv', 
                                    'text/comma-separated-values', 
                                    '.csv'))
  })

  ## reads csv file and previews
  rawData <- eventReactive(input$file1, {
    readr::read_csv(input$file1$datapath, na = c("", "NA"))
  })
  
observeEvent(
  rawData(), 
  {
    output$tbl <- DT::renderDT({
      datatable(rawData(), options = list(lengthChange = FALSE, scrollX = TRUE)
        )
      })

  }
)

  ### toggles validation status when validate button pressed
  observeEvent(
    input$validate, {
      annotation_status <- validateModelManifest(input$file1$datapath, input$template_type)
      toggle('text_div2')

      showNotification(id="processing", "Processing...", duration = NULL, type = "default" )

      if ( length(annotation_status) != 0 ) { ## if error not empty aka there is an error
        filled_manifest <- populateModelManifest(paste0("HTAN_",input$template_type), input$file1$datapath, input$template_type)

        ### create list of string names for the long error messages
        str_names <- sprintf("str_%d", seq(length(annotation_status)))
        ### list to save errors
        in_vals <- sprintf("input_%d", seq(length(annotation_status)))
        ### create error messages
        for (i in seq_along(annotation_status)) {
          row <- annotation_status[[i]][1]
          column <- annotation_status[[i]][2]
          in_val <- annotation_status[[i]][3]
          allowed_vals <- annotation_status[[i]][4]

          ### if empty value change to NA
          if (unlist(in_val) == "") {
            in_val <- NA
            str_names[i] <- paste("Spreadsheet row <b>",
                                  row, "</b>column <b>", column,
                                  "</b>your value <b>", in_val,
                                  "</b> is not one of of:", allowed_vals, sep=" ")
            in_vals[i] <- in_val
          } else {
            str_names[i] <- paste("Spreadsheet row <b>",
                                  row, "</b>column <b>", column,
                                  "</b>your value <b>", in_val,
                                  "</b> is not one of:", allowed_vals, sep=" ")
            in_vals[i] <- in_val
          }
        }

        ### format output text
        output$text2 <- renderUI ({
          HTML("Your metadata is invalid according to the data model.<br/> ",
               "See errors at: <br/>",
               paste0(sprintf("%s", str_names), collapse = "<br/>"),
               "<br/>Edit your data locally or ",
               paste0('<a href="', filled_manifest, '">on Google Sheets</a>')
               )
### tags$a(href = manifest_url, manifest_url, target="_blank") add
        })
        ### update DT view with incorrect values
        ### currently only one column, requires backend support of multiple
        output$tbl <- DT::renderDT({
          datatable(rawData(),
                    options = list(lengthChange = FALSE, scrollX = TRUE)
          ) %>% formatStyle(unlist(column),
                            backgroundColor = styleEqual(unlist(in_vals), rep("yellow", length(in_vals))) ) ## how to have multiple errors
        })
        removeNotification(id = "processing")
      } else {
        output$text2 <- renderUI ({
          HTML("Your metadata is valid!")
        })
        removeNotification(id = "processing")
        ### show submit button
        output$submit <- renderUI({
          actionButton("submitButton", "Submit to Synapse")
        })

      }
    }
  )

  ###submit button
  observeEvent(
    input$submitButton, {
      
      showNotification(id="processing","Submitting...", duration = NULL, type = "default" )

      ### reads in csv and adds entityID, then saves it as synapse_storage_manifest.csv in folder
      infile <- readr::read_csv(input$file1$datapath, na = c("", "NA"))
      
      ### already has entityId
      if ("entityId" %in% colnames(infile)) {
        write.csv(infile, file= "./files/synapse_storage_manifest.csv", quote = FALSE, row.names = FALSE, na = "")
        
      } else{ # if not get ids
        selected_folder <- input$dataset
        selected_project <- input$var
        
        project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
        folder_list <- get_folder_list(synStore_obj, project_synID)
        folders_namedList <- c()
        for (i in seq_along(folder_list)) {
          folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
        }
        
        folder_synID <- folders_namedList[[selected_folder]]
        
        file_list <- get_file_list(synStore_obj, folder_synID)
        
        file_namedList <- c()
        for (i in seq_along(file_list)) {
          file_namedList[file_list[[i]][[2]]] <- file_list[[i]][[1]]
        }
        
        files_df <- stack(file_namedList)
        colnames(files_df) <- c("entityId", "Filename" )
        files_entity <- inner_join(infile, files_df, by = "Filename")
        
        write.csv(files_entity, file= "./files/synapse_storage_manifest.csv", quote = FALSE, row.names = FALSE, na = "")
      }
      selected_project <- input$var
      selected_folder <- input$dataset

      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project

      folder_list <- get_folder_list(synStore_obj, project_synID)
      folders_namedList <- c()
      for (i in seq_along(folder_list)) {
        folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
      }

      folder_synID <- folders_namedList[[selected_folder]]

      print(folder_synID)

      ### assocites metadata with data and returns manifest id
      manifest_id <- get_manifest_syn_id(synStore_obj, "./files/synapse_storage_manifest.csv", folder_synID)
      print(manifest_id)
      manifest_path <- paste0("synapse.org/#!Synapse:", manifest_id)
      ### if uploaded provide message
      if ( startsWith(manifest_id, "syn") == TRUE) {
        removeNotification(id = "processing")
        showNotification( id= "success",  paste0("Submit Manifest to: ", manifest_path), duration = NULL, type = "message")
        rm("./files/synapse_storage_manifest.csv")
        
        ### clear inputs 
        output$text2 <- renderUI ({
          HTML("")
        })
        output$submit <- renderUI({
        })

        ### rerenders fileinput UI
        output$fileInput_ui <- renderUI({
        fileInput("file1", "Upload CSV File",
                                accept=c('text/csv', 
                                        'text/comma-separated-values', 
                                        '.csv'))
      })
       ### renders empty df
      output$tbl <- DT::renderDT(
        datatable(as.data.frame(matrix(0, ncol = 0, nrow = 0)))
        )


      } else {
        showNotification(id = "error", paste0("error ", manifest_id ), duration = NULL, type = "error")
        rm("/tmp/synapse_storage_manifest.csv")
      }
      })

    # output$center_summary <- DT::renderDT({
    #   proj_folder_manifest_cells %>% 
    #     select(
    #       `Dataset ` = name,
    #       Project = project_name,
    #       total_cells,
    #       filled_cells,
    #       percent_filled,
    #       component2,
    #       total_cells2,
    #       filled_cells2,
    #       percent_filled2,
    #       `Synapse Project Folder` = `Synapse Project Folder`
    #     ) %>% 
    #     datatable(
    #       # selection = list(
    #       #   mode = 'single'
    #       # ),
    #       # options = list(
    #       #   scrollX = TRUE,
    #       #   autoWidth = F,
    #       #   dom = "tip"
    #       # ),
    #       # rownames = FALSE
    #     )  
    # }, server = FALSE )

    # })


  }


shinyApp(ui, server)
