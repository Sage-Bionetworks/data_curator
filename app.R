library(shiny)
library(shinyjs)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(stringr)
library(DT)
library(jsonlite)


source(file= "./functions.R")

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    titleWidth = 250,
    title = "Data Curator" ,
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
    menuItem("Submit & Validate Metadata", tabName = "upload", icon = icon("upload"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style("#shiny-notification-error {height: 500px; padding :20px; display: table-cell}" #makes taller so the error will fit
      ),
      singleton(
        includeScript("www/readCookie.js")
      )),
    tabItems(
      # tabItem(tabName = "dashboard",
      #         h2("Welcome to your dataset dashboard!"),
      #         fluidRow(
      #           box(
      #             status = "primary",
      #             solidHeader = TRUE,
      #             title = "Datasets and Annotations",
      #             width = 12 #, 
      #             # DT::DTOutput("projData"),
      #           )
      #         )),
      # First tab content
      tabItem(tabName = "data",
              h2("Set Dataset and Template for Curation"),
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width= 6,
                  title = "Choose a Project and Dataset: ",
                  selectInput(inputId = "var", label = "Project:",
                              choices = names(projects_namedList) ),
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
                    choices = list("Minimal Metadata") #, "scRNAseq") ## add mapping step from string to input
                  ) ## HTAPP to Minimal Metadata
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
                  fileInput("csvFile", "Upload CSV File",
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv'))
                  ) ,
                box(
                  title = "Metadata Preview",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12, 
                  DT::DTOutput("rawData") #,
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
  ### synapse cookies
  session$sendCustomMessage(type = "readCookie", message = list())
  observeEvent(input$cookie, {
    
    syn_login(sessionToken=input$cookie, rememberMe = TRUE) 
    
    # output$title <- renderUI({
    #   titlePanel(sprintf("Welcome, %s", synGetUserProfile()$userName))
    # })
  })
    
  ### rename the input template type to HTAPP 
  in_template_type <- "HTAPP" 
    
  ### folder datasets 
  output$folders = renderUI({
    selected_project <- input$var
    
    project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
    folder_list <- get_folder_list(project_synID)
    folders_namedList <- c()
    for (i in seq_along(folder_list)) {
      folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
    }
    folderNames <- names(folders_namedList)
    selectInput(inputId = "dataset", label = "Dataset:", folderNames)
  })
  
  ###toggles link when download button pressed
  observeEvent(
    input$download, {
      selected_folder <- input$dataset
      selected_project <- input$var
      
      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
      folder_list <- get_folder_list(project_synID)
      folders_namedList <- c()
      for (i in seq_along(folder_list)) {
        folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
      }
      
      folder_synID <- folders_namedList[[selected_folder]]
      
      file_list <- get_file_list(folder_synID)

      file_namedList <- c()
      for (i in seq_along(file_list)) {
        file_namedList[file_list[[i]][[2]]] <- file_list[[i]][[1]]
      }
      filename_list <- names(file_namedList)
      
      manifest_url <- getModelManifest(in_template_type, filenames = filename_list )
      toggle('text_div')
      
      ### if want a progress bar need more feedback from API to know how to increment progress bar
      # withProgress(message = "connecting to Google Sheets API") 
      
      output$text <- renderUI({
        tags$a(href = manifest_url, manifest_url, target="_blank") ### add link to data dictionary
      })
    }
)
  
  ###toggles link to previous manifest when pressed 
  observeEvent(
    input$link, {
      selected_project <- input$var
      selected_folder <- input$dataset
      
      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
      folder_list <- get_folder_list(project_synID)
      folders_namedList <- c()
      for (i in seq_along(folder_list)) {
        folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
      }
      
      folder_synID <- folders_namedList[[selected_folder]]
      
      fpath <- get_storage_manifest_path(folder_synID)
      if ( is.null(fpath)) {
        toggle('text_div3')
        output$text3 <- renderUI({
          tags$b("No previously uploaded manifest was found")
        })
      } else {
        manifest_url <- populateModelManifest(fpath, in_template_type )
        toggle('text_div3')

        output$text3 <- renderUI({
        tags$a(href = manifest_url, manifest_url, target="_blank")
        })
      }
      

    }
  )
  
  ### reads csv file
  rawData <- eventReactive(input$csvFile, {
    readr::read_csv(input$csvFile$datapath, na = c("", "NA"))
  })
  
  ### DT 
  output$rawData <- DT::renderDT({ 
    datatable(rawData(),
    options = list(lengthChange = FALSE, scrollX = TRUE)
    ) 
  
    })

  ### toggles validation status when validate button pressed 
  observeEvent(
    input$validate, {
      annotation_status <- validateModelManifest(input$csvFile$datapath, in_template_type) 
      toggle('text_div2')
      if ( length(annotation_status) != 0 ) { ## if error not empty aka there is an error
        filled_manifest <- populateModelManifest(input$csvFile$datapath, in_template_type) 
        
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
                                  "</b> is not an allowed value from:", allowed_vals, sep=" ")
            in_vals[i] <- in_val
          } else {
            str_names[i] <- paste("Spreadsheet row <b>",
                                  row, "</b>column <b>", column,
                                  "</b>your value <b>", in_val,
                                  "</b> is not an allowed value from:", allowed_vals, sep=" ")
            in_vals[i] <- in_val
          }
        }
        
        ### format output text
        output$text2 <- renderUI ({
          HTML("Your metadata is invalid according to the data model.<br/> ",
               "See errors at: <br/>",
               paste0(sprintf("%s", str_names), collapse = "<br/>"),
               "<br/>Edit your data locally or ",
               paste0('<a href="', filled_manifest, '">here</a>')
               )

        })
        ### update DT view with incorrect values
        ### currently only one column, requires backend support of multiple
        output$rawData <- DT::renderDT({ 
          datatable(rawData(),
                    options = list(lengthChange = FALSE, scrollX = TRUE)
          ) %>% formatStyle(unlist(column), 
                            backgroundColor = styleEqual(unlist(in_vals), rep("yellow", length(in_vals))) ) ## how to have multiple errors 
        })
        
      } else {   
        output$text2 <- renderUI ({
          HTML("Your metadata is valid!")
        })
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
      ### reads in csv and adds entityID, then saves it as synapse_storage_manifest.csv in tmp
      infile <- readr::read_csv(input$csvFile$datapath, na = c("", "NA"))
      selected_folder <- input$dataset
      selected_project <- input$var
      
      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
      folder_list <- get_folder_list(project_synID)
      folders_namedList <- c()
      for (i in seq_along(folder_list)) {
        folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
      }
      
      folder_synID <- folders_namedList[[selected_folder]]
      
      file_list <- get_file_list(folder_synID)
      
      file_namedList <- c()
      for (i in seq_along(file_list)) {
        file_namedList[file_list[[i]][[2]]] <- file_list[[i]][[1]]
      }
      
      files_df <- stack(file_namedList)
      colnames(files_df) <- c("entityId", "Filename" )
      files_entity <- inner_join(infile, files_df, by = "Filename")
      write.csv(files_entity, file= "/tmp/synapse_storage_manifest.csv", quote = FALSE, row.names = FALSE, na = "")
      
      ### copies file to rename it
      # file.copy(input$csvFile$datapath, "/tmp/synapse_storage_manifest.csv")
      
      selected_project <- input$var
      selected_folder <- input$dataset
      
      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
      
      folder_list <- get_folder_list(project_synID)
      folders_namedList <- c()
      for (i in seq_along(folder_list)) {
        folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
      }
      
      folder_synID <- folders_namedList[[selected_folder]]
      
      print(folder_synID)
      
      ### assocites metadata with data and returns manifest id
      manifest_id <- get_manifest_syn_id("/tmp/synapse_storage_manifest.csv", folder_synID)
      print(manifest_id)
      manifest_path <- paste0("synapse.org/#!Synapse:", manifest_id)
      ### if uploaded provide message
      if ( startsWith(manifest_id, "syn") == TRUE) {
        showNotification( id= "success",  paste0("Uploaded Manifest to: ", manifest_path), duration = NULL, type = "message")
      } else {
        showNotification(paste0("error ", manifest_id ), duration = NULL, type = "error")
      }
      })
  
    }
  # )
  


  # )
# }

shinyApp(ui, server)