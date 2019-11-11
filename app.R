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
    menuItem("Submit & Validate Metadata", tabName = "upload", icon = icon("upload")), 
    menuItem("Annotation Status Summaries", tabName = "anno_summaries", icon = icon("tasks") )
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
                    choices = list("Minimal Metadata") #, "scRNAseq") ## add mapping step from string to input
                  ) ## HTAPP to Minimal Metadata
                ),
                box(
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12, 
                  title = "All Dataset Annotation by Component Completion",
                  plotOutput("dash_plot", height = "500px") %>% shinycssloaders::withSpinner()
                  # plotly::plotlyOutput("dash_plot", height = "600px")
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
      ),
      tabItem(tabName = "anno_summaries",
              h2("Submit & Validate a Filled Metadata Template"),
              fluidRow(
                box(
                  title = "Annotation Snapshot", 
                  width = 12, 
                  status = "primary", 
                  solidHeader = TRUE, 
                  DT::dataTableOutput('center_summary')
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

  observeEvent(input$cookie, {  
    showNotification(id="dash_gather", "Gathering your Data...", duration = NULL, type = "default" )
        ###================ gets the manifest cells table 
    tab <- syn_tableQuery("select * from syn20446927", resultsAs='csv')
    tab <- read.csv(tab$filepath)
    # get just folders and match parent ID to project name
    projects_df <- stack(projects_namedList)
    colnames(projects_df) <- c("parentId", "project_name")
    proj_folder_df <- inner_join(tab %>% filter(type == "folder"), projects_df)
    
    all_folders_list <- as.character(proj_folder_df$id)
    
    ### get manifest paths per folder
    folders_manifest_df <- data.frame()
    for (i in seq_along(all_folders_list)) {
      # folders_manifest_df[]
      print(i)
      print(all_folders_list[i])
      folders_manifest_df[i,1] <- all_folders_list[i]
      path <- get_storage_manifest_path(input$cookie , all_folders_list[i])
      if ( is.null(path) ) { ## if no manifest is uploaded 
        folders_manifest_df[i,2] <- NA  
      } else {
        folders_manifest_df[i,2] <- path
      }
    }
    
    colnames(folders_manifest_df) <- c("id", "manifest_path")
    
    proj_folder_manifest <- inner_join(folders_manifest_df, proj_folder_df, by = "id")
    
    ### get metrics for each manifest
  manifest_cells <- data.frame()
  for (i in seq_along(proj_folder_manifest$id)) {
    print(i)
    manifest_path <- proj_folder_manifest$manifest_path[i]
    manifest_cells[i, 1] <- proj_folder_manifest$id[i]
    manifest_cells[i, 2] <- manifest_path
    
    if ( !is.na(manifest_path)) {
      manifest_df <- read.csv(manifest_path)
      
      ### subset cols by component type ASSAY
      assay_cols <- manifest_df[, c("Cancer.Type", "Library.Construction.Method")]
      
      # how to count empty cells vs all cells
      dim_val <- dim(assay_cols)
      total_cells <- dim_val[1] * dim_val[2]
      empty_cells <- table(is.na(assay_cols))
      per_filled <- (empty_cells[1] / total_cells )* 100
      
      manifest_cells[i,3] <- "Assay"
      manifest_cells[i,4] <- total_cells
      manifest_cells[i,5] <- empty_cells[1] ## filled, empty= F
      manifest_cells[i,6] <- per_filled
      
      
      HTAN_cols <- manifest_df[, c("HTAN.Participant.ID", "HTAN.Sample.ID")]
      # how to count empty cells vs all cells
      dim_val <- dim(HTAN_cols)
      total_cells <- dim_val[1] * dim_val[2]
      empty_cells <- table(is.na(HTAN_cols))
      per_filled <- (empty_cells[1] / total_cells )* 100
      
      manifest_cells[i,7] <- "HTAN_IDs"
      manifest_cells[i,8] <- total_cells
      manifest_cells[i,9] <- empty_cells[1] ## filled, empty= F
      manifest_cells[i,10] <- per_filled
    } else {
      # print(proj_folder_manifest$id[i])
      manifest_cells[i,3] <- "Assay"
      manifest_cells[i,4] <- 0
      manifest_cells[i,5] <- 0
      manifest_cells[i,6] <- 0

      manifest_cells[i,7] <- "HTAN_IDs"
      manifest_cells[i,8] <- 0
      manifest_cells[i,9] <- 0 ## filled, empty= F
      manifest_cells[i,10] <- 0
      
    }
  }

  ### NAs to 0 but not for manifest path
  manifest_cells[,-c(2)][is.na((manifest_cells[,-c(2)]))] <- 0

  colnames(manifest_cells) <- c("id", "manifest_path", "component", "total_cells","filled_cells", "percent_filled", "component2", "total_cells2","filled_cells2", "percent_filled2")

  proj_folder_manifest_cells <- right_join(manifest_cells, proj_folder_manifest )
  proj_folder_manifest_cells <<- proj_folder_manifest_cells %>% 
    mutate('Synapse Project Folder' = id) %>% 
    create_synapse_links( list('Synapse Project Folder' = "id") )

    minimal <- proj_folder_manifest_cells[, c("project_name", "name", "component", "total_cells","filled_cells", "percent_filled", "component2", "total_cells2","filled_cells2", "percent_filled2")]

    colnames(minimal)[7:10] <- c("component", "total_cells","filled_cells", "percent_filled")

    minimal_long <- rbind(minimal[,1:6], minimal[,c(1:2,7:10) ])

    ### get the wrapped project names to fit
    minimal_long <- mutate(minimal_long, project = str_wrap(project_name, width = 10))

    # p_dash <- ggplot(minimal_long, aes(x= name, y = percent_filled, fill = component)) + 
    #       geom_histogram(stat= "identity", position = "dodge")  + 
    #       coord_flip() +
    #       facet_grid(project ~ ., 
    #                 scales = "free_y", space = "free_y",
    #                 drop = TRUE) +
    #       guides(alpha = FALSE, fill = FALSE, colour = guide_legend(title = "Annotation Component")) +
    #       xlab("") +
    #       ylab("Percent Annotated") +
    #       theme_bw() +
    #       theme(strip.text.y = element_text(face = "bold", angle = 270),
    #             legend.title = element_blank(),
    #             axis.text.x = element_text(angle = 0, hjust = 1),
    #             strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")))
    
    # g <- ggplotGrob(p_dash)
    # g$heights[6] <- unit(2, "cm")
    # for(i in 13:15) g$grobs[[i]]$heights = unit(1, "npc") # Set height of grobs

    # grid.newpage()
    # grid.draw(g)

    # output$dash_plot <- plotly::renderPlotly({
    #     ggplotly( p_dash ) %>%
    #     layout(legend = list(orientation = 'h',
    #                           y = 1, x = 0, yanchor = "bottom"),
    #             margin = list(l = 300, b = 60, r = 60)) %>% 
    #     plotly::config(displayModeBar = F)  ### plotly doesn't show single dataset names in facets?? use static plot for now
    # })
    output$dash_plot <- renderPlot({
      ggplot(minimal_long, aes(x= name, y = percent_filled, fill = component)) + 
          geom_histogram(stat= "identity", position = "dodge")  + 
          coord_flip() +
          facet_grid(project ~ ., 
                    scales = "free_y", space = "free_y",
                    drop = TRUE) +
          # guides(fill = guide_legend(title = "Annotation Component")) +
          labs(fill = "Fill legend label") +
          xlab("") +
          ylab("Percent Annotated") +
          theme_bw() +
          theme(strip.text.y = element_text(face = "bold", angle = 270),
                legend.title = element_blank(),
                axis.text.x = element_text(angle = 0, hjust = 1),
                strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
                text = element_text(size=12),
                legend.position = "top")
    })

    removeNotification(id = "dash_gather")
    
  })



  ### rename the input template type to HTAPP
  in_template_type <- "HTAPP"

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

      manifest_url <- getModelManifest(paste0("HTAN_",in_template_type), in_template_type, filenames = filename_list )
      toggle('text_div')

      ### if want a progress bar need more feedback from API to know how to increment progress bar
      # withProgress(message = "connecting to Google Sheets API")

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
        manifest_url <- populateModelManifest(paste0("HTAN_",in_template_type), fpath, in_template_type )
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
      annotation_status <- validateModelManifest(input$file1$datapath, in_template_type)
      toggle('text_div2')

      showNotification(id="processing", "Processing...", duration = NULL, type = "default" )

      if ( length(annotation_status) != 0 ) { ## if error not empty aka there is an error
        filled_manifest <- populateModelManifest(paste0("HTAN_",in_template_type), input$file1$datapath, in_template_type)

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
                                  "</b> is not an allowed value of:", allowed_vals, sep=" ")
            in_vals[i] <- in_val
          } else {
            str_names[i] <- paste("Spreadsheet row <b>",
                                  row, "</b>column <b>", column,
                                  "</b>your value <b>", in_val,
                                  "</b> is not an allowed value of:", allowed_vals, sep=" ")
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

    output$center_summary <- DT::renderDT({
      proj_folder_manifest_cells %>% 
        select(
          `Dataset ` = name,
          Project = project_name,
          total_cells,
          filled_cells,
          percent_filled,
          component2,
          total_cells2,
          filled_cells2,
          percent_filled2,
          `Synapse Project Folder` = `Synapse Project Folder`
        ) %>% 
        datatable(
          # selection = list(
          #   mode = 'single'
          # ),
          # options = list(
          #   scrollX = TRUE,
          #   autoWidth = F,
          #   dom = "tip"
          # ),
          # rownames = FALSE
        )  
    }, server = FALSE )

    # })


  }


shinyApp(ui, server)
