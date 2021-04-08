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
library(shinypop)
library(waiter)

#########global
use_condaenv('data_curator_env', required = TRUE)
reticulate::import("sys")

source_python("synLoginFun.py")
source_python("metadataModelFuns.py")

#########

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    titleWidth = 250,
    title = "NF Data Curator",
    tags$li(class = "dropdown",
            tags$a(href = "https://nf-osi.github.io/", target = "_blank", ##insert links and logos of your choice, this is just an example
                   tags$img(height = "50px", alt = "NF LOGO",
                            src = "nfosi_logo_crop.png")))
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs", 
      menuItem("Instructions", tabName = "instructions", icon = icon("book-open")),
      menuItem("Select your Dataset", tabName = "data", icon = icon("mouse-pointer")),
      menuItem("Get Metadata Template", tabName = "template", icon = icon("table")),
      menuItem("Submit & Validate Metadata", tabName = "upload", icon = icon("upload")),  
      HTML('<footer>
            Managed by the</br>
            Neurofibromatosis Open Science Initiative</br>
            Created by the Human Tumor Atlas Network</br>
            (U24-CA233243-01)</br>
            Powered by Sage Bionetworks
        </footer>')
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      singleton(
        includeScript("www/readCookie.js")
      )),
    uiOutput("title"),
    use_notiflix_report(), 
    tabItems(
      # First tab content
      tabItem(tabName = "instructions",
              h2("Instructions for the Data Curator App:"),
              h3("1. Go to", strong("Select your Dataset"), "tab - select your project; choose the folder where your data are stored; and, choose your metadata template based on the type of data you’ll be annotating (e.g., Genomics Assay, Imaging Assay, etc.)."),
              h3("2. Go to", strong("Get Metadata Template"), "tab - click the button to generate a metadata template in Google Sheets; once generated (this may take a few moments), click the link that appears, fill out the sheet (columns in blue are required; columns in yellow are optional), and then download the file as a CSV."),
              h3("3. Go to", strong("Submit and Validate Metadata"), "tab - upload your filled CSV and validate your metadata. If you receive errors: correct them in the CSV, re-upload, and revalidate until you receive no more errors. Once your CSV is error-free, please submit it to nf-osi@sagebionetworks.org."),
              h2("About"),
              h3("The NF Data Curator App allows you to annotate your data files — stored in Sage Bionetworks’ data storage platform, Synapse — using metadata templates with preset attributes (a.k.a, keys or column headers) and values. The app also provides validation, to ensure metadata templates are completed according to the Neurofibromatosis Open Science Initiative’s (NF-OSI) metadata dictionary. Ultimately, this process enables data discovery on the NF Data Portal, once data are released."),
              h2("Note"),
              h3("Please note that this app is still in beta, and may not always work as designed. If you receive a ‘disconnected from the server’ error — this could mean that you’re not logged into Synapse (you must be logged in to use this app), or that something went wrong with the app — we apologize if this happens! You may have to reload the app and try again. If you experience repeated issues, please contact us at nf-osi@sagebionetworks.org. In addition, the third tab, Submit and Validate Metadata, is not fully functional. You won’t be able to submit your completed template to Synapse using this app. For now, please email your completed manifest to nf-osi@sagebionetworks.org.")     
      ),
      # second tab content
      tabItem(tabName = "data",
              h2("Set Dataset and Metadata Template for Curation"),
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  title = "Choose a Project and Folder: ",
                  selectizeInput(inputId = "var", label = "Project:",
                                 choices = "Generating..."),
                  uiOutput('folders'),
                  helpText("If your recently updated folder does not appear, please wait for Synapse to sync and refresh")
                ),
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  title = "Choose a Metadata Template Type: ",
                  uiOutput("manifest_display_name")
                )
              )
      ),
      # Third tab item
      tabItem(tabName = "template",
              useShinyjs(),
              h2("Download Template for Selected Folder"),
              fluidRow(
                box(
                  title = "Get Link, Annotate, and Download Template as CSV",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("download", "Click to Generate Google Sheets Template"),
                  hidden(
                    div(
                      id = 'text_div',
                      height = "100%",
                      htmlOutput("text"),
                      style = "font-size:18px; background-color: white; border: 1px solid #ccc; border-radius: 3px; margin: 10px 0; padding: 10px"
                    )
                  ),
                  helpText("This link will leads to an empty template or your previously submitted template with new files if applicable.")
                ) 
                
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "upload",
              h2("Submit & Validate a Filled Metadata Template"),
              fluidRow(
                box(
                  title = "Upload Filled Metadata as a CSV",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12,
                  uiOutput('fileInput_ui')
                ),
                box(
                  title = "Metadata Preview",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12,
                  DT::DTOutput("tbl"),
                  helpText("Google spreadsheet row numbers are incremented from this table by 1")
                ),
                box(
                  title = "Validate Filled Metadata",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("validate", "Validate Metadata"),
                  hidden(
                    div(id = 'text_div2', 
                        height = "100%",
                        htmlOutput("text2"),
                        style = "font-size:18px; background-color: white; border: 1px solid #ccc; border-radius: 3px; margin: 10px 0; padding: 10px"
                    ),
                    DT::DTOutput("tbl2"),
                    actionButton("gsheet_btn", "  Click to Generate Google Sheet Link", icon = icon("table")),
                    div(id = 'gsheet_div', 
                        height = "100%",
                        htmlOutput("gsheet_link"),
                        style = "font-size:18px; background-color: white; border: 1px solid #ccc; border-radius: 3px; margin: 10px 0; padding: 10px"
                    )
                  ),
                  helpText(
                    HTML("If you have an error, please try editing locally or on google sheet.<br/>
                         Reupload your CSV and press the validate button as needed.")
                  )
                ),
                box(title = "Submit Validated Metadata to Synapse",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    uiOutput("submit")
                )
              )
      )
    ),
    uiOutput("Next_Previous"),
    
    ## waiter loading screen
    use_waiter(),
    waiter_show_on_load(
      html = tagList(
        img(src = "loading.gif"),
        h4("Retrieving Synapse information...")
      ),
      color = "#424874"
    )
  )
)

server <- function(input, output, session) {
  
  ########### session global variables
  reticulate::source_python("synStore_Session.py")
  
  ### read config in
  config <- jsonlite::fromJSON('www/config.json')
  
  ### logs in and gets list of projects they have access to
  synStore_obj <- NULL
  # get_projects_list(synStore_obj)
  projects_list <- c()
  
  projects_namedList <- c()
  
  proj_folder_manifest_cells <- c()
  
  folder_synID <- NULL
  
  filename_list <- c()
  ############
  
  ### synapse cookies
  session$sendCustomMessage(type = "readCookie", message = list())
  
  ### initial login front page items
  observeEvent(input$cookie, {
    
    ## login and update session; otherwise, notify to login to Synapse first
    tryCatch({
      
      ### logs in 
      syn_login(sessionToken = input$cookie, rememberMe = FALSE)
      
      ### welcome message
      output$title <- renderUI({
        titlePanel(h4(sprintf("Welcome, %s", syn_getUserProfile()$userName)))
      })
      
      ### updating global vars with values for projects
      # synStore_obj <<- syn_store(config$main_fileview, token = input$cookie)
      synStore_obj <<- syn_store(token = input$cookie)

      # get_projects_list(synStore_obj)
      projects_list <<- syn_store$getStorageProjects(synStore_obj)
      
      for (i in seq_along(projects_list)) {
        projects_namedList[projects_list[[i]][[2]]] <<- projects_list[[i]][[1]]
      }
      
      ### updates project dropdown
      updateSelectizeInput(session, 'var', choices = sort(names(projects_namedList)))
<<<<<<< HEAD

      ### 
      admin_team_table <- config$admin_team_table
      
      user_teams <- syn_restGET(glue::glue("/user/{syn_getUserProfile()[['ownerId']]}/team?limit=10000"))$results 
      
      # the teams that user belongs to
      all_teams <- purrr::map_chr(user_teams, function(x) x$id)
      
      # the teams with override access
      dashboard_teams <- syn_tableQuery(glue::glue("select * from {admin_team_table}"))$asDataFrame()
      allowed_teams <- sapply(dashboard_teams$TeamID, jsonlite::fromJSON)

      #final allowed agencies
      allowed_teams <- all_teams[all_teams %in% allowed_teams]
      
      if(length(allowed_teams)>0){
        override <<- TRUE
      }else{
        override <<- FALSE
      }
      
      ### update waiter loading screen once login successful
=======

    ### update waiter loading screen once login successful
>>>>>>> 4356f4913f936888401b1ca6f7fae48615a6f269
      waiter_update(
        html = tagList(
          img(src = "synapse_logo.png", height = "120px"),
          h3(sprintf("Welcome, %s!", syn_getUserProfile()$userName))
        )
      )
      Sys.sleep(2)
      waiter_hide()
    }, error = function(err) {
      Sys.sleep(2)
      waiter_update(
        html = tagList(
          img(src = "synapse_logo.png", height = "120px"),
          h3("Looks like you're not logged in!"),
          span("Please ", a("login", href = "https://www.synapse.org/#!LoginPlace:0", target = "_blank"), 
               " to Synapse, then refresh this page.")
        )
      )
    })
  })
  
  
  ###### BUTTONS STUFF  !!! remove last arrow 
  Previous_Button=tags$div(actionButton("Prev_Tab",HTML('
<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
')))
  Next_Button=div(actionButton("Next_Tab",HTML('
<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>
')))
  
  list_tabs <- c("instructions", "data", "template", "upload")
  
  output$Next_Previous <- renderUI({
    
    tab_list=list_tabs
    if ( input[["tabs"]] == "upload" ){
      # column(1,offset=1,Previous_Button)
    } else if (input[["tabs"]] == "instructions" ) {
      column(1,offset = 10,Next_Button)
    } else {
      div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
    }
  })
  
  observeEvent(input$Prev_Tab,
               {
                 tab_list=list_tabs
                 current_tab=which(tab_list==input[["tabs"]])
                 updateTabItems(session,"tabs",selected=tab_list[current_tab-1])
               })
  
  observeEvent(input$Next_Tab,
               {
                 tab_list=list_tabs
                 current_tab=which(tab_list==input[["tabs"]])
                 updateTabItems(session,"tabs",selected=tab_list[current_tab+1])
               })
  
  ####### BUTTONS END
  
  ### lists folder datasets if exists in project
  observeEvent(ignoreNULL = TRUE, ignoreInit = TRUE,
               input$var, {
                 output$folders = renderUI({
                   selected_project <- input$var
                   
                   # if selected_project not empty
                   if (!is.null(selected_project)) {
                     project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
                     
                     ### gets folders per project
                     #folder_list <- syn_store$getStorageDatasetsInProject(synStore_obj, project_synID)
                     folder_df <- syn_tableQuery(sprintf("select name, id from %s where type = 'folder' and projectId = '%s'", config$main_fileview, project_synID))$asDataFrame()
                     
                       folders_namedList <- setNames(as.list(folder_df$id), folder_df$name)
                     folderNames <- names(folders_namedList)
                     
                     ### updates foldernames
                     selectInput(inputId = "dataset", label = "Folder:", choices = folderNames)
                   }
                 })
               })
  
### mapping from display name to schema name
schema_name  <- config$manifest_schemas$schema_name
display_name <- config$manifest_schemas$display_name

output$manifest_display_name <- renderUI({
	selectInput(inputId = "template_type",
                    label = "Template:",
                    choices = display_name)

})

observeEvent({input$dataset
              input$template_type
              }, {
                sapply(c('text_div', 'text_div2', 'tbl2', 'gsheet_btn', 'gsheet_div', 'submitButton'), FUN=hide)
})

schema_to_display_lookup <- data.frame(schema_name, display_name)

  # loading screen for template link generation
  manifest_w <- Waiter$new(
    html = tagList(
      spin_plus(), br(),
      h4("Generating link...")
    ),
    color = "rgba(66, 72, 116, .9)"
  )
  
  ###shows new metadata link when get gsheets template button pressed OR updates old metadata if is exists 
  observeEvent(
    input$download, {
      
      manifest_w$show()
      
      selected_folder <- input$dataset
      selected_project <- input$var
      
      ###lookup schema template name 
      template_type_df <- schema_to_display_lookup[match(input$template_type, schema_to_display_lookup$display_name), 1, drop = F ]
      template_type <- as.character(template_type_df$schema_name)
      
      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
      
      folder_list <- syn_store$getStorageDatasetsInProject(synStore_obj, project_synID)
      folders_namedList <- c()
      for (i in seq_along(folder_list)) {
        folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
      }
      folder_synID <- folders_namedList[[selected_folder]]
      
      ### checks if a manifest already exists
      existing_manifestID <- syn_store$updateDatasetManifestFiles(synStore_obj, folder_synID)
      
      ### if there isn't an existing manifest make a new one 
      if (existing_manifestID == '') {
        file_list <- syn_store$getFilesInStorageDataset(synStore_obj, folder_synID)
        file_namedList <- c()
        for (i in seq_along(file_list)) {
          file_namedList[file_list[[i]][[2]]] <- file_list[[i]][[1]]
        }
        filename_list <- names(file_namedList)
        
        
        manifest_url <- metadata_model$getModelManifest(paste0(config$community," ", input$template_type), template_type, filenames = as.list(filename_list))
        ### make sure not scalar if length of list is 1 in R
        ## add in the step to convert names later ###
        
        output$text <- renderUI({
          tags$a(href = manifest_url, manifest_url, target = "_blank") ### add link to data dictionary when we have it ###
        })
      } else {
        ### if the manifest already exists
        manifest_entity <- syn_get(existing_manifestID)
        # prepopulatedManifestURL = mm.populateModelManifest("test_update", entity.path, component)
        manifest_url <- metadata_model$populateModelManifest(paste0(config$community," ", input$template_type), manifest_entity$path, template_type)
        
        output$text <- renderUI({
          tags$a(href = manifest_url, manifest_url, target = "_blank")
        })
      }
<<<<<<< HEAD
      ## links shows in text box
      show('text_div')
      ### if want a progress bar need more feedback from API to know how to increment progress bar ###
      
      manifest_w$hide()
=======
      filename_list <- names(file_namedList)


      manifest_url <- metadata_model$getModelManifest(paste0(config$community," ", input$template_type), template_type, filenames = as.list(filename_list))
      ### make sure not scalar if length of list is 1 in R
      ## add in the step to convert names later ###

      output$text <- renderUI({
        tags$a(href = manifest_url, manifest_url, target = "_blank") ### add link to data dictionary when we have it ###
      })
    } else {
      ### if the manifest already exists
      manifest_entity <- syn_get(existing_manifestID)
      # prepopulatedManifestURL = mm.populateModelManifest("test_update", entity.path, component)
      manifest_url <- metadata_model$populateModelManifest(paste0(config$community," ", input$template_type), manifest_entity$path, template_type)

      output$text <- renderUI({
        tags$a(href = manifest_url, manifest_url, target = "_blank")
      })
    }
    ## links shows in text box
    show('text_div')
    ### if want a progress bar need more feedback from API to know how to increment progress bar ###
    
    manifest_w$hide()
>>>>>>> 4356f4913f936888401b1ca6f7fae48615a6f269
    }
  )
  
  ### renders fileInput ui
  output$fileInput_ui <- renderUI({
    
    fileInput("file1", "Upload CSV File",
              accept = c('text/csv',
                         'text/comma-separated-values',
                         '.csv'))
  })
  
  ### reads csv file and previews
  rawData <- eventReactive(input$file1, {
    infile <- readr::read_csv(input$file1$datapath, na = c("", "NA"))
    ### remove empty rows/columns where readr called it "X"[digit] for unnamed col
    infile <- infile[, !grepl('^X', colnames(infile))]
    infile <- infile[rowSums(is.na(infile)) != ncol(infile), ]
  })
  
<<<<<<< HEAD

=======
>>>>>>> 4356f4913f936888401b1ca6f7fae48615a6f269
  observeEvent(input$file1, {
    sapply(c('text_div2', 'tbl2', 'gsheet_btn', 'gsheet_div', 'submitButton'), FUN=hide)
  })
  
  ### renders in DT for preview 
  observeEvent(
    rawData(), {
      output$tbl <- DT::renderDT({
        datatable(rawData(), options = list(lengthChange = FALSE, scrollX = TRUE)
        )
      })
      
    }
  )
  
  ## loading screen for validating metadata
  validate_w <- Waiter$new(
    html = tagList(
      spin_plus(), br(),
      h4("Validating...")
    ),
    color = "rgba(66, 72, 116, .9)"
  )
  
  ### toggles validation status when validate button pressed
  observeEvent(
    input$validate, {
      
    validate_w$show()

    ###lookup schema template name 
    template_type_df <- schema_to_display_lookup[match(input$template_type, schema_to_display_lookup$display_name), 1, drop = F ]
    template_type <- as.character(template_type_df$schema_name)

    annotation_status <- metadata_model$validateModelManifest(input$file1$datapath, template_type)
    
    show('text_div2')


<<<<<<< HEAD
    if (length(annotation_status) != 0  & !isTRUE(override)) {
=======
    if (length(annotation_status) != 0) {
>>>>>>> 4356f4913f936888401b1ca6f7fae48615a6f269

      # mismatched template index
      inx_mt <- which(sapply(annotation_status, function(x) grepl("Component value provided is: .*, whereas the Template Type is: .*", x[[3]])))
      
      if (length(inx_mt) > 0) {  # mismatched error(s): selected template mismatched with validating template
        
        # get all mismatched components
        error_values <- sapply(annotation_status[inx_mt], function(x) x[[4]][[1]]) %>% unique()
        column_names <- "Component"
        
        # error messages for mismatch
        mismatch_c <- error_values %>% sQuote %>% paste(collapse = ", ")
        type_error <- paste0("The submitted metadata contains << <b>", mismatch_c, "</b> >> in the Component column, but requested validation for << <b>",  input$template_type, "</b> >>.")
        help_msg <- paste0("Please check that you have selected the correct template in the <b>Select your Dataset</b> tab and 
                            ensure your metadata contains <b>only</b> one template, e.g. ", input$template_type, ".")
        
        # get wrong columns and values for updating preview table
        errorDT <- data.frame(Column=sapply(annotation_status[inx_mt], function(i) i[[2]]),
                              Value=sapply(annotation_status[inx_mt], function(i) i[[4]][[1]]))

      } else {
        
        type_error <- paste0("The submitted metadata have ", length(annotation_status), " errors.")
        help_msg <- NULL

        errorDT <- data.frame(Column=sapply(annotation_status, function(i) i[[2]]),
                              Value=sapply(annotation_status, function(i) i[[4]][[1]]),
                              Error=sapply(annotation_status, function(i) i[[3]])) 
        # sort rows based on input column names
        errorDT <- errorDT[order(match(errorDT$Column, colnames(rawData()))),]

        # output error messages as data table
        show("tbl2")
        output$tbl2 <- DT::renderDT({
          datatable(errorDT, caption = "The errors are also highlighted in the preview table above.", 
                    rownames = FALSE, options = list(pageLength = 50, scrollX = TRUE, 
                                                     scrollY = min(50*length(annotation_status), 400),
                                                     lengthChange = FALSE, info = FALSE, searching = FALSE)
          )
        })
      }                                     
 
      validate_w$update(
        html = h3(sprintf("%d errors found", length(annotation_status)))
      )

      ### format output text
      output$text2 <- renderUI({
        tagList( 
          HTML("Your metadata is invalid according to the data model.<br/><br/>"),
          HTML(type_error, "<br/><br/>"),
          HTML(help_msg)
        )
      })
      
      ### update DT view with incorrect values
      ### currently only one column, requires backend support of multiple
      output$tbl <- DT::renderDT({
        datatable(rawData(),
                    options = list(lengthChange = FALSE, scrollX = TRUE)
          ) %>% formatStyle(errorDT$Column,
                            backgroundColor = styleEqual(errorDT$Value, rep("yellow", length(errorDT$Value) ) )) ## how to have multiple errors
      })
      
      show('gsheet_btn')
      
<<<<<<< HEAD
    } else if (length(annotation_status) != 0  & isTRUE(override)) {
      
      # mismatched template index
      inx_mt <- which(sapply(annotation_status, function(x) grepl("Component value provided is: .*, whereas the Template Type is: .*", x[[3]])))
      
      if (length(inx_mt) > 0) {  # mismatched error(s): selected template mismatched with validating template
        
        # get all mismatched components
        error_values <- sapply(annotation_status[inx_mt], function(x) x[[4]][[1]]) %>% unique()
        column_names <- "Component"
        
        # error messages for mismatch
        mismatch_c <- error_values %>% sQuote %>% paste(collapse = ", ")
        type_error <- paste0("The submitted metadata contains << <b>", mismatch_c, "</b> >> in the Component column, but requested validation for << <b>",  input$template_type, "</b> >>.")
        help_msg <- paste0("Please check that you have selected the correct template in the <b>Select your Dataset</b> tab and 
                            ensure your metadata contains <b>only</b> one template, e.g. ", input$template_type, ".")
        
        # get wrong columns and values for updating preview table
        errorDT <- data.frame(Column=sapply(annotation_status[inx_mt], function(i) i[[2]]),
                              Value=sapply(annotation_status[inx_mt], function(i) i[[4]][[1]]))
        
      } else {
        
        type_error <- paste0("The submitted metadata have ", length(annotation_status), " errors.")
        help_msg <- NULL
        
        errorDT <- data.frame(Column=sapply(annotation_status, function(i) i[[2]]),
                              Value=sapply(annotation_status, function(i) i[[4]][[1]]),
                              Error=sapply(annotation_status, function(i) i[[3]])) 
        # sort rows based on input column names
        errorDT <- errorDT[order(match(errorDT$Column, colnames(rawData()))),]
        
        # output error messages as data table
        show("tbl2")
        output$tbl2 <- DT::renderDT({
          datatable(errorDT, caption = "The errors are also highlighted in the preview table above.", 
                    rownames = FALSE, options = list(pageLength = 50, scrollX = TRUE, 
                                                     scrollY = min(50*length(annotation_status), 400),
                                                     lengthChange = FALSE, info = FALSE, searching = FALSE)
          )
        })
      }                                     
      
      validate_w$update(
        html = h3(sprintf("%d errors found", length(annotation_status)))
      )
      
      ### format output text
      output$text2 <- renderUI({
        tagList( 
          HTML("Your metadata is invalid according to the data model.<br/><br/>"),
          HTML(type_error, "<br/><br/>"),
          HTML(help_msg)
        )
      })
      
      ### update DT view with incorrect values
      ### currently only one column, requires backend support of multiple
      output$tbl <- DT::renderDT({
        datatable(rawData(),
                  options = list(lengthChange = FALSE, scrollX = TRUE)
        ) %>% formatStyle(errorDT$Column,
                          backgroundColor = styleEqual(errorDT$Value, rep("yellow", length(errorDT$Value) ) )) ## how to have multiple errors
      })
      
      show('gsheet_btn')
      
      ### show submit button
      output$submit <- renderUI({
        actionButton("submitButton", "Override and Submit to Synapse")
      })
      
=======
>>>>>>> 4356f4913f936888401b1ca6f7fae48615a6f269
    } else {
      output$text2 <- renderUI({
        HTML("Your metadata is valid!")
      })

      ### show submit button
      output$submit <- renderUI({
        actionButton("submitButton", "Submit to Synapse")
      })
    }
        
      Sys.sleep(2)
      validate_w$hide()
    }
  )
  
  # if user click gsheet_btn, generating gsheet
  observeEvent(
    input$gsheet_btn, {
      
      # loading screen for Google link generation
      gsheet_w <- Waiter$new(
        html = tagList(
          spin_plus(), br(),
          h4("Generating link...")
        ),
        color = "rgba(66, 72, 116, .9)"
      )
      
      gsheet_w$show()
      
      ###lookup schema template name 
      template_type_df <- schema_to_display_lookup[match(input$template_type, schema_to_display_lookup$display_name), 1, drop = F ]
      template_type <- as.character(template_type_df$schema_name)
      
      ## if error not empty aka there is an error
      filled_manifest <- metadata_model$populateModelManifest(paste0(config$community," ", input$template_type), input$file1$datapath, template_type)
      
      show('gsheet_div')
      
      output$gsheet_link <- renderUI({
        # tags$a(href = filled_manifest, filled_manifest, target = "_blank")
        HTML(paste0('<a target="_blank" href="', filled_manifest, '">Edit on the Google Sheet.</a>'))
      })
      
      hide('gsheet_btn') # hide btn once link generated
      
      gsheet_w$hide()
    })
  
  ## loading screen for submitting data
  submit_w <- Waiter$new(
    html = tagList(
      img(src = "loading.gif"),
      h4("Submitting...")
    ),
    color = "#424874"
  )
  ###submit button
  observeEvent(
    input$submitButton, {
<<<<<<< HEAD
      
      submit_w$show()
      
      ### reads in csv 
      infile <- readr::read_csv(input$file1$datapath, na = c("", "NA"))
      
      ### remove empty rows/columns where readr called it "X"[digit] for unnamed col
      infile <- infile[, !grepl('^X', colnames(infile))]
      infile <- infile[rowSums(is.na(infile)) != ncol(infile), ]
      
      ### IF an assay component selected (define assay components)
      ## note for future - the type to filter (eg assay) on could probably also be a config choice
      assay_schemas <- config$manifest_schemas$display_name[config$manifest_schemas$type=="assay"]
      
      ### and adds entityID, saves it as synapse_storage_manifest.csv, then associates with synapse files 
      if ( input$template_type %in% assay_schemas ) {
        
        ### make into a csv or table for assay components
        ### already has entityId
        if ("entityId" %in% colnames(infile)) {
          
          write.csv(infile, file = "./files/synapse_storage_manifest.csv", quote = TRUE, row.names = FALSE, na = "")
          
        } else {
          # if not get ids
          selected_folder <- input$dataset
          selected_project <- input$var
          
          project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
          folder_list <- syn_store$getStorageDatasetsInProject(synStore_obj, project_synID)
          folders_namedList <- c()
          for (i in seq_along(folder_list)) {
            folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
          }
          
          folder_synID <- folders_namedList[[selected_folder]]
          
          file_list <- syn_store$getFilesInStorageDataset(synStore_obj, folder_synID)
          file_namedList <- c()
          for (i in seq_along(file_list)) {
            file_namedList[file_list[[i]][[2]]] <- file_list[[i]][[1]]
          }
          
          files_df <- stack(file_namedList)
          colnames(files_df) <- c("entityId", "Filename")
          files_entity <- inner_join(infile, files_df, by = "Filename")
          
          write.csv(files_entity, file = "./files/synapse_storage_manifest.csv", quote = TRUE, row.names = FALSE, na = "")
        }
=======

    submit_w$show()

    ### reads in csv 
    infile <- readr::read_csv(input$file1$datapath, na = c("", "NA"))
   
    ### remove empty rows/columns where readr called it "X"[digit] for unnamed col
    infile <- infile[, !grepl('^X', colnames(infile))]
    infile <- infile[rowSums(is.na(infile)) != ncol(infile), ]
    
    ### IF an assay component selected (define assay components)
    ## note for future - the type to filter (eg assay) on could probably also be a config choice
    assay_schemas <- config$manifest_schemas$display_name[config$manifest_schemas$type=="assay"]

    ### and adds entityID, saves it as synapse_storage_manifest.csv, then associates with synapse files 
    if ( input$template_type %in% assay_schemas ) {
      
      ### make into a csv or table for assay components
      ### already has entityId
      if ("entityId" %in% colnames(infile)) {

        write.csv(infile, file = "./files/synapse_storage_manifest.csv", quote = TRUE, row.names = FALSE, na = "")

      } else {
        # if not get ids
        selected_folder <- input$dataset
>>>>>>> 4356f4913f936888401b1ca6f7fae48615a6f269
        selected_project <- input$var
        selected_folder <- input$dataset
        
        project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
        
        folder_df <- syn_tableQuery(sprintf("select name, id from %s where type = 'folder' and projectId = '%s'", config$main_fileview, project_synID))$asDataFrame()
        
        folders_namedList <- setNames(as.list(folder_df$id), folder_df$name)
        folderNames <- names(folders_namedList)
        
        folder_synID <- folders_namedList[[selected_folder]]
        
        ### associates metadata with data and returns manifest id
        manifest_id <- syn_store$associateMetadataWithFiles(synStore_obj, "./files/synapse_storage_manifest.csv", folder_synID)
        print(manifest_id)
        manifest_path <- paste0("synapse.org/#!Synapse:", manifest_id)
        ### if no error 
        if (startsWith(manifest_id, "syn") == TRUE) {
          nx_report_success("Success!", paste0("Manifest submitted to: ", manifest_path))
          rm("./files/synapse_storage_manifest.csv")
          
          ### clear inputs 
          output$text2 <- renderUI({
            HTML("")
          })
          output$submit <- renderUI({
          })
          
          ### rerenders fileinput UI
          output$fileInput_ui <- renderUI({
            fileInput("file1", "Upload CSV File",
                      accept = c('text/csv',
                                 'text/comma-separated-values',
                                 '.csv'))
          })
          ### renders empty df
          output$tbl <- DT::renderDT(
            datatable(as.data.frame(matrix(0, ncol = 0, nrow = 0)))
          )
          
        } else {
          submit_w$update(
            html = tagList(
              img(src = "synapse_logo.png", height = "115px"),
              h3("Uh oh, looks like something went wrong!"),
              span(manifest_id, " is not a valid Synapse ID. Try again?")
            )
          )
<<<<<<< HEAD
          rm("/tmp/synapse_storage_manifest.csv")
        }
        
      } else { ## if not assay type tempalte
        write.csv(infile, file = "./files/synapse_storage_manifest.csv", quote = TRUE, row.names = FALSE, na = "")
        
        selected_project <- input$var
        selected_folder <- input$dataset
        
        project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
        # folder_synID <- get_folder_synID(synStore_obj, project_synID, selected_folder)
        
        folder_df <- syn_tableQuery(sprintf("select name, id from %s where type = 'folder' and projectId = '%s'", config$main_fileview, project_synID))$asDataFrame()
        
        folders_namedList <- setNames(as.list(folder_df$id), folder_df$name)
        folderNames <- names(folders_namedList)
        
        folder_synID <- folders_namedList[[selected_folder]]
        
        
        ### associates metadata with data and returns manifest id
        manifest_id <- syn_store$associateMetadataWithFiles(synStore_obj, "./files/synapse_storage_manifest.csv", folder_synID)
        print(manifest_id)
        manifest_path <- paste0("synapse.org/#!Synapse:", manifest_id)
        
        ### if uploaded provided valid synID message
        if (startsWith(manifest_id, "syn") == TRUE) {
          nx_report_success("Success!", paste0("Manifest submitted to: ", manifest_path))
          rm("./files/synapse_storage_manifest.csv")
          
          ### clear inputs 
          output$text2 <- renderUI({
            HTML("")
          })
          output$submit <- renderUI({
          })
          
          ### rerenders fileinput UI
          output$fileInput_ui <- renderUI({
            fileInput("file1", "Upload CSV File",
                      accept = c('text/csv',
                                 'text/comma-separated-values',
                                 '.csv'))
          })
          ### renders empty df
          output$tbl <- DT::renderDT(
            datatable(as.data.frame(matrix(0, ncol = 0, nrow = 0)))
=======
        )
        rm("/tmp/synapse_storage_manifest.csv")
      }

    } else { ## if not assay type tempalte
      write.csv(infile, file = "./files/synapse_storage_manifest.csv", quote = TRUE, row.names = FALSE, na = "")

      selected_project <- input$var
      selected_folder <- input$dataset

      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project
      # folder_synID <- get_folder_synID(synStore_obj, project_synID, selected_folder)

      folder_list <- syn_store$getStorageDatasetsInProject(synStore_obj, project_synID)
      folders_namedList <- c()
      for (i in seq_along(folder_list)) {
        folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
      }
      folder_synID <- folders_namedList[[selected_folder]]

      ### associates metadata with data and returns manifest id
      manifest_id <- syn_store$associateMetadataWithFiles(synStore_obj, "./files/synapse_storage_manifest.csv", folder_synID)
      print(manifest_id)
      manifest_path <- paste0("synapse.org/#!Synapse:", manifest_id)

      ### if uploaded provided valid synID message
      if (startsWith(manifest_id, "syn") == TRUE) {
        nx_report_success("Success!", paste0("Manifest submitted to: ", manifest_path))
        rm("./files/synapse_storage_manifest.csv")

        ### clear inputs 
        output$text2 <- renderUI({
          HTML("")
        })
        output$submit <- renderUI({
        })

        ### rerenders fileinput UI
        output$fileInput_ui <- renderUI({
          fileInput("file1", "Upload CSV File",
                                  accept = c('text/csv',
                                          'text/comma-separated-values',
                                          '.csv'))
        })
        ### renders empty df
        output$tbl <- DT::renderDT(
          datatable(as.data.frame(matrix(0, ncol = 0, nrow = 0)))
>>>>>>> 4356f4913f936888401b1ca6f7fae48615a6f269
          )
          
        } else {
          submit_w$update(
            html = tagList(
              img(src = "synapse_logo.png", height = "115px"),
              h3("Uh oh, looks like something went wrong!"),
              span(manifest_id, " is not a valid Synapse ID. Try again?")
            )
          )
          rm("/tmp/synapse_storage_manifest.csv")
        }
      }
      Sys.sleep(3)
      submit_w$hide()
      
    })
  
  
}


shinyApp(ui, server)