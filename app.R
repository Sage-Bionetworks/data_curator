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

# schematic <- reticulate::import("schematic")

# source("functions.R")

#########

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    titleWidth = 250,
    title = "Data Curator",
    tags$li(class = "dropdown",
            tags$a(href = "https://humantumoratlas.org/", target = "_blank",
                   tags$img(height = "40px", alt = "HTAN LOGO",
                            src = "HTAN_text_logo.png")))
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
            Supported by the Human Tumor Atlas Network <br/>
            (U24-CA233243-01)<br/>
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
              h3("1. Go to", strong("Select your Dataset"), "tab - select your project; choose your folder and metadata template type matching your metadata."),
              h3("2. Go to", strong("Get Metadata Template"), "tab - click on the link to generate the metadata template, then fill out and download the file as a CSV. If you already have an annotated metadata template, you may skip this step."),
              h3("3. Go to", strong("Submit and Validate Metadata"), "tab - upload your filled CSV and validate your metadata. If you receive errors correct them, reupload your CSV, and revalidate until you receive no more errors. When your metadata is valid, you will be able to see a 'Submit' button. Press it to submit your metadata.")
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
# useShinyjs(),
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
                    )
                  ),
                  helpText("Errors are evaluated one column at a time, if you have an error please reupload your CSV and press the validate button as needed.")
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
      synStore_obj <<- syn_store(config$main_fileview, token = input$cookie)

      # get_projects_list(synStore_obj)
      projects_list <<- syn_store$getStorageProjects(synStore_obj)

      for (i in seq_along(projects_list)) {
        projects_namedList[projects_list[[i]][[2]]] <<- projects_list[[i]][[1]]
      }

      ### updates project dropdown
      updateSelectizeInput(session, 'var', choices = sort(names(projects_namedList)))

      ### update waiter loading screen once login successful
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
        folder_list <- syn_store$getStorageDatasetsInProject(synStore_obj, project_synID)
        folders_namedList <- c()
        for (i in seq_along(folder_list)) {
          folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
        }
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


      ## links shows in text box
      toggle('text_div')
      ### if want a progress bar need more feedback from API to know how to increment progress bar ###

      output$text <- renderUI({
        tags$a(href = manifest_url, manifest_url, target = "_blank") ### add link to data dictionary when we have it ###
      })
    } else {
      ### if the manifest already exists
      manifest_entity <- syn_get(existing_manifestID)
      # prepopulatedManifestURL = mm.populateModelManifest("test_update", entity.path, component)

      manifest_url <- metadata_model$populateModelManifest(paste0(config$community," ", input$template_type), manifest_entity$path, template_type)

      toggle('text_div3')

      output$text <- renderUI({
        tags$a(href = manifest_url, manifest_url, target = "_blank")
      })
    }
    manifest_w$hide()
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
    readr::read_csv(input$file1$datapath, na = c("", "NA"))
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
    
    toggle('text_div2')


    if (length(annotation_status) != 0) {

      ## if error not empty aka there is an error
      filled_manifest <- metadata_model$populateModelManifest(paste0(config$community," ", input$template_type), input$file1$datapath, template_type)

      ### create list of string names for the error messages if there is more than one at a time 
      str_names <- c()

      ### initialize list of errors and column names to highlight 
      error_values <- c()
      column_names <- c()

      ### loop through the multiple error messages
      for (i in seq_along(annotation_status)) {
        

        row <- annotation_status[i][[1]][1]
        column <- annotation_status[i][[1]][2]
        message <- annotation_status[i][[1]][3]

        error_value <- annotation_status[i][[1]][4]

        ## if empty value change to NA ### not reporting the value in the cell anymore!!!
        if (unlist(error_value) == "") {
          error_value <- NA
          
        } else {

          error_value <- error_value
        }

        
        error_values[i] <- error_value
        column_names[i] <- column
        str_names[i] <- paste( paste0(i, "."),
                              "At row <b>", row, 
                              "</b>value <b>", error_value,
                              "</b>in ", "<b>", column, "</b>",
                              message, paste0("</b>", "<br/>"), sep = " ")
      }
 
      validate_w$update(
        html = h3(sprintf("%d errors found", length(annotation_status)))
      )


      ### format output text
      output$text2 <- renderUI({
          tagList( 
          HTML("Your metadata is invalid according to the data model.<br/> ",
              "You have", length(annotation_status), " errors: <br/>"),
          HTML(str_names),
          HTML("<br/>Please edit your data locally and resubmit." # or ",
            #  paste0('<a target="_blank" href="', filled_manifest, '">on Google Sheets </a>')
              )

          )
      })
      ### update DT view with incorrect values
      ### currently only one column, requires backend support of multiple
      output$tbl <- DT::renderDT({
        datatable(rawData(),
                    options = list(lengthChange = FALSE, scrollX = TRUE)
          ) %>% formatStyle(unlist(column_names),
                            backgroundColor = styleEqual( unlist(error_values), rep("yellow", length(error_values) ) )) ## how to have multiple errors
      })
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

    submit_w$show()

    ### reads in csv 
    infile <- readr::read_csv(input$file1$datapath, na = c("", "NA"))
   
    ### remove empty rows/columns where readr called it "X[digit]" for unnamed col
    infile <- infile[, -grep("^X\\d", colnames(infile))]
    infile <- infile[rowSums(is.na(infile)) != ncol(infile), ]
    
    ### IF an assay component selected (define assay components)
    ## note for future - the type to filter (eg assay) on could probably also be a config choice
    assay_schemas <- config$manifest_schemas$display_name[config$manifest_schemas$type=="assay"]

    ### IF an assay component selected, adds entityID, saves it as synapse_storage_manifest.csv, then associates with synapse files 
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
      selected_project <- input$var
      selected_folder <- input$dataset

      project_synID <- projects_namedList[[selected_project]] ### get synID of selected project

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
        rm("/tmp/synapse_storage_manifest.csv")
      }

    } else {
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
