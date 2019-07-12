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
    title = "HTAN Upload" ,
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 50px}"),
            tags$style(".main-header .logo {height: 70px; font-size: 28px; padding-top: 10px}"),
            tags$style(".sidebar-toggle {height: 15px; padding-top: 25px !important;}"),
            tags$style(".navbar {min-height:50px !important}"),
            tags$style(".messages-menu {padding-top :5px}" ),
            tags$a(href="https://humantumoratlas.org/", target = "_blank", 
                   tags$img(height = "40px", alt = "HTAN LOGO", 
                            src = "HTAN_text_logo.png"))),
    dropdownMenu(type = "messages", icon = icon("user", "fa-2x")) ### dummy user icon
    ),
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 80px}"),
    sidebarMenu(
    menuItem("Download Metadata Template", tabName = "template", icon = icon("table")),
    menuItem("Upload Data", tabName = "upload", icon = icon("upload"))
    )
  ),
  dashboardBody(
    tags$head(
      singleton(
        includeScript("www/readCookie.js")
      )),
    tabItems(
      # First tab content
      tabItem(tabName = "template",
              h2("Choose your Assay and Dataset Template"),
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width= 6,
                  title = "Select a Project: ",
                  selectInput(inputId = "var", label = "Project:",
                              choices = c("HCA Project (test)" ))
        
                ),
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width= 6,
                  title = "Select a Dataset: ",
                  selectInput(inputId = "var", label = "Dataset:",
                              choices = c("HCA Census of Immune Cells", "HCA Ischaemic Sensitivity of Human Tissue" ))
                ),
                
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  title = "Select an Assay: ",
                  selectInput(inputId = "dataset", label = "Assay:",
                                     choices = list("scRNAseq", "Whole Exome Seq", "FISH", "CODEX" ))
                ),
              
                box(
                  title = "Download Template as CSV",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("download", "Generate Link"),
                  hidden(
                    div(id='text_div',
                        height = "100%",
                        htmlOutput("text"),
                        style = "font-size:18px; background-color: white; border: 1px solid #ccc; border-radius: 3px; margin: 10px 0; padding: 10px"
                    )
                  )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "upload",
              # useShinyjs(),
              h2("Upload Annotated Metadata File"),
              fluidRow(
                
                box(
                  title = "Upload Annotated Metadata as a csv",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12,
                  fileInput("csvFile", "Upload CSV File",
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv'))
                  ) ,
                box(
                  title = "Uploaded CSV",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12, 
                  DT::DTOutput("rawData"),
                  helpText("Google spreadsheet row numbers are incremented from this table by 1")
                ),
                box(
                  title = "Validate Annotated Metadata",
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
                box( 
                  title = "Submit Validated Metadata to Synapse",
                  status = "primary",
                  solidHeader = TRUE, 
                  width = 12, 
                  actionButton("submit", "Submit to Synapse")
                   )
      )
    )
  )
)
)

server <- function(input, output, session) {
  ### synapse cookies
  session$sendCustomMessage(type = "readCookie", message = list())
  
  ###toggles link when download button pressed
  observeEvent(
    input$download, {
      manifest_url <- getModelManifest("scRNASeq", filenames = c("MantonCB1_HiSeq_1_S1_L002_R1_001.fastq.gz", "MantonCB1_HiSeq_1_S1_L001_R2_001.fastq.gz", "MantonCB1_HiSeq_1_S1_L001_R1_001.fastq.gz", "MantonCB1_HiSeq_1_S1_L002_R2_001.fastq.gz"))
      toggle('text_div')
      
      ### if want a progress bar need more feedback from API to know how to increment progress bar
      # withProgress(message = "connecting to Google Sheets API") 
      
      output$text <- renderUI({
        tags$a(href = manifest_url, manifest_url) ### add link to data dictionary
      })
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
      annotation_status <- validateModelManifest(input$csvFile$datapath, "scRNASeq") ### right now assay is hardcoded
      filled_manifest <- populateModelManifest(input$csvFile$datapath, "scRNASeq") ### wrong schema for values?
      toggle('text_div2')
      if ( length(annotation_status) != 0 ) { ## if error not empty aka there is an error
        
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
          
          if (unlist(in_val) == "") {
            in_val <- NA
            str_names[i] <- paste("spreadsheet row <b>",
                                  row, "</b>column <b>", column,
                                  "</b>your value <b>", in_val,
                                  "</b> is not an allowed value from:", allowed_vals, sep=" ")
            in_vals[i] <- in_val
          } else {
          
          str_names[i] <- paste("spreadsheet row <b>",
                                row, "</b>column <b>", column,
                                "</b>your value <b>", in_val,
                                "</b> is not an allowed value from:", allowed_vals, sep=" ")
          in_vals[i] <- in_val
          }
        }

        output$text2 <- renderUI ({
          HTML("Your metadata is invalid according to the data model.<br/> ",
               "See errors below: <br/>",
               paste0(sprintf("%s", str_names), collapse = "<br/>"),
               "Edit your data here: ",
               paste0('<a href="', filled_manifest, '">here</a>')
               )

        })
        
        output$rawData <- DT::renderDT({ 
          datatable(rawData(),
                    options = list(lengthChange = FALSE, scrollX = TRUE)
          ) %>% formatStyle(unlist(column), 
                            backgroundColor = styleEqual(unlist(in_vals), rep("yellow", length(in_vals))) ) ## how to have multiple errors 
        })
        
      } else {   
      output$text2 <- renderUI ({
            HTML("Your metadata is valid!"  )
      })
      }
    }
  )

  # )
}

shinyApp(ui, server)