library(shiny)
library(shinyjs)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(stringr)
library(DT)
library(jsonlite)

source(file= "functions.R")

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
                  title = "Select a Bucket: ",
                  selectInput(inputId = "var", label = "Bucket:",
                              choices = c("hca-intake-bucket" ))
        
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
                  DT::DTOutput("rawData")
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
      getModelManifest("scRNASeq") ##cant use additionalMetadata for now bc python dict not compatible
      toggle('text_div')
      output$text <- renderUI({
        tags$a(href = manifest_url, manifest_url)
      })
    }
)
  ### reads and displays csv file
  rawData <- eventReactive(input$csvFile, {
    read.csv(input$csvFile$datapath)
  })
  
  output$rawData <- DT::renderDT(
    rawData(), options = list(lengthChange = FALSE, scrollX = TRUE)
  )

  ### toggles validation status when validate button pressed 
  observeEvent(
    input$validate, {
      validateModelManifest(input$csvFile$datapath, "scRNASeq") ### right now assay is hardcoded
      toggle('text_div2')
      if ( annotation_status != "Validation success!") {
        annotation_status2 <- strsplit(annotation_status, ";")
        err1 <- str_trim( annotation_status2[[1]][1])
        err2 <- str_trim( annotation_status2[[1]][2])
        output$text2 <- renderUI ({
          HTML(paste("Your metadata is invalid according to the data model.","See errors below:" ,err1, err2,sep="<br/>")  )
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