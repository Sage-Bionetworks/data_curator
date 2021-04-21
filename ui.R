# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This interface has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org

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
library(sass)  # read scss file

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    titleWidth = 250,
    title = "Data Curator",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://humantumoratlas.org/",
        target = "_blank",
        ## insert links and logos of your choice, this is just an example
        tags$img(
          height = "40px", alt = "HTAN LOGO",
          src = "HTAN_text_logo.png"
        )
      )
    )
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Instructions",
        tabName = "instructions",
        icon = icon("book-open")
      ),
      menuItem(
        "Select your Dataset",
        tabName = "data",
        icon = icon("mouse-pointer")
      ),
      menuItem(
        "Get Metadata Template",
        tabName = "template",
        icon = icon("table")
      ),
      menuItem(
        "Submit & Validate Metadata",
        tabName = "upload",
        icon = icon("upload")
      ),
      HTML(
        "<footer>
            Supported by the Human Tumor Atlas Network <br/>
            (U24-CA233243-01)<br/>
            Powered by Sage Bionetworks
        </footer>"
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(sass(sass_file("www/scss/main.scss"))),
      singleton(
        includeScript("www/readCookie.js")
      )
    ),
    uiOutput("title"),
    use_notiflix_report(),
    tabItems(
      # First tab content
      tabItem(
        tabName = "instructions",
        h2("Instructions for the Data Curator App:"),
        h3(
          "1. Go to",
          strong("Select your Dataset"),
          "tab - select your project; choose your folder and metadata template type matching your metadata."
        ),
        h3(
          "2. Go to",
          strong("Get Metadata Template"),
          "tab - click on the link to generate the metadata template, then fill out and download the file as a CSV. If you already have an annotated metadata template, you may skip this step."
        ),
        h3(
          "3. Go to",
          strong("Submit and Validate Metadata"),
          "tab - upload your filled CSV and validate your metadata. If you receive errors correct them, reupload your CSV, and revalidate until you receive no more errors. When your metadata is valid, you will be able to see a 'Submit' button. Press it to submit your metadata."
        )
      ),
      # second tab content
      tabItem(
        tabName = "data",
        h2("Set Dataset and Metadata Template for Curation"),
        fluidRow(
          box(
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            title = "Choose a Project and Folder: ",
            selectizeInput(
              inputId = "var",
              label = "Project:",
              choices = "Generating..."
            ),
            uiOutput("folders"),
            helpText(
              "If your recently updated folder does not appear, please wait for Synapse to sync and refresh"
            )
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
      tabItem(
        tabName = "template",
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
                id = "text_div",
                height = "100%",
                htmlOutput("text")
              )
            ),
            helpText("This link will leads to an empty template or your previously submitted template with new files if applicable.")
          )
        )
      ),
      # Fourth tab content
      tabItem(
        tabName = "upload",
        h2("Submit & Validate a Filled Metadata Template"),
        fluidRow(
          box(
            title = "Upload Filled Metadata as a CSV",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            uiOutput("fileInput_ui")
          ),
          box(
            title = "Metadata Preview",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            DT::DTOutput("tbl")
          ),
          box(
            title = "Validate Filled Metadata",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            actionButton("validate", "Validate Metadata"),
            hidden(
              div(
                id = "text_div2",
                height = "100%",
                htmlOutput("text2")
              ),
              DT::DTOutput("tbl2"),
              actionButton("gsheet_btn", "  Click to Generate Google Sheet Link", icon = icon("table")),
              div(
                id = "gsheet_div",
                height = "100%",
                htmlOutput("gsheet_link")
              )
            ),
            helpText(
              HTML("If you have an error, please try editing locally or on google sheet.<br/>
                   Reupload your CSV and press the validate button as needed.")
            )
          ),
          box(
            title = "Submit Validated Metadata to Synapse",
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

uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    authorization_url <- oauth2.0_authorize_url(api, app, scope = scope)
    return(tags$script(HTML(sprintf(
      "location.replace(\"%s\");",
      authorization_url
    ))))
  } else {
    ui
  }
}
