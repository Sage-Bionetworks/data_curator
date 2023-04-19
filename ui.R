# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This interface has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org

ui <- shinydashboardPlus::dashboardPage(
  title = "Data Curator",
  skin = "purple",
  dashboardHeader(
    titleWidth = 250,
    title = tagList(
      span(class = "logo-lg", "Data Curator"),
      span(class = "logo-mini", "DCA")
    ),
    uiOutput("logo")
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      # uiOutput("title"),
      # menuItem(
      #   "Instructions",
      #   tabName = "tab_instructions",
      #   icon = icon("book-open")
      # ),
      menuItem(
        "Select DCC",
        tabName = "tab_asset_view",
        icon = icon("server")
      ),
      menuItem(
        "Select Project",
        tabName = "tab_project",
        icon = icon("database")
      ),
      menuItem(
        "Select Template",
        tabName = "tab_template_select",
        icon = icon("table")
      ),
      menuItem(
        "Select Folder",
        tabName = "tab_folder",
        icon = icon("folder")
      ),
      menuItem(
        "Download Template",
        tabName = "tab_template",
        icon = icon("download")
      ),
      menuItem(
        "Validate & Submit Metadata",
        tabName = "tab_upload",
        icon = icon("upload")
      ),
      # add sidebar footer here
      tags$a(
        id = "sidebar_footer", `data-toggle` = "tab",
        tags$footer(HTML(' Powered by <i class="far fa-heart"></i> and Sage Bionetworks'))
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(sass(sass_file("www/scss/main.scss"))),
      singleton(includeScript("www/js/readCookie.js")),
      tags$script(htmlwidgets::JS("setTimeout(function(){history.pushState({}, 'Data Curator', window.location.pathname);},2000);"))
    ),
    uiOutput("sass"),
    # load dependencies
    use_notiflix_report(width = "400px"),
    use_waiter(),
    #dcamodules::use_dca(),
    tabItems(
      # First tab content
      # tabItem(
      #   tabName = "tab_instructions",
      #   h2("Instructions for the Data Curator App (DCA):"),
      #   h3(
      #     "1. Go to",
      #     strong("Select your Dataset"),
      #     "tab - select your project; choose your folder and metadata template type matching your metadata."
      #   ),
      #   h3(
      #     "2. Go to",
      #     strong("Get Metadata Template"),
      #     "tab - click on the link to generate the metadata template, then fill out and download the file as a CSV. If you already have an annotated metadata template, you may skip this step."
      #   ),
      #   h3(
      #     "3. Go to",
      #     strong("Submit and Validate Metadata"),
      #     "tab - upload your filled CSV and validate your metadata. If you receive errors correct them, reupload your CSV, and revalidate until you receive no more errors. When your metadata is valid, you will be able to see a 'Submit' button. Press it to submit your metadata."
      #   ),
      #   switchTabUI("switchTab1", direction = "right")
      # ),
      # second tab content
      tabItem(
        tabName = "tab_asset_view",
        #h2("Select the asset view"),
        fluidRow(
          box(
            id = "box_pick_asset_view",
            status = "primary",
            width = 6,
            title = "Select a DCC: ",
            selectInput(
              inputId = "dropdown_asset_view",
              label = NULL,
              choices = setNames(dcc_config$synapse_asset_view,
                                 dcc_config$project_name)
            ),
            actionButton("btn_asset_view", "Go",
                         class = "btn-primary-color"
            )
        )
        )#,
        #switchTabUI("switchTab1", direction = "right") # remove arrow from assetview page.
      ),
      tabItem(
        tabName = "tab_project",
        fluidRow(
          box(
            id = "box_pick_project",
            status = "primary",
            width = 6,
            title = "Select a Project: ",
            selectInput(
              inputId = "dropdown_project",
              label = NULL,
              choices = "Generating..."
            ),
            actionButton("btn_project", "Go",
                         class = "btn-primary-color"
            )
            ),
          if (dca_schematic_api != "offline" && Sys.getenv("DCA_COMPLIANCE_DASHBOARD")==TRUE) dashboardUI("dashboard")
          ),
        #switchTabUI("switchTab2", direction = "both")
        ),
      tabItem(
        tabName = "tab_template_select",
        fluidRow(
          box(
            id = "box_pick_template",
            status = "primary",
            width = 6,
            title = "Select a Template: ",
            selectInput(
              inputId = "dropdown_template",
              label = NULL,
              choices = "Generating..."
            ),
            actionButton("btn_template_select", "Go",
                         class = "btn-primary-color"
            )
          )
        ),
        #switchTabUI("switchTab3", direction = "both")
      ),
      tabItem(
        tabName = "tab_folder",
        fluidRow(
          box(
            id = "box_pick_folder",
            status = "primary",
            width = 6,
            title = "Select a Folder: ",
            selectInput(
              inputId = "dropdown_folder",
              label = NULL,
              choices = "Generating..."
            ),
            actionButton("btn_folder", "Go",
                         class = "btn-primary-color"
            )
          )
        ),
        switchTabUI("switchTab4", direction = "right")
      ),
      tabItem(
        tabName = "tab_template",
        useShinyjs(),
        if (Sys.getenv("DCA_MANIFEST_OUTPUT_FORMAT") != "excel") {
        fluidRow(
          box(
            title = "Get Link, Annotate, and Download Template as CSV",
            status = "primary",
            width = 12,
            actionButton("btn_template", "Click to Generate Google Sheets Template",
              class = "btn-primary-color"
            ),
            hidden(
              div(
                id = "div_template_warn",
                height = "100%",
                htmlOutput("text_template_warn")
              ),
              div(
                id = "div_template",
                height = "100%",
                htmlOutput("text_template")
              )
            ),
            helpText("This link will lead to an empty template or your previously submitted template with new files if applicable.")
          )
        )}else{
        fluidRow(
          box(
            title = "Get template for <selected-template> and <selected-folder>",
            status = "primary",
            width = 12,
            downloadButton("downloadData", "Download"),
            hidden(
              div(
                id = "div_template_warn_xls",
                height = "100%",
                htmlOutput("text_template_warn_xls")
              ),
              div(
                id = "div_template_xls",
                height = "100%",
                htmlOutput("text_template_xls")
              )
            )
          )
        )},
        switchTabUI("switchTab5", direction = "right")
      ),
      # Fourth tab content
      tabItem(
        tabName = "tab_upload",
        #h2("Submit & Validate a Filled Metadata Template"),
        fluidRow(
          box(
            title = "Upload Filled Metadata as a CSV",
            status = "primary",
            width = 12,
            csvInfileUI("inputFile")
          ),
          box(
            title = "Metadata Preview",
            collapsible = TRUE,
            status = "primary",
            width = 12,
            DTableUI("tbl_preview"),
            id = "box_preview"
          ),
          box(
            title = "Validate Filled Metadata",
            status = "primary",
            collapsible = TRUE,
            width = 12,
            actionButton("btn_validate", "Validate Metadata", class = "btn-primary-color"),
            div(
              id = "div_validate",
              height = "100%",
              ValidationMsgUI("text_validate")
            ),
            DTableUI("tbl_validate"),
            uiOutput("val_gsheet"),
            uiOutput("dl_manifest"),
            helpText(
              HTML("If you have an error, please try editing locally or on google sheet.
                  Reupload your CSV and press the validate button as needed.")
            ),
            id = "box_validate"
          ),
          box(
            title = "Submit Validated Metadata to Synapse",
            status = "primary",
            width = 12,
            uiOutput("submit"),
            id = "box_submit"
          )
        ),
        #switchTabUI("switchTab6", direction = "left")
      )
    ),
    # waiter loading screen
    dcWaiter("show", landing = TRUE)
  )
)

uiFunc <- function(req) {
  if (dca_schematic_api == "offline") {
    message("dca_schematic_api set to offline. Running in offline mode.")
    return(ui)
  }
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
