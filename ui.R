# This R Shiny app functions as a frontend GUI to the Schematic python module
# github.com/sage-Bionetworks/schematic
# 
# The UI is a dashboard that acts like a wizard. The user steps through each tab
# and selects from dropdown menus.
# After input selection, a spreadsheet is downloaded, modified outside of the 
# app, and then uploaded to the app.
# The final steps are to validate the spreadsheet with Schematic and either
# report validation errors to the user or submit the data to synapse.org

ui <- shinydashboardPlus::dashboardPage(
  title = "Data Curator",
  skin = "purple",
  dashboardHeader(
    titleWidth = 250,
    title = tagList(
      span(class = "logo-lg", "Data Curator"),
      span(class = "logo-mini", "DCA")
    ),
    uiOutput("logo"),
    leftUi = hidden(
      tagList(
      dropdownBlock(
        id = "header_selection_dropdown",
        title = "Selected data",
        icon = icon("sliders"),
        badgeStatus = NULL,
        fluidRow(
          # Set color and background for items to remove the grey background
          # when using the option maxItems = 1. Note, this affects all dropdowns
          # but it's difficult to notice.
          tags$style(HTML(
            ".item {
               background: white !important;
               color: black !important;
             }"
          )),
          div(
            id = "header_content_project",
            # Use selectizeInput instead of selectInput to specify options
            # maxItems = "1" will remove the dropdown triangle from the box.
            selectizeInput(
              inputId = "header_dropdown_project",
              label = NULL,
              choices = "No project selected",
              options = list(maxItems = "1")
            )
          ),
          div(
            id = "header_content_folder",
            selectizeInput(
              inputId = "header_dropdown_folder",
              label = NULL,
              choices = "No folder selected",
              options = list(maxItems = "1")
            )
          ),
          div(
            id = "header_content_template",
            selectizeInput(
              inputId = "header_dropdown_template",
              label = NULL,
              choices = "No template selected",
              options = list(maxItems = "1")
            )
          )
        )
      )
    )
    ) # end hidden
  ),
  dashboardSidebar(
    width = 250,
    hidden(
    sidebarMenu(
      id = "tabs",
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
        "Select Folder",
        tabName = "tab_folder",
        icon = icon("folder")
      ),
      menuItem(
        "Select Template",
        tabName = "tab_template_select",
        icon = icon("table")
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
        tags$footer(
          actionButton("info_box", "About Data Curator", icon("circle-info"), class="btn-primary-color"),
          HTML('&#12288 Powered by <i class="far fa-heart"></i> and Sage Bionetworks')
        )
      )
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
  use_notiflix_report(width = "500px", messageMaxLength = 10000,
                      titleMaxLength = 100),
  use_waiter(),
  tabItems(
  # second tab content
  tabItem(
    tabName = "tab_asset_view",
    fluidRow(
      box(
        id = "box_pick_asset_view",
        status = "primary",
        width = 6,
        title = "Select a DCC: ",
        selectInput(
          inputId = "dropdown_asset_view",
          label = NULL,
          choices = setNames(tenants_config$synapse_asset_view,
                             tenants_config$name)
        ),
        actionButton("btn_asset_view", "Next",
        class = "btn-primary-color"
        )
      )
    )
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
        actionButton("btn_project", "Next",
        class = "btn-primary-color"
        )
      ),
    ),
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
        actionButton("btn_folder", "Next",
                     class = "btn-primary-color"
        )
      )
    )
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
        actionButton("btn_template_select", "Download template",
                     class = "btn-primary-color"
        ),
        actionButton("btn_folder_have_template",
                     "Skip to validation",
                     class = "btn-primary-color"
        )
      )
    ),
  ),
  tabItem(
    tabName = "tab_template",
    useShinyjs(),
      fluidRow(
        box(
          title = textOutput('template_title'),
          status = "primary",
          width = 12,
          hidden(
            div(
              id = "div_download_data",
              height = "100%",
              downloadButton("downloadData", "Download")
            ),
            div(
              id = "div_template",
              height = "100%",
              htmlOutput("text_template")
            ),
            helpText("Note: After downloading, spreadsheet apps may add blank",
                     "rows that must be removed before validating.")
            ),
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
        ),
      ),
    switchTabUI("switchTab5", direction = "right")
  ),
  # Fourth tab content
  tabItem(
    tabName = "tab_upload",
    fluidRow(
      box(
        title = "Upload Filled Metadata as a CSV",
        status = "primary",
        width = 12,
        csvInfileUI("inputFile"),
        helpText("Note: Remove blank rows from your file before uploading.")
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
    redir <- tags$script(HTML(
      sprintf("location.replace(\"%s\");", authorization_url)
    ))
    return(redir)
  } else {
    ui
  }
}
