# This is the server logic for a Shiny web application.  You can find out more
# about building applications with Shiny here: http://shiny.rstudio.com This
# server has been modified to be used specifically on Sage Bionetworks Synapse
# pages to log into Synapse as the currently logged in user from the web portal
# using the session token.  https://www.synapse.org

shinyServer(function(input, output, session) {
  options(shiny.reactlog = TRUE)
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params) & dca_schematic_api != "offline") {
   return()
  }
  
  redirect_url <- paste0(
    api$access, "?", "redirect_uri=", app_url, "&grant_type=",
    "authorization_code", "&code=", params$code
  )
  
  if (dca_schematic_api != "offline") {
    # get the access_token and userinfo token
    req <- POST(redirect_url, encode = "form", body = "", authenticate(app$key, app$secret,
      type = "basic"
    ), config = list())
    
    # Stop the code if anything other than 2XX status code is returned
    stop_for_status(req, task = "get an access token")
    token_response <- content(req, type = NULL)
    access_token <- token_response$access_token
    
    session$userData$access_token <- access_token
  } else {
    dcWaiter("show", "Cannot connect to Synapse. Running in offline mode.")
  }
  
  ######## session global variables ########
  # read config in
  if (grepl("dev", dcc_config_file)) {
    def_config <- fromJSON("https://raw.githubusercontent.com/Sage-Bionetworks/data_curator_config/dev/demo/dca-template-config.json")
  } else if (grepl("staging", dcc_config_file)) {
    def_config <- fromJSON("https://raw.githubusercontent.com/Sage-Bionetworks/data_curator_config/staging/demo/dca-template-config.json")
  } else def_config <- fromJSON("https://raw.githubusercontent.com/Sage-Bionetworks/data_curator_config/main/demo/dca-template-config.json")
  
  config <- reactiveVal()
  config_schema <- reactiveVal(def_config)
  model_ops <- setNames(dcc_config$data_model_url,
  dcc_config$synapse_asset_view)
  
  # mapping from display name to schema name
  template_namedList <- reactiveVal()
  
  all_asset_views <- setNames(dcc_config$synapse_asset_view,
  dcc_config$project_name)
  asset_views <- reactiveVal(c("mock dca fileview"="syn33715412"))
  
  dcc_config_react <- reactiveVal(dcc_config)
  
  manifest_data <- reactiveVal()
  validation_res <- reactiveVal()
  manifest_id <- reactiveVal()
  
  data_list <- list(
    projects = reactiveVal(NA), folders = reactiveVal(NULL),
    template = reactiveVal(setNames(def_config$schema_name, def_config$display_name)),
    files = reactiveVal(NULL),
    master_asset_view = reactiveVal(NULL)
  )
  # synapse ID of selected data
  selected <- list(
    project = reactiveVal(NULL), folder = reactiveVal(""),
    schema = reactiveVal(NULL), schema_type = reactiveVal(NULL),
    master_asset_view = reactiveVal(NULL),
    master_asset_view_label = reactiveVal(NULL)
  )
  
  isUpdateFolder <- reactiveVal(FALSE)
  
  data_model_options <- setNames(dcc_config$data_model_url,
    dcc_config$synapse_asset_view)
  data_model = reactiveVal(NULL)
  
  # data available to the user
  syn_store <- NULL # gets list of projects they have access to
  
  asset_views <- reactiveVal(c("mock dca fileview (syn33715412)"="syn33715412"))
  
  # All of tabName from the tabs in ui.R
  tabs_list <- c("tab_asset_view",
    "tab_project",
    "tab_template_select",
    "tab_folder",
    "tab_template",
    "tab_upload")
  clean_tags <- c(
    "div_template", "div_template_warn",
    "div_validate", NS("tbl_validate", "table"), "btn_val_gsheet", "btn_submit"
  )
  
  # add box effects
  boxEffect(zoom = FALSE, float = TRUE)
  
  ######## Initiate Login Process ########
  # synapse cookies
  session$sendCustomMessage(type = "readCookie", message = list())
  
  shinyjs::useShinyjs()
  shinyjs::hide(selector = ".sidebar-menu")
  shinyjs::hide("box_preview")
  shinyjs::hide("box_validate")
  shinyjs::hide("box_submit")
  
  # initial loading page
  observeEvent(input$cookie, {
  
  # login and update session
  #
  # The original code pulled the auth token from a cookie, but it
  # should actually come from session$userData.  The former is
  # the Synapse login, only works when the Shiny app' is hosted
  # in the synapse.org domain, and is unscoped.  The latter will
  # work in any domain and is scoped to the access required by the
  # Shiny app'
  #
  
  if (dca_schematic_api != "offline") {
    access_token <- session$userData$access_token
    has_access <- vapply(all_asset_views, function(x) {
      synapse_access(id=x, access="DOWNLOAD", auth=access_token)
    }, 1L)
    asset_views(all_asset_views[has_access==1])
    
    if (length(asset_views) == 0) stop("You do not have DOWNLOAD access to any supported Asset Views.")
    updateSelectInput(session, "dropdown_asset_view",
      choices = asset_views())
    
    user_name <- synapse_user_profile(auth=access_token)$firstName
    
    is_certified <- synapse_is_certified(auth=access_token)
    if (!is_certified) {
     dcWaiter("update", landing = TRUE, isCertified = FALSE)
    } else {
      # update waiter loading screen once login successful
      dcWaiter("update", landing = TRUE, userName = user_name)
    }
  } else {
    updateSelectInput(session, "dropdown_asset_view",
      choices = c("Offline mock data (synXXXXXX)"="synXXXXXX"))
    dcWaiter("hide")
  }
    
  if (length(asset_views()) == 1L) {
    click("btn_asset_view")
  } 
  
  ######## Arrow Button ########
  lapply(1:6, function(i) {
    switchTabServer(id = paste0("switchTab", i), tabId = "tabs", tab = reactive(input$tabs)(), tabList = tabs_list, parent = session)
  })
  
  })
  
  # Goal of this observer is to retrieve a list of projects the users can access
  # within the selected asset view.
  observeEvent(input$btn_asset_view, {
    dcWaiter("show", msg = paste0("Getting data. This may take a minute."),
      color=col2rgba(col2rgb("#CD0BBC01")))
    shinyjs::disable("btn_asset_view")
    
    selected$master_asset_view(input$dropdown_asset_view)
    av_names <- names(asset_views()[asset_views() %in% selected$master_asset_view()])
    selected$master_asset_view_label(av_names)
    
    dcc_config_react(dcc_config[dcc_config$synapse_asset_view == selected$master_asset_view(), ])
    if (dca_schematic_api == "offline") dcc_config_react(dcc_config[dcc_config$project_name == "DCA Demo", ])
    
    data_model(data_model_options[selected$master_asset_view()])
    
    output$sass <- renderUI({
    tags$head(tags$style(css()))
    })
    css <- reactive({
    # Don't change theme for default projects
    sass(input = list(primary_col=dcc_config_react()$primary_col,
      htan_col=dcc_config_react()$secondary_col,
      sidebar_col=dcc_config_react()$sidebar_col,
      sass_file("www/scss/main.scss")))
    })
    
    dcWaiter("hide")
    dcWaiter("show", msg = paste0("Getting data. This may take a minute."),
      color = col2rgba(dcc_config_react()$primary_col, 255*0.9))
    
    logo_img <- ifelse(!is.na(dcc_config_react()$logo_location),
      paste0("https://raw.githubusercontent.com/Sage-Bionetworks/data_curator_config/main/",
        dcc_config_react()$logo_location),
      "https://raw.githubusercontent.com/Sage-Bionetworks/data_curator_config/main/demo/Logo_Sage_Logomark.png")
    
    logo_link <- ifelse(!is.na(dcc_config_react()$logo_link),
      dcc_config_react()$logo_link,
      "https://synapse.org"
    )
    
    output$logo <- renderUI({
      tags$li(
        class = "dropdown", id = "logo",
        tags$a(
          href = logo_link,
          target = "_blank",
          tags$img(
            height = "40px", alt = "LOGO",
            src = logo_img
          )
        )
      )
      })
    
    if (dca_schematic_api == "reticulate") {
      # Update schematic_config and login
      schematic_config <- yaml::read_yaml("schematic_config.yml")
      schematic_config$synapse$master_fileview <- selected$master_asset_view()
      schematic_config$model$input$download_url <- model_ops[names(model_ops) == selected$master_asset_view()]
      yaml::write_yaml(schematic_config, "schematic_config.yml")
      download.file(schematic_config$model$input$download_url, schematic_config$model$input$location)
      setup_synapse_driver()
      syn$login(authToken = access_token, rememberMe = FALSE)
      syn_store <<- synapse_driver(access_token = access_token)
      
      system(
      "python3 .github/config_schema.py -c schematic_config.yml --service_repo 'Sage-Bionetworks/schematic' --overwrite"
      )
    
    }
    # Use the template dropdown config file from the appropriate branch of
    # data_curator_config
    conf_file <- reactiveVal(template_config_files[input$dropdown_asset_view])
    if (!file.exists(conf_file())){
      if (grepl("dev", dcc_config_file)) {
        conf_file(
          file.path("https://raw.githubusercontent.com/Sage-Bionetworks/data_curator_config/dev",
                    conf_file()
          )
        )
      } else if (grepl("staging", dcc_config_file)) {
        conf_file(
          file.path("https://raw.githubusercontent.com/Sage-Bionetworks/data_curator_config/staging",
                    conf_file()
          )
        )
      } else {
        conf_file(
          file.path("https://raw.githubusercontent.com/Sage-Bionetworks/data_curator_config/main",
                    conf_file()
          )
        )
      }
    }
    config_df <- jsonlite::fromJSON(conf_file())
    
    conf_template <- setNames(config_df[[1]]$schema_name, config_df[[1]]$display_name)
    config(config_df)
    config_schema(config_df)
    data_list$template(conf_template)
  
    if (dca_synapse_api == TRUE & dca_schematic_api != "offline") {
      #This chunk gets projects using the synapse REST API
      #Check for user access to project scopes within asset view
      
      .asset_view <- selected$master_asset_view()
      promises::future_promise({
        try({
          scopes <- synapse_get_project_scope(id = .asset_view, auth = access_token)
          scope_access <- vapply(scopes, function(x) {
            synapse_access(id=x, access="DOWNLOAD", auth=access_token)
          }, 1L)
          scopes <- scopes[scope_access==1]
          projects <- bind_rows(
            lapply(scopes, function(x) synapse_get(id=x, auth=access_token))
          ) %>% arrange(name)
          setNames(projects$id, projects$name)
        }, silent = FALSE)
      }) %...>% data_list$projects()
    
    } else {
      data_list_raw <- switch(dca_schematic_api,
      reticulate  = storage_projects_py(synapse_driver, access_token),
      rest = storage_projects(url=file.path(api_uri, "v1/storage/projects"),
      asset_view = selected$master_asset_view(),
      access_token = access_token),
      list(list("Offline Project A", "Offline Project"))
      )
      data_list$projects(list2Vector(data_list_raw))
    }
  })
  
  observeEvent(data_list$projects(), ignoreInit = TRUE, {
    if (is.null(data_list$projects()) || length(data_list$projects()) == 0 ||
        inherits(data_list$projects(), "try-error")) {
      dcWaiter("update", landing = TRUE, isPermission = FALSE)
    } else {
    
      # updates project dropdown
      lapply(c("dropdown_"), function(x) {
      lapply(c(1, 3), function(i) {
      updateSelectInput(session, paste0(x, dropdown_types[i]),
      choices = sort(names(data_list[[i]]()))
      )
      })
      })
    
    updateTabsetPanel(session, "tabs", selected = "tab_project")
    
    shinyjs::show(selector = ".sidebar-menu")
    shinyjs::hide(select = "li:nth-child(3)")
    shinyjs::hide(select = "li:nth-child(4)")
    shinyjs::hide(select = "li:nth-child(5)")
    shinyjs::hide(select = "li:nth-child(6)")
    
    dcWaiter("hide")
    }
  })
  
  observeEvent(input$dropdown_asset_view, {
    shinyjs::enable("btn_asset_view")
    shinyjs::enable("btn_template_select")
    output$data_model_url_download <- renderUI({
      data_model_url <- tags$a(href = data_model(), "See Data Model Used to Generate this Template", target = "_blank")
    })
    output$data_model_url_validate <- renderUI({
      data_model_url <- tags$a(href = data_model(), "See Data Model Used to Validate this Metadata", target = "_blank")
    })
  })
  
  # Goal of this observer is to get all of the folders within the selected
  # project.
  observeEvent(input$btn_project, {
    ######## Update Folder List ########
    dcWaiter("show", msg = paste0("Getting data"),
    color = col2rgba(dcc_config_react()$primary_col, 255*0.9))
    shinyjs::disable("btn_project")
    selected$project(data_list$projects()[names(data_list$projects()) == input$dropdown_project])
    
    observeEvent(input[["dropdown_project"]], {
      # get synID of selected project
      project_id <- selected$project()
      
      .asset_view <- selected$master_asset_view()
      
      promises::future_promise({
        try({
        folder_list_raw <- switch(
          dca_schematic_api,
          reticulate = storage_projects_datasets_py(
            synapse_driver,
            project_id),
          rest = storage_project_datasets(
            url=file.path(api_uri, "v1/storage/project/datasets"),
            asset_view = .asset_view,
            project_id=project_id,
            access_token=access_token),
          list(list("DatatypeA", "DatatypeA"), list("DatatypeB","DatatypeB"))
        )
        
        folder_list <- list2Vector(folder_list_raw)
        folder_list[sort(names(folder_list))]
        }, silent = TRUE)
      
      }) %...>% data_list$folders()
    })
  })
  
  observeEvent(data_list$folders(), ignoreInit = TRUE, {
    updateTabsetPanel(session, "tabs",
                      selected = "tab_folder")
    shinyjs::show(select = "li:nth-child(3)")
    updateSelectInput(session, "header_dropdown_project",
                      choices = selected$project())
    updateSelectInput(session, "dropdown_folder", choices = data_list$folders())
    
    if (inherits(data_list$folders(), "try-error")) {
      nx_report_error(title = "Error retrieving folders",
                      message = tagList(
                        p("Confirm that this project contains folders."),
                        p("Refresh the app to try again or contact the DCC for help."),
                        p("For debugging: ", data_list$folders())
                      )
      )
      hide(selector = "#NXReportButton") # hide OK button so users can't continue
    }
    if (length(data_list$folders()) < 1) {
      nx_report_error(title = "Error retrieving folders",
                      message = tagList(
                        p("Confirm you have appropriate access permissions."),
                        p("Refresh the app to try again or contact the DCC for help."),
                        p("For debugging: ", data_list$folders())
                      )
      )
      hide(selector = "#NXReportButton") # hide OK button so users can't continue
    }
    dcWaiter("hide")
  })
  
  observeEvent(input$dropdown_project, {
    shinyjs::enable("btn_project")
    shinyjs::enable("btn_template_select")
  })
  
  # Goal of this button is to updpate the template reactive object
  # with the template the user chooses
  observeEvent(input$btn_template_select, {
    dcWaiter("show", msg = "Please wait", color = col2rgba(dcc_config_react()$primary_col, 255*0.9), sleep=0)
    shinyjs::disable("btn_template_select")
    selected$schema(data_list$template()[input$dropdown_template])
    shinyjs::show(select = "li:nth-child(5)")
    shinyjs::show(select = "li:nth-child(6)")
    updateTabsetPanel(session, "tabs",
                      selected = "tab_template")
    dcWaiter("hide")
  })
  
  observeEvent(input$dropdown_template, {
    shinyjs::enable("btn_template")
    shinyjs::enable("btn_template_select")
    updateSelectInput(session, "header_dropdown_template",
                      choices = input$dropdown_template)
  })
  
  # Goal of this button is to get the files within a folder the user selects
  observeEvent(input$btn_folder, {
  
    dcWaiter("show", msg = paste0("Getting data"), color = col2rgba(dcc_config_react()$primary_col, 255*0.9))
    shinyjs::disable("btn_folder")
    shinyjs::show(select = "li:nth-child(4)")

    
    updateTabsetPanel(session, "tabs",
    selected = "tab_template_select")
    
    # clean tags in generating-template tab
    sapply(clean_tags[1:2], FUN = hide)
    
    
    if (selected$schema_type() %in% c("record", "file")) {
      # check number of files if it's file-based template
      # This gets files using the synapse REST API
      # get file list in selected folder
      if (dca_synapse_api == TRUE & dca_schematic_api != "offline") {
        .folder <- selected$folder()
        promises::future_promise({
          files <- synapse_entity_children(auth = access_token, parentId=.folder, includeTypes = list("file"))
          if (nrow(files) > 0) { files_vec <- setNames(files$id, files$name)
          } else files_vec <- NA_character_
          files_vec
        }) %...>% data_list$files()
      
      } else {
      
        file_list <- switch(dca_schematic_api,
          reticulate = storage_dataset_files_py(selected$folder()),
          rest = storage_dataset_files(url=file.path(api_uri, "v1/storage/dataset/files"),
            asset_view = selected$master_asset_view(),
            dataset_id = selected$folder(),
            access_token=access_token),
          list(list("DatatypeA", "DatatypeA"), list("DatatypeB", "DatatypeB")))
        
        # update files list in the folder
        data_list$files(list2Vector(file_list))
      }
    }
  })
  
  observeEvent(input$dropdown_folder,{
    shinyjs::enable("btn_folder")
    shinyjs::enable("btn_template_select")
    selected_folder <- data_list$folders()[which(data_list$folders() == input$dropdown_folder)]
    selected$folder(selected_folder)
    updateSelectInput(session, "header_dropdown_folder",
                      choices = selected$folder())
  })
  
  observeEvent(data_list$files(), ignoreInit = TRUE, {
    warn_text <- NULL
    if (length(data_list$folders()) == 0) {
      # add warning if there is no folder in the selected project
      warn_text <- paste0(
        "please create a folder in the ",
        strong(sQuote(input$dropdown_project)),
        " prior to submitting templates."
      )
    }
    if (is.null(data_list$files())) {
      # display warning message if folder is empty and data type is file-based
      warn_text <- paste0(
        strong(sQuote(input$dropdown_folder)), " folder is empty,
        please upload your data before generating manifest.",
        "<br>", strong(sQuote(input$dropdown_template)),
        " requires data files to be uploaded prior to generating and submitting templates.",
        "<br>", "Filling in a template before uploading your data,
        may result in errors and delays in your data submission later."
      )
    }
    
    # if there is warning from above checks
    if (!is.null(warn_text)){
      # display warnings
      output$text_template_warn <- renderUI(tagList(br(), span(class = "warn_msg", HTML(warn_text))))
      show("div_template_warn")
    }
    
    dcWaiter("hide")
  
  })
  
  observeEvent(input$btn_folder_have_template, {
    shinyjs::show(select = "li:nth-child(5)")
    shinyjs::show(select = "li:nth-child(6)")
    updateTabsetPanel(session, "tabs",
                      selected = "tab_upload")
  })
  
  observeEvent(input$update_confirm, {
    req(input$update_confirm == TRUE)
    isUpdateFolder(TRUE)
    lapply(dropdown_types, function(x) {
      updateSelectInput(session, paste0("dropdown_", x),
        selected = input[[paste0("header_dropdown_", x)]]
      )
    })
  })
  
  ######## Update Template ########
  # update selected schema template name
  observeEvent(input$dropdown_template, {
    shinyjs::enable("btn_template_select")
    # update reactive selected values for schema
    selected$schema(data_list$template()[input$dropdown_template])
    schema_type <- config_schema()[[1]]$type[which(config_schema()[[1]]$display_name == input$dropdown_template)]
    selected$schema_type(schema_type)
    # clean all tags related with selected template
    sapply(clean_tags, FUN = hide)
  }, ignoreInit = TRUE)
  
  ######## Dashboard ########
  #  dashboard(
  #    id = "dashboard",
  #    syn.store = syn_store,
  #    project.scope = selected$project,
  #    schema = selected$schema,
  #    schema.display.name = reactive(input$dropdown_datatype),
  #    disable.ids = c("box_pick_project", "box_pick_manifest"),
  #    ncores = ncores,
  #    access_token = access_token,
  #    fileview = selected$master_asset_view(),
  #    folder = selected$project(),
  #    schematic_api = dca_schematic_api,
  #    schema_url = data_model()
  #  )
  
  manifest_url <- reactiveVal(NULL)
  
  ######## Template Google Sheet Link ########
  # validate before generating template
  observeEvent(c(selected$folder(), selected$schema(), input$tabs), {
  
  })
  
  observeEvent(input$tabs, {
    req(input$tabs %in% "tab_template")
    output$template_title <- renderText({
      sprintf("Go to %s template for %s folder",
              selected$schema(),
              names(selected$folder())
              )
    })
  })
  
  observeEvent(input$tabs, {
    req(input$tabs %in% c("tab_project", "tab_template_select", "tab_folder", "tab_template", "tab_upload"))
    shinyjs::addClass(id = "header_selection_dropdown", class = "dropdown open")
  })
  
  observeEvent(input$tabs, {
    req(input$tabs == "tab_template_select")
    shinyjs::show("header_selection_dropdown")
  })
  
  observeEvent(c(input$`switchTab4-Next`, input$tabs), {
  
    req(input$tabs == "tab_template")
    dcWaiter("show", msg = "Getting template. This may take a minute.", color = dcc_config_react()$primary_col)
    
    ### This doesn't work - try moving manifest_generate outside of downloadButton
    .schema <- selected$schema()
    .datasetId <- selected$folder()
    .schema_url <- data_model()
    .asset_view <- selected$master_asset_view()
    .template <- paste(
      dcc_config_react()$project_name,
      "-",
      input$dropdown_template
    )
    .url <- ifelse(dca_schematic_api != "offline",
    file.path(api_uri, "v1/manifest/generate"),
    NA)
    .output_format <- dcc_config_react()$manifest_output_format
    .use_annotations <- dcc_config_react()$manifest_use_annotations
    
    promises::future_promise({
      try({
      switch(dca_schematic_api, 
        rest = manifest_generate(
          url=.url,
          schema_url = .schema_url,
          title = .template,
          data_type = .schema,
          dataset_id = .datasetId,
          asset_view = .asset_view,
          use_annotations = .use_annotations,
          output_format = .output_format,
          access_token=access_token,
          strict_validation = FALSE
        ),
        {
          message("Downloading offline manifest")
          Sys.sleep(0)
          tibble(a="b", c="d")
        }
      )
      }, silent = TRUE)
    }) %...>% manifest_data()
  
  })
  
  observeEvent(manifest_data(), {
    if (inherits(manifest_data(), "try-error")) {
      nx_report_error("Failed to get manifest",
                      tagList(
                        p("There was a problem downloading the manifest."),
                        p("Try again or contact the DCC for help"),
                        p("For debugging: ", manifest_data())
                      ))
      shinyjs::enable("btn_template_select")
      updateTabsetPanel(session, "tab_template_select")
    } else {
      if (dcc_config_react()$manifest_output_format == "google_sheet") {
        shinyjs::show("div_template")
      } else shinyjs::show("div_download_data")
    }
    dcWaiter("hide")
  })
  
  # Bookmarking this thread in case we can't use writeBin...
  # Use a db connection instead
  # https://community.rstudio.com/t/how-to-let-download-button-work-with-eventreactive/20937
  
  # The giant anonymous content function lets users click through the app and
  # only download the manifest if they need to. Originally, this was in the
  # observeEvent above.
  output$downloadData <- downloadHandler(
    filename = function() sprintf("%s.xlsx", input$dropdown_template),
    #filename = function() sprintf("%s.csv", input$dropdown_template),
    content = function(file) {
    dcWaiter("show", msg = "Downloading data", color = dcc_config_react()$primary_col)
    dcWaiter("hide", sleep = 0)
    writeBin(manifest_data(), file)
    }
  )
  
    # generate link
  output$text_template <- renderUI(
    tags$a(id = "template_link", href = manifest_data(), list(icon("hand-point-right"), manifest_data()), target = "_blank")
  )
  
  if (dca_schematic_api == "offline") {
    mock_offline_manifest <- tibble("column1"="mock offline data")
    output$downloadData <- downloadHandler(
      filename = function() sprintf("%s.csv", input$dropdown_template),
      content = function(file) {
        write_csv(mock_offline_manifest, file)
      }
    )
  }
  
  observeEvent(input$btn_template_confirm, {
   req(input$btn_template_confirm == TRUE)
   runjs("$('#template_link')[0].click();")
  })
  
  ######## Reads .csv File ########
  # Check out module and don't use filepath. Keep file in memory
  inFile <- csvInfileServer("inputFile", colsAsCharacters = TRUE, keepBlank = TRUE, trimEmptyRows = TRUE)
  
  observeEvent(inFile$data(), {
    # After trimming blank rows and columns from data, write to the filepath
    # so it can be passed to the submit endpoint.
    readr::write_csv(inFile$data(), inFile$raw()$datapath)
    # hide the validation section when upload a new file
    sapply(clean_tags[-c(1:2)], FUN = hide)
    # renders in DT for preview
    DTableServer("tbl_preview", inFile$data(), filter = "top")
    shinyjs::show("box_preview")
    shinyjs::show("box_validate")
  })
  
  ######## Validation Section #######
  observeEvent(input$btn_validate, {
  
    dcWaiter("show", msg = "Validating manifest. This may take a minute.", color = col2rgba(dcc_config_react()$primary_col, 255*0.9))
    
    # Reset validation_result in case user reuploads the same file. This makes
    # the validation_res observer trigger any time this button is pressed.
    validation_res(NULL) 
    
    # loading screen for validating metadata
    .datapath <- inFile$raw()$datapath
    .schema <- selected$schema()
    .project <- list(selected$project())
    .data_model <- data_model()
    .infile_data <- inFile$data()
    .dd_template <- input$dropdown_template
    .restrict_rules <- dcc_config_react()$validate_restrict_rules
    
    promises::future_promise({
      annotation_status <- switch(dca_schematic_api,
        reticulate = manifest_validate_py(
          .datapath,
          .schema,
          TRUE,
        .project),
      rest = manifest_validate(
        url=file.path(api_uri, "v1/model/validate"),
        schema_url=.data_model,
        data_type=.schema,
        file_name=.datapath,
        restrict_rules = .restrict_rules),
      {
        Sys.sleep(0)
        list(list(
        "errors" = list(
        Row = NA, Column = NA, Value = NA,
        Error = "Mock error for offline mode."
        )
        ))
      }
      )
      
      # validation messages
      validationResult(annotation_status, .dd_template, .infile_data)
    
    }) %...>% validation_res()
  
  })
  
  observeEvent(validation_res(), {
    # if there is a file uploaded
    if (!is.null(validation_res()$result)) {
    
      ValidationMsgServer("text_validate", validation_res())
      
      # highlight invalue cells in preview table
      if (validation_res()$error_type == "Wrong Schema") {
        DTableServer("tbl_preview", data = inFile$data(), highlight = "full")
      } else {
        DTableServer(
        "tbl_preview",
        data = inFile$data(),
        highlight = "partial", highlightValues = validation_res()$preview_highlight
        )
      }
      
      if (validation_res()$result == "valid" | dca_schematic_api == "offline" && grepl("fixed", inFile$data()[1,1])) {
        # show submit button
        output$submit <- renderUI(actionButton("btn_submit", "Submit data", class = "btn-primary-color"))
        dcWaiter("update", msg = paste0(validation_res()$error_type, " Found !!! "), spin = spin_inner_circles(), sleep = 2.5)
        shinyjs::show("box_submit")
      } else {
          if (dca_schematic_api != "offline" & dcc_config_react()$manifest_output_format == "google_sheet") {
          #output$val_gsheet <- renderUI(
          #actionButton("btn_val_gsheet", "  Generate Google Sheet Link", icon = icon("table"), class = "btn-primary-color")
          #)
          } else if (dca_schematic_api == "offline") {
          output$dl_manifest <- renderUI({
          downloadButton("downloadData_good", "Download Corrected Data")
          })
          }
        dcWaiter("update", msg = paste0(validation_res()$error_type, " Found !!! "), spin = spin_pulsar(), sleep = 2.5)
        }
    } else {
     dcWaiter("hide")
    }
    
    show("div_validate")
  
  })
  
  # if user click gsheet_btn, generating gsheet
  observeEvent(input$btn_val_gsheet, {
    # loading screen for Google link generation
    dcWaiter("show", msg = "Generating link...", color = col2rgba(dcc_config_react()$primary_col, 255*0.9))
    filled_manifest <- switch(dca_schematic_api,
      reticulate = manifest_populate_py(paste0(config$community, " ", input$dropdown_template),
        inFile$raw()$datapath,
        selected$schema()),
      rest = manifest_populate(url=file.path(api_uri, "v1/manifest/populate"),
        schema_url = data_model(),
        title = paste0(config$community, " ", input$dropdown_template),
        data_type = selected$schema(),
        return_excel = FALSE,
        csv_file = inFile$raw()$datapath),
      "offline-no-gsheet-url")
    
    
    # rerender and change button to link
    if (dca_schematic_api != "offline") {
      output$val_gsheet <- renderUI({
        HTML(paste0("<a target=\"_blank\" href=\"", filled_manifest, "\">Edit on the Google Sheet.</a>"))
      })
    }
    dcWaiter("hide")
  })
  
  # Offline version of downloading a failed manifest
  mock_offline_manifest_2 <- tibble("column1"="fixed offline data")
    output$downloadData_good <- downloadHandler(
      filename = function() sprintf("%s.csv", input$dropdown_template),
      content = function(file) {
        write_csv(mock_offline_manifest_2, file)
     }
  )
  
  
  ######## Submission Section ########
  observeEvent(input$btn_submit, {
    # loading screen for submitting data
    dcWaiter("show", msg = "Submitting data. This may take a minute.", color = col2rgba(dcc_config_react()$primary_col, 255*0.9))
    
    
    if (is.null(selected$folder())) {
      # add waiter if no folder selected
      dcWaiter("update", msg = paste0("Please select a folder to submit"), spin = spin_pulsar(), sleep = 0)
    }
    
    # abort submission if no folder selected
    req(selected$folder())
    
    manifest_filename <- sprintf("%s_%s.csv", manifest_basename, tolower(selected$schema()))
    tmp_out_dir <- tempdir()
    tmp_file_path <- file.path(tmp_out_dir, manifest_filename)
    dir.create(tmp_out_dir, showWarnings = FALSE)
    
    # reads file csv again
    submit_data <- csvInfileServer("inputFile", colsAsCharacters = TRUE, keepBlank = TRUE, trimEmptyRows = TRUE)$data()
    
    # If a file-based component selected (define file-based components) note for future
    # the type to filter (eg file-based) on could probably also be a config choice
    display_names <- config_schema()$manifest_schemas$display_name[config_schema()$manifest_schemas$type == "file"]

    if (input$dropdown_template %in% display_names) {
      # make into a csv or table for file-based components already has entityId
      if ("entityId" %in% colnames(submit_data)) {
        # Convert this to JSON instead and submit
        write.csv(submit_data,
        file = tmp_file_path,
        quote = TRUE, row.names = FALSE, na = ""
        )
      } else {
        # Get file list from synapse REST API
        if (dca_synapse_api == TRUE & dca_schematic_api != "offline") {
          files <- synapse_entity_children(auth = access_token, parentId=selected$folder(), includeTypes = list("file"))
          data_list$files(setNames(files$id, files$name))
        } else {
          file_list_raw <- switch(dca_schematic_api,
            reticulate = storage_dataset_files_py(selected$folder()),
            rest = storage_dataset_files(url=file.path(api_uri, "v1/storage/dataset/files"),
              asset_view = selected$master_asset_view(),
              dataset_id = selected$folder(),
              access_token=access_token))
          
          data_list$files(list2Vector(file_list_raw))
        }
        
        # better filename checking is needed
        # TODO: crash if no file existing
        files_df <- stack(data_list$files())
        # adds entityID, saves it as synapse_storage_manifest.csv, then associates with synapse files
        colnames(files_df) <- c("entityId", "Filename")
        files_entity <- inner_join(submit_data, files_df, by = "Filename")
        # convert this to JSON instead and submit
        write.csv(files_entity,
         file = tmp_file_path,
          quote = TRUE, row.names = FALSE, na = ""
        )
      }
      
      .folder <- selected$folder()
      .data_model <- data_model()
      .schema <- selected$schema()
      .asset_view <- selected$master_asset_view()
      .submit_use_schema_labels <- dcc_config_react()$submit_use_schema_labels
      .table_manipulation <- dcc_config_react()$submit_table_manipulation
      .submit_manifest_record_type <- dcc_config_react()$submit_manifest_record_type
      .restrict_rules <- dcc_config_react()$validate_restrict_rules
      .hide_blanks <- dcc_config_react()$submit_hide_blanks
      
      # associates metadata with data and returns manifest id
      promises::future_promise({
        try({
          switch(dca_schematic_api,
                 reticulate = model_submit_py(schema_generator,
                                              tmp_file_path,
                                              .folder,
                                              "table",
                                              FALSE),
                 rest = model_submit(url=file.path(api_uri, "v1/model/submit"),
                                     schema_url = .data_model,
                                     data_type = NULL, # NULL to bypass validation
                                     dataset_id = .folder,
                                     access_token = access_token,
                                     restrict_rules = .restrict_rules,
                                     file_name = tmp_file_path,
                                     asset_view = .asset_view,
                                     use_schema_label=.submit_use_schema_labels,
                                     manifest_record_type=.submit_manifest_record_type,
                                     table_manipulation=.table_manipulation,
                                     hide_blanks=.hide_blanks),
                 "synXXXX - No data uploaded"
          )
        }, silent = TRUE)
      }) %...>% manifest_id()
      
    } else {
    # if not file-based type template
    # convert this to JSON and submit
    write.csv(submit_data,
      file = tmp_file_path, quote = TRUE,
     row.names = FALSE, na = ""
    )
    
    # associates metadata with data and returns manifest id
    .folder <- selected$folder()
    .data_model <- data_model()
    .schema <- selected$schema()
    .asset_view <- selected$master_asset_view()
    .submit_use_schema_labels <- dcc_config_react()$submit_use_schema_labels
    .table_manipulation <- dcc_config_react()$submit_table_manipulation
    .submit_manifest_record_type <- dcc_config_react()$submit_manifest_record_type
    .restrict_rules <- dcc_config_react()$validate_restrict_rules
    .hide_blanks <- dcc_config_react()$submit_hide_blanks
    # associates metadata with data and returns manifest id
    promises::future_promise({
      try({
        switch(dca_schematic_api,
               reticulate = model_submit_py(schema_generator,
                                            tmp_file_path,
                                            .folder,
                                            "table",
                                            FALSE),
               rest = model_submit(url=file.path(api_uri, "v1/model/submit"),
                                   schema_url = .data_model,
                                   data_type = NULL, # NULL to bypass validation
                                   dataset_id = .folder,
                                   access_token = access_token,
                                   restrict_rules = .restrict_rules,
                                   file_name = tmp_file_path,
                                   asset_view = .asset_view,
                                   use_schema_label=.submit_use_schema_labels,
                                   manifest_record_type=.submit_manifest_record_type,
                                   table_manipulation=.table_manipulation,
                                   hide_blanks=.hide_blanks),
               "synXXXX - No data uploaded"
        )
      }, silent = TRUE)
    }) %...>% manifest_id()
    
    }
    
  })
  
  observeEvent(manifest_id(), {
    
    req(!is.null(manifest_id()))
    
    if (inherits(manifes_id(), "try-error")) {
      dcWaiter("hide")
      nx_report_error(title = "Error submitting manifest",
                      message = tagList(
                        p("Refresh the app to try again or contact the DCC for help."),
                        p("For debugging: ", manifest_id())
                      )
      )
    } else {
      manifest_path <- tags$a(href = paste0("https://www.synapse.org/#!Synapse:", manifest_id()), manifest_id(), target = "_blank")
      
      # add log message
      message(paste0("Manifest :", sQuote(manifest_id()), " has been successfully uploaded"))
      
      # if no error
      if (startsWith(manifest_id(), "syn") == TRUE) {
        dcWaiter("hide")
        nx_report_success("Success!", HTML(paste0("Manifest submitted to: ", manifest_path)))
        
        # clean up old inputs/results
        sapply(clean_tags, FUN = hide)
        reset("inputFile-file")
        DTableServer("tbl_preview", data.frame(NULL))
      } else {
        dcWaiter("update", msg = HTML(paste0(
          "Uh oh, looks like something went wrong!",
          manifest_id,
          " is not a valid Synapse ID. Try again?"
        )), sleep = 0)
      }
    }

    manifest_id(NULL)
  })
})
