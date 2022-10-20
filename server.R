# This is the server logic for a Shiny web application.  You can find out more
# about building applications with Shiny here: http://shiny.rstudio.com This
# server has been modified to be used specifically on Sage Bionetworks Synapse
# pages to log into Synapse as the currently logged in user from the web portal
# using the session token.  https://www.synapse.org

shinyServer(function(input, output, session) {
  options(shiny.reactlog = TRUE)
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  redirect_url <- paste0(
    api$access, "?", "redirect_uri=", app_url, "&grant_type=",
    "authorization_code", "&code=", params$code
  )
  # get the access_token and userinfo token
  req <- POST(redirect_url, encode = "form", body = "", authenticate(app$key, app$secret,
    type = "basic"
  ), config = list())
  # Stop the code if anything other than 2XX status code is returned
  stop_for_status(req, task = "get an access token")
  token_response <- content(req, type = NULL)
  access_token <- token_response$access_token

  session$userData$access_token <- access_token
  
  ######## session global variables ########
  source("R/schematic_rest_api.R")
  # import module that contains SynapseStorage class
  # read config in
  config <- jsonlite::fromJSON("www/config.json")
  config_schema <- as.data.frame(config[[1]])
  # mapping from display name to schema name
  template_namedList <- config_schema$schema_name
  names(template_namedList) <- config_schema$display_name
  #master_fileview <- schematic_config$synapse$master_fileview
  #master_fileview <- Sys.getenv("DCA_SYNAPSE_MASTER_FILEVIEW")

  # data available to the user
  syn_store <- NULL # gets list of projects they have access to

  data_list <- list(
    project = reactiveVal(NULL), folders = reactiveVal(NULL),
    schemas = reactiveVal(template_namedList), files = reactiveVal(NULL),
    master_fileview = reactiveVal(NULL)
  )
  # synapse ID of selected data
  selected <- list(
    project = reactiveVal(NULL), folder = reactiveVal(""),
    schema = reactiveVal(NULL), schema_type = reactiveVal(NULL),
    master_fileview = reactiveVal(NULL)
  )

  selected <- list(
    project = reactiveVal(NULL), folder = reactiveVal(""),
    schema = reactiveVal(NULL), schema_type = reactiveVal(NULL),
    master_fileview = reactiveVal(NULL)
  )
  
  isUpdateFolder <- reactiveVal(FALSE)

  tabs_list <- c("tab_asset_view", "tab_data", "tab_template", "tab_upload")
  clean_tags <- c(
    "div_template", "div_template_warn",
    "div_validate", NS("tbl_validate", "table"), "btn_val_gsheet", "btn_submit"
  )

  # add box effects
  boxEffect(zoom = FALSE, float = TRUE)

  ######## Initiate Login Process ########
  # synapse cookies
  session$sendCustomMessage(type = "readCookie", message = list())

  # initial loading page
  #
  # TODO:  If we don't use cookies, then what event should trigger this?
  #
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
    access_token <- session$userData$access_token
    
    user_name <- datacurator::synapse_user_profile(auth=access_token)[["userName"]]
    
    if (!synapse_is_certified(auth = access_token)) {
      dcWaiter("update", landing = TRUE, isCertified = FALSE)
    } else {
      # update waiter loading screen once login successful
      dcWaiter("update", landing = TRUE, userName = user_name)
    }
    
    ######## Arrow Button ########
    lapply(1:4, function(i) {
      switchTabServer(id = paste0("switchTab", i), tabId = "tabs", tab = reactive(input$tabs)(), tabList = tabs_list, parent = session)
    })
    
  })
  
  #observeEvent(input[["dropdown_asset_view"]], ignoreNULL = TRUE, ignoreInit = TRUE, {
  observeEvent(input$btn_asset_view, {
    selected$master_fileview(input$dropdown_asset_view)
               # updating syn storage
               projects_list <- storage_projects(url=file.path(api_uri, "v1/storage/projects"),
                                                 asset_view = selected$master_fileview(),
                                                 input_token = access_token)
               data_list$project(list2Vector(projects_list))
               
               # updates project dropdown
               lapply(c("header_dropdown_", "dropdown_"), function(x) {
                 lapply(c(1, 3), function(i) {
                   updateSelectInput(session, paste0(x, dropdown_types[i]),
                                     choices = sort(names(data_list[[i]]()))
                   )
                 })
               })
})

  ######## Header Dropdown Button ########
  # Adjust header selection dropdown based on tabs
  observe({
    if (input[["tabs"]] %in% c("tab_data", "tab_asset_view")) {
      hide("header_selection_dropdown")
    } else {
      show("header_selection_dropdown")
      addClass(id = "header_selection_dropdown", class = "open")
    }
  })
  
  # sync header dropdown with main dropdown
  lapply(dropdown_types, function(x) {
    observeEvent(input[[paste0("dropdown_", x)]], {
      updateSelectInput(session, paste0("header_dropdown_", x),
        selected = input[[paste0("dropdown_", x)]]
      )
    })
  })

  observeEvent(input$btn_header_update, {
    nx_confirm(
      inputId = "update_confirm",
      title = "Are you sure to update?",
      message = "previous selections will also change",
      button_ok = "Sure!",
      button_cancel = "Nope!"
    )
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

  ######## Update Folder List ########
  lapply(c("header_dropdown_", "dropdown_"), function(x) {
    observeEvent(ignoreInit = TRUE, input[[paste0(x, "project")]], {
      # get synID of selected project
      project_id <- data_list$project()[input[[paste0(x, "project")]]]

      # gets folders per project
      folder_list <- storage_project_datasets(url=file.path(api_uri, "v1/storage/project/datasets"),
                                              asset_view = selected$master_fileview(),
                                              project_id=project_id,
                                              input_token=access_token) %>% list2Vector()

      # update folder names
      updateSelectInput(session, paste0(x, "folder"), choices = sort(names(folder_list)))

      if (x == "dropdown_") {
        selected$project(project_id)
        data_list$folders(folder_list)
      }

      if (isUpdateFolder()) {
        # sync with header dropdown
        updateSelectInput(session, "dropdown_folder", selected = input[["header_dropdown_folder"]])
        isUpdateFolder(FALSE)
      }
    })
  })

  ######## Update Template ########
  # update selected schema template name
  observeEvent(input$dropdown_template, {
    # update reactive selected values for schema
    selected$schema(data_list$schemas()[input$dropdown_template])
    schema_type <- config_schema$type[which(config_schema$display_name == input$dropdown_template)]
    selected$schema_type(schema_type)
  })

  ######## Dashboard ########
  dashboard(
    id = "dashboard",
    syn.store = syn_store,
    project.scope = selected$project,
    schema = selected$schema,
    schema.display.name = reactive(input$dropdown_template),
    disable.ids = c("box_pick_project", "box_pick_manifest"),
    ncores = ncores,
    access_token = access_token,
    fileview = selected$master_fileview(),
    folder = selected$project()
  )

  ######## Template Google Sheet Link ########
  observeEvent(c(input$dropdown_folder, input$tabs), {
    if (input$tabs == "tab_template") {
      tmp_folder_id <- data_list$folders()[[input$dropdown_folder]]
      # update selected folder ID if folder changes
      req(tmp_folder_id != selected$folder())
      selected$folder(tmp_folder_id)

      dcWaiter("show", msg = paste0("Getting files in ", input$dropdown_folder, "..."))
      # get file list in selected folder
      file_list <- storage_dataset_files(url=file.path(api_uri, "v1/storage/dataset/files"),
                                         asset_view = selected$master_fileview(),
                            dataset_id = selected$project(),
                            input_token=access_token)
      if (inherits(file_list, "xml_document")) {
        err_msg <- xml2::xml_text(xml2::xml_child(file_list, "head/title"))
        stop(sprintf("Storage/dataset/files request was unsuccessful. %s", err_msg))
      }
      data_list$files <<- list2Vector(file_list)
      dcWaiter("hide")
    }
  })

  # display warning message if folder is empty and data type is file-based
  observeEvent(c(selected$folder(), selected$schema()), {

    # hide tags when users select new template
    sapply(clean_tags, FUN = hide)

    # only show below warning in template tab
    if (input$tabs == "tab_template") {
      req(length(data_list$files) == 0 & selected$schema_type() == "file")
      warn_text <- paste0(
        strong(sQuote(input$dropdown_folder)), " folder is empty,
        please upload your data before generating manifest.",
        "<br><br>", strong(sQuote(input$dropdown_template)),
        " requires data files to be uploaded prior generating and submitting templates.",
        "<br><br>", "Filling in a template before uploading your data,
        may result in errors and delays in your data submission later."
      )

      nx_report_warning("Warning", HTML(warn_text))
      output$text_template_warn <- renderUI(tagList(br(), span(class = "warn_msg", HTML(warn_text))))

      show("div_template_warn")
    }
  })

  observeEvent(input$btn_template, {

    # loading screen for template link generation
    dcWaiter("show", msg = "Generating link...")
    #schematic rest api to generate manifest
    manifest_url <- manifest_generate(url=file.path(api_uri, "v1/manifest/generate"),
                                      title = input$dropdown_template,
                      data_type = selected$schema(), dataset_id = selected$folder())
    # generate link
    output$text_template <- renderUI(
      tags$a(id = "template_link", href = manifest_url, list(icon("hand-point-right"), manifest_url), target = "_blank")
    )

    dcWaiter("hide", sleep = 1)

    nx_confirm(
      inputId = "btn_template_confirm",
      title = "Go to the template now?",
      message = paste0("click 'Go' to edit your ", sQuote(input$dropdown_template), " template on the google sheet"),
      button_ok = "Go",
    )

    # display link
    show("div_template") # TODO: add progress bar on (loading) screen
  })

  observeEvent(input$btn_template_confirm, {
    req(input$btn_template_confirm == TRUE)
    runjs("$('#template_link')[0].click();")
  })

  ######## Reads .csv File ########
  # Check out module and don't use filepath. Keep file in memory
  inFile <- csvInfileServer("inputFile", colsAsCharacters = TRUE, keepBlank = TRUE)

  observeEvent(inFile$data(), {
    # hide the validation section when upload a new file
    sapply(clean_tags[-c(1:2)], FUN = hide)
    # renders in DT for preview
    DTableServer("tbl_preview", inFile$data(), filter = "top")
  })

  ######## Validation Section #######
  observeEvent(input$btn_validate, {

    # loading screen for validating metadata
    dcWaiter("show", msg = "Validating...")

    # schematic rest api to validate metadata
    annotation_status <- manifest_validate(url=file.path(api_uri, "v1/model/validate"),
                                           schema_url=Sys.getenv("DCA_MODEL_INPUT_DOWNLOAD_URL"),
                                           data_type=selected$schema(),
                                           json_str=toJSON(inFile$data()))
                           #csv_file=inFile$raw()$datapath)
                           #file_name = toJSON(inFile$data))

    # validation messages
    validation_res <- validationResult(annotation_status, input$dropdown_template, inFile$data())
    ValidationMsgServer("text_validate", validation_res)

    # if there is a file uploaded
    if (!is.null(validation_res$result)) {

      # highlight invalue cells in preview table
      if (validation_res$error_type == "Wrong Schema") {
        DTableServer("tbl_preview", data = inFile$data(), highlight = "full")
      } else {
        DTableServer(
          "tbl_preview",
          data = inFile$data(),
          highlight = "partial", highlightValues = validation_res$preview_highlight
        )
      }

      if (validation_res$result == "valid") {
        # show submit button
        output$submit <- renderUI(actionButton("btn_submit", "Submit to Synapse", class = "btn-primary-color"))
        dcWaiter("update", msg = paste0(validation_res$error_type, " Found !!! "), spin = spin_inner_circles(), sleep = 2.5)
      } else {
        output$val_gsheet <- renderUI(
          actionButton("btn_val_gsheet", "  Generate Google Sheet Link", icon = icon("table"), class = "btn-primary-color")
        )
        dcWaiter("update", msg = paste0(validation_res$error_type, " Found !!! "), spin = spin_pulsar(), sleep = 2.5)
      }
    } else {
      dcWaiter("hide")
    }

    show("div_validate")
  })

  # if user click gsheet_btn, generating gsheet
  observeEvent(input$btn_val_gsheet, {
    # loading screen for Google link generation
    dcWaiter("show", msg = "Generating link...")
    
    filled_manifest <- manifest_generate(url=file.path(api_uri, "v1/manifest/generate"),
                                         data_type=paste0(config$community,
      " ", input$dropdown_template),
      title=selected$schema,
      csv_file=inFile$raw()$datapath)

    # rerender and change button to link
    output$val_gsheet <- renderUI({
      HTML(paste0("<a target=\"_blank\" href=\"", filled_manifest, "\">Edit on the Google Sheet.</a>"))
    })

    dcWaiter("hide")
  })


  ######## Submission Section ########
  observeEvent(input$btn_submit, {
    # loading screen for submitting data
    dcWaiter("show", msg = "Submitting...")

    dir.create("./manifests", showWarnings = FALSE)

    # reads file csv again
    submit_data <- csvInfileServer("inputFile")$data()

    # If a file-based component selected (define file-based components) note for future
    # the type to filter (eg file-based) on could probably also be a config choice
    display_names <- config_schema$manifest_schemas$display_name[config_schema$manifest_schemas$type == "file"]
    # if folder_ID has not been updated yet
    if (is.null(selected$folder())) selected$folder(data_list$folders()[[input$dropdown_folder]])

    if (input$dropdown_template %in% display_names) {
      # make into a csv or table for file-based components already has entityId
      if ("entityId" %in% colnames(submit_data)) {
        # Convert this to JSON instead and submit
        write.csv(submit_data,
          file = "./manifests/synapse_storage_manifest.csv",
          quote = TRUE, row.names = FALSE, na = ""
        )
      } else {
        file_list <- storage_dataset_files(url=file.path(api_uri, "v1/storage/dataset/files"),
                                           asset_view = project_synID,
                                            dataset_id = selected$folder(),
                                            input_token=access_token)
        data_list$files <<- list2Vector(file_list)

        # better filename checking is needed
        # TODO: crash if no file existing
        files_df <- stack(data_list$files())
        # adds entityID, saves it as synapse_storage_manifest.csv, then associates with synapse files
        colnames(files_df) <- c("entityId", "Filename")
        files_entity <- inner_join(submit_data, files_df, by = "Filename")
        # convert this to JSON instead and submit
        write.csv(files_entity,
          file = "./manifests/synapse_storage_manifest.csv",
          quote = TRUE, row.names = FALSE, na = ""
        )
      }
      
      # schematic rest api to submit metadata
      # This validates AND submits the data to Synapse
      # Returns synapse table ID if successful
      manifest_id <- model_submit(url=file.path(api_uri, "v1/model/submit"),
                                       schema_url = Sys.getenv("DCA_MODEL_INPUT_DOWNLOAD_URL"),
                                    data_type=selected$schema(),
                              dataset_id=selected$folder(),
                              input_token=access_token,
                              restrict_rules=FALSE,
                              #csv_file="./manifests/synapse_storage_manifest.csv")
                              json_str = toJSON(inFile$data()),
                              asset_view=selected$master_fileview())
      
      #> xml_text(xml_child(content(req3), "head/title"))
      #[1] "jsonschema.exceptions.ValidationError: Manifest could not be validated under provided data model. Validation failed with the following errors: [[2, 'Wrong schema', \"'HTAN Parent ID' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'Storage Method' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'Protocol Link' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'Acquisition Method Type' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'Collection Days from Index' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'Fixative Type' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'HTAN Biospecimen ID' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'Biospecimen Type' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'Processing Days from Index' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'Timepoint Label' is a required property\", 'Wrong schema'], [2, 'Wrong schema', \"'Site of Resection or Biopsy' is a required property\", 'Wrong schema']] // Werkzeug Debugger"
      #
      ### TODO:
      # Handle validation error results
      #
      # associates metadata with data and returns manifest id
      manifest_path <- tags$a(href = paste0("synapse.org/#!Synapse:", manifest_id), manifest_id, target = "_blank")
  
      # if no error
      if (startsWith(manifest_id, "syn") == TRUE) {
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
        )), sleep = 3)
      }
    } else {
      # if not file-based type template
      # convert this to JSON and submit
      write.csv(submit_data,
        file = "./manifests/synapse_storage_manifest.csv", quote = TRUE,
        row.names = FALSE, na = ""
      )

      # associates metadata with data and returns manifest id
      manifest_id <- model_submit(url=file.path(api_uri, "v1/model/submit"),
                                    data_type=selected$schema(),
                                    schema_url = Sys.getenv("DCA_MODEL_INPUT_DOWNLOAD_URL"),
                                    dataset_id=selected$folder(),
                                    input_token=access_token,
                                    restrict_rules=FALSE,
                                    #csv_file="./manifests/synapse_storage_manifest.csv")
                                  json_str = toJSON(inFile$data()),
                                  asset_view=selected$master_fileview())
      manifest_path <- tags$a(href = paste0("synapse.org/#!Synapse:", manifest_id), manifest_id, target = "_blank")
      # if uploaded provided valid synID message
      if (startsWith(manifest_id, "syn") == TRUE) {
        dcWaiter("hide")
        nx_report_success("Success!", HTML(paste0("Manifest submitted to: ", manifest_path)))

        # clear inputs
        sapply(clean_tags, FUN = hide)

        # rerenders fileinput UI
        output$fileInput_ui <- renderUI({
          fileInput("file1", "Upload CSV File", accept = c(
            "text/csv", "text/comma-separated-values",
            ".csv"
          ))
        })
        # renders empty df
        output$tbl_preview <- renderDT(datatable(as.data.frame(matrix(0,
          ncol = 0,
          nrow = 0
        ))))
      } else {
        dcWaiter("update", msg = HTML(paste0(
          "Uh oh, looks like something went wrong!",
          manifest_id, " is not a valid Synapse ID. Try again?"
        )), sleep = 3)
      }
    }
    # delete tmp manifest folder
    unlink("./manifests/synapse_storage_manifest.csv")
  })
})
