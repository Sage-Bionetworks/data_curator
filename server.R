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

  synapse_driver <- import("schematic.store.synapse")$SynapseStorage

  ######## session global variables ########
  # read config in
  config <- config_file
  config_schema <- as.data.frame(config[[1]])

  # mapping from display name to schema name
  template_namedList <- config_schema$schema_name
  names(template_namedList) <- config_schema$display_name

  # data available to the user
  syn_store <- NULL # gets list of projects they have access to

  data_list <- list(
    projects = reactiveVal(NULL), folders = reactiveVal(NULL),
    schemas = reactiveVal(template_namedList), files = reactiveVal(NULL)
  )
  # synapse ID of selected data
  selected <- list(
    project = reactiveVal(NULL), folder = reactiveVal(""),
    schema = reactiveVal(NULL), schema_type = reactiveVal(NULL)
  )

  isUpdateFolder <- reactiveVal(FALSE)

  tabs_list <- c("tab_data", "tab_template", "tab_upload")
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

    syn$login(authToken = access_token, rememberMe = FALSE)

    data_list$projects(
      tryCatch(
        {
          # get syn storage
          syn_store <<- synapse_driver(access_token = access_token)
          # get user's common projects
          list2Vector(syn_store$getStorageProjects())
        },
        error = function(e) {
          message(e$message)
          return(NULL)
        }
      )
    )

    if (is.null(data_list$projects()) || length(data_list$projects()) == 0) {
      dcWaiter("update", landing = TRUE, isPermission = FALSE)
    } else {

      # updates project dropdown
      lapply(c("header_dropdown_", "dropdown_"), function(x) {
        lapply(c(1, 3), function(i) {
          updateSelectInput(session, paste0(x, dropdown_types[i]),
            choices = sort(names(data_list[[i]]()))
          )
        })
      })

      user_name <- syn$getUserProfile()$userName

      if (!syn$is_certified(user_name)) {
        dcWaiter("update", landing = TRUE, isCertified = FALSE)
      } else {
        # update waiter loading screen once login successful
        dcWaiter("update", landing = TRUE, userName = user_name)
      }
    }
  })

  ######## Arrow Button ########
  lapply(1:3, function(i) {
    switchTabServer(id = paste0("switchTab", i), tabId = "tabs", tab = reactive(input$tabs)(), tabList = tabs_list, parent = session)
  })

  ######## Header Dropdown Button ########
  # Adjust header selection dropdown based on tabs
  observe({
    if (input[["tabs"]] == "tab_data") {
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
      project_id <- data_list$projects()[input[[paste0(x, "project")]]]

      # gets folders per project
      folder_list <- syn_store$getStorageDatasetsInProject(project_id) %>% list2Vector()
      folder_names <- ifelse(length(folder_list) > 0, sort(names(folder_list)), " ")

      # update folder names
      updateSelectInput(session, paste0(x, "folder"), choices = folder_names)

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

  ######## Update Folder ########
  # update selected folder synapse id and name
  observeEvent(input$dropdown_folder, {
    selected$folder(data_list$folders[[input$dropdown_folder]])
    # clean tags in generating-template tab
    sapply(clean_tags[1:2], FUN = hide)
  })

  ######## Update Template ########
  # update selected schema template name
  observeEvent(input$dropdown_datatype, {
    # update reactive selected values for schema
    selected$schema(data_list$schemas()[input$dropdown_datatype])
    schema_type <- config_schema$type[which(config_schema$display_name == input$dropdown_datatype)]
    selected$schema_type(schema_type)
    # clean all tags related with selected template
    sapply(clean_tags, FUN = hide)
  })

  ######## Dashboard ########
  dashboard(
    id = "dashboard",
    syn.store = syn_store,
    project.scope = selected$project,
    schema = selected$schema,
    schema.display.name = reactive(input$dropdown_datatype),
    disable.ids = c("box_pick_project", "box_pick_manifest"),
    ncores = ncores
  )

  ######## Template Google Sheet Link ########
  # validate before generating template
  observeEvent(c(selected$folder(), selected$schema(), input$tabs), {
    req(input$tabs %in% c("tab_template", "tab_validate"))

    warn_text <- NULL

    if (length(data_list$folder) == 0) {
      # add warning if there is no folder in the selected project
      warn_text <- paste0(
        "please create a folder in the ",
        strong(sQuote(input$dropdown_project)),
        " prior to submitting templates."
      )
    } else if (selected$schema_type() == "file") {
      # check number of files if it's file-based template

      dcWaiter("show", msg = paste0("Getting files in ", input$dropdown_folder, "..."))
      # get file list in selected folder
      file_list <- syn_store$getFilesInStorageDataset(selected$folder())

      # update files list in the folder
      data_list$files(list2Vector(file_list))

      dcWaiter("hide")

      if (length(data_list$files()) == 0) {
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
    }

    # if there is warning from above checks
    req(warn_text)
    # display warnings
    output$text_template_warn <- renderUI(tagList(br(), span(class = "warn_msg", HTML(warn_text))))
    show("div_template_warn")
  })

  # generate template
  observeEvent(input$btn_template, {

    # loading screen for template link generation
    dcWaiter("show", msg = "Generating link...")

    manifest_url <-
      metadata_model$getModelManifest(
        title = paste0(config$community, " ", input$dropdown_datatype),
        rootNode = selected$schema(),
        filenames = switch((selected$schema_type() == "file") + 1,
          NULL,
          as.list(names(data_list$files()))
        ),
        datasetId = selected$folder()
      )

    # generate link
    output$text_template <- renderUI(
      tags$a(id = "template_link", href = manifest_url, list(icon("hand-point-right"), manifest_url), target = "_blank")
    )

    dcWaiter("hide", sleep = 1)

    nx_confirm(
      inputId = "btn_template_confirm",
      title = "Go to the template now?",
      message = paste0("click 'Go' to edit your ", sQuote(input$dropdown_datatype), " template on the google sheet"),
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

    annotation_status <-
      tryCatch(
        metadata_model$validateModelManifest(
          manifestPath = inFile$raw()$datapath,
          rootNode = selected$schema(),
          restrict_rules = TRUE, # set true to disable great expectation
          project_scope = list(selected$project())
        ),
        error = function(e) {
          message(e$message)
          return(NULL)
        }
      )

    # validation messages
    validation_res <- validationResult(annotation_status, input$dropdown_datatype, inFile$data())
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

    filled_manifest <- metadata_model$populateModelManifest(
      title = paste0(config$community, " ", input$dropdown_datatype),
      manifestPath = inFile$raw()$datapath,
      rootNode = selected$schema()
    )

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



    if (is.null(selected$folder())) {
      # add waiter if no folder selected
      dcWaiter("update", msg = paste0("Please select a folder to submit"), spin = spin_pulsar(), sleep = 2.5)
    }

    # abort submission if no folder selected
    req(selected$folder())

    tmp_out_dir <- "./manifest"
    tmp_file_path <- file.path(tmp_out_dir, "synapse_storage_manifest.csv")
    dir.create(tmp_out_dir, showWarnings = FALSE)

    # reads file csv again
    submit_data <- csvInfileServer("inputFile")$data()

    # If a file-based component selected (define file-based components) note for future
    # the type to filter (eg file-based) on could probably also be a config choice
    display_names <- config_schema$manifest_schemas$display_name[config_schema$manifest_schemas$type == "file"]

    if (input$dropdown_datatype %in% display_names) {
      # make into a csv or table for file-based components already has entityId
      if ("entityId" %in% colnames(submit_data)) {
        write.csv(submit_data,
          file = tmp_file_path,
          quote = TRUE, row.names = FALSE, na = ""
        )
      } else {
        file_list <- syn_store$getFilesInStorageDataset(selected$folder())
        data_list$files(list2Vector(file_list))

        # better filename checking is needed
        # TODO: crash if no file existing
        files_df <- stack(data_list$files())
        # adds entityID, saves it as synapse_storage_manifest.csv, then associates with synapse files
        colnames(files_df) <- c("entityId", "Filename")
        files_entity <- inner_join(submit_data, files_df, by = "Filename")

        write.csv(files_entity,
          file = tmp_file_path,
          quote = TRUE, row.names = FALSE, na = ""
        )
      }

      # associates metadata with data and returns manifest id
      manifest_id <- syn_store$associateMetadataWithFiles(
        schemaGenerator = schema_generator,
        metadataManifestPath = tmp_file_path,
        datasetId = selected$folder(),
        manifest_record_type = "table",
        restrict_manifest = FALSE
      )
      manifest_path <- tags$a(href = paste0("https://www.synapse.org/#!Synapse:", manifest_id), manifest_id, target = "_blank")

      # add log message
      message(paste0("Manifest :", sQuote(manifest_id), " has been successfully uploaded"))

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
      write.csv(submit_data,
        file = tmp_file_path, quote = TRUE,
        row.names = FALSE, na = ""
      )

      # associates metadata with data and returns manifest id
      manifest_id <- syn_store$associateMetadataWithFiles(
        schemaGenerator = schema_generator,
        metadataManifestPath = tmp_file_path,
        datasetId = selected$folder(),
        manifest_record_type = "table",
        restrict_manifest = FALSE
      )
      manifest_path <- tags$a(href = paste0("https://www.synapse.org/#!Synapse:", manifest_id), manifest_id, target = "_blank")

      # add log message
      message(paste0("Manifest :", sQuote(manifest_id), " has been successfully uploaded"))

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
    unlink(tmp_file_path)
  })
})
