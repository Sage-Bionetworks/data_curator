# This is the server logic for a Shiny web application.  You can find out more
# about building applications with Shiny here: http://shiny.rstudio.com This
# server has been modified to be used specifically on Sage Bionetworks Synapse
# pages to log into Synapse as the currently logged in user from the web portal
# using the session token.  https://www.synapse.org

shinyServer(function(input, output, session) {
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
  source_python("functions/synapse_func_alias.py")
  source_python("functions/metadata_model.py")
  # import module that contains SynapseStorage class
  synapse_driver <- import("schematic.store.synapse")$SynapseStorage
  # read config in
  config <- fromJSON("www/config.json")

  # mapping from display name to schema name
  template_namedList <- config$manifest_schemas$schema_name
  names(template_namedList) <- config$manifest_schemas$display_name

  synStore_obj <- NULL # gets list of projects they have access to
  project_synID <- NULL # selected project synapse ID
  folder_synID <- reactiveVal("") # selected foler synapse ID
  template_schema_name <- reactiveVal(NULL) # selected template schema name
  template_type <- NULL # type of selected template

  isUpdateFolder <- reactiveVal(FALSE)
  datatype_list <- list(projects = NULL, folders = NULL, manifests = template_namedList, files = NULL)

  tabs_list <- c("tab_instructions", "tab_data", "tab_template", "tab_upload")
  clean_tags <- c("div_template", "div_validate", NS("tbl_validate", "table"), "btn_val_gsheet", "btn_submit")

  # add box effects
  boxEffect(zoom = TRUE, float = TRUE)

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

    syn_login(authToken = access_token, rememberMe = FALSE)

    # updating syn storage
    tryCatch(synStore_obj <<- synapse_driver(access_token = access_token), error = function(e) NULL)

    if (is.null(synStore_obj)) {
      message("'synapse_driver' fails, run 'synapse_driver' to see detailed error")
      dcWaiter("update", landing = TRUE, isPermission = FALSE)
    } else {
      projects_list <- synapse_driver$getStorageProjects(synStore_obj)
      datatype_list$projects <<- list2Vector(projects_list)

      # updates project dropdown
      lapply(c("header_dropdown_", "dropdown_"), function(x) {
        lapply(c(1, 3), function(i) {
          updateSelectInput(session, paste0(x, datatypes[i]),
            choices = sort(names(datatype_list[[i]]))
          )
        })
      })

      user_name <- syn_getUserProfile()$userName

      if (!syn_is_certified(user_name)) {
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
    if (input[["tabs"]] %in% c("tab_instructions", "tab_data")) {
      hide("header_selection_dropdown")
    } else {
      show("header_selection_dropdown")
      addClass(id = "header_selection_dropdown", class = "open")
    }
  })

  # sync header dropdown with main dropdown
  lapply(datatypes, function(x) {
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
    lapply(datatypes, function(x) {
      updateSelectInput(session, paste0("dropdown_", x),
        selected = input[[paste0("header_dropdown_", x)]]
      )
    })
  })

  ######## Update Folder List ########
  lapply(c("header_dropdown_", "dropdown_"), function(x) {
    observeEvent(ignoreInit = TRUE, input[[paste0(x, "project")]], {
      # get synID of selected project
      projectID <- datatype_list$projects[[input[[paste0(x, "project")]]]]

      # gets folders per project
      folder_list <- synapse_driver$getStorageDatasetsInProject(synStore_obj, projectID) %>% list2Vector()

      # updates foldernames
      updateSelectInput(session, paste0(x, "folder"), choices = sort(names(folder_list)))

      if (x == "dropdown_") {
        project_synID <<- projectID
        datatype_list$folders <<- folder_list
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
    template_schema_name(template_namedList[match(input$dropdown_template, names(template_namedList))])
    template_type <<- config$manifest_schemas$type[match(template_schema_name(), template_namedList)]
  })

  ######## Template Google Sheet Link ########
  observeEvent(c(input$dropdown_folder, input$tabs), {
    req(input$tabs %in% c("tab_template", "tab_upload"))
    tmp_folder_synID <- datatype_list$folders[[input$dropdown_folder]]
    req(tmp_folder_synID != folder_synID()) # if folder changes

    # update selected folder ID
    folder_synID(tmp_folder_synID)

    if (input$tabs == "tab_template") {
      dcWaiter("show", msg = paste0("Getting files in ", input$dropdown_folder, "..."))
      # get file list in selected folder
      file_list <- synapse_driver$getFilesInStorageDataset(
        synStore_obj,
        folder_synID()
      )
      datatype_list$files <<- list2Vector(file_list)
      dcWaiter("hide")
    }
  })

  # display warning message if folder is empty and data type is file-based
  observeEvent(c(folder_synID(), template_schema_name()), {

    # hide tags when users select new template
    sapply(clean_tags, FUN = hide)

    req(input$tabs == "tab_template")
    hide("div_template_warn")

    req(length(datatype_list$files) == 0 & template_type == "file")
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
  })

  observeEvent(input$btn_template, {

    # loading screen for template link generation
    dcWaiter("show", msg = "Generating link...")

    manifest_url <-
      metadata_model$getModelManifest(paste0(config$community, " ", input$dropdown_template),
        template_schema_name(),
        filenames = switch((template_type == "file") + 1,
          NULL,
          as.list(names(datatype_list$files))
        ),
        datasetId = folder_synID()
      )

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
  inFile <- csvInfileServer("inputFile", colsAsCharacters = TRUE, keepBlank = TRUE)

  observeEvent(inFile$data(), {
    # hide the validation section when upload a new file
    sapply(clean_tags[-1], FUN = hide)
    # renders in DT for preview
    DTableServer("tbl_preview", inFile$data())
  })

  ######## Validation Section #######
  observeEvent(input$btn_validate, {

    # loading screen for validating metadata
    dcWaiter("show", msg = "Validating...")

    try(
      silent = TRUE,
      annotation_status <- metadata_model$validateModelManifest(
        inFile$raw()$datapath,
        template_schema_name(),
        restrict_rules = TRUE # set true to disable great expectation
      )
    )

    # validation messages
    valRes <- validationResult(annotation_status, input$dropdown_template, inFile$data())
    ValidationMsgServer("text_validate", valRes, input$dropdown_template, inFile$data())

    # if there is a file uploaded
    if (!is.null(valRes$validationRes)) {

      # output error messages as data table if it is invalid value type
      # render empty if error is not "invaid value" type - ifelse() will not work
      if (valRes$errorType == "Invalid Value") {
        DTableServer("tbl_validate", valRes$errorDT,
          rownames = FALSE, filter = "none",
          caption = "View all the error(s) highlighted in the preview table above",
          options = list(
            pageLength = 50, scrollX = TRUE,
            scrollY = min(50 * nrow(valRes$errorDT), 400), lengthChange = FALSE,
            info = FALSE, searching = FALSE
          )
        )
        show(NS("tbl_validate", "table"))
      }

      # highlight invalue cells in preview table
      if (valRes$errorType == "Wrong Schema") {
        DTableServer("tbl_preview", data = inFile$data(), highlight = "full")
      } else {
        DTableServer(
          "tbl_preview",
          data = inFile$data(),
          highlight = "partial", highlightValues = valRes$errorHighlight
        )
      }

      if (valRes$validationRes == "valid") {
        # show submit button
        output$submit <- renderUI(actionButton("btn_submit", "Submit to Synapse", class = "btn-primary-color"))
        dcWaiter("update", msg = paste0(valRes$errorType, " Found !!! "), spin = spin_inner_circles(), sleep = 2.5)
      } else {
        output$val_gsheet <- renderUI(
          actionButton("btn_val_gsheet", "  Generate Google Sheet Link", icon = icon("table"), class = "btn-primary-color")
        )
        dcWaiter("update", msg = paste0(valRes$errorType, " Found !!! "), spin = spin_pulsar(), sleep = 2.5)
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

    filled_manifest <- metadata_model$populateModelManifest(paste0(
      config$community,
      " ", input$dropdown_template
    ), inFile$raw()$datapath, template_schema_name())

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

    dir.create("./tmp", showWarnings = FALSE)

    # reads file csv again
    submit_data <- csvInfileServer("inputFile")$data()

    # If a file-based component selected (define file-based components) note for future
    # the type to filter (eg file-based) on could probably also be a config choice
    display_names <- config$manifest_schemas$display_name[config$manifest_schemas$type == "file"]
    # if folder_ID has not been updated yet
    if (folder_synID() == "") folder_synID(datatype_list$folders[[input$dropdown_folder]])

    if (input$dropdown_template %in% display_names) {
      # make into a csv or table for file-based components already has entityId
      if ("entityId" %in% colnames(submit_data)) {
        write.csv(submit_data,
          file = "./tmp/synapse_storage_manifest.csv",
          quote = TRUE, row.names = FALSE, na = ""
        )
      } else {
        file_list <- synapse_driver$getFilesInStorageDataset(synStore_obj, folder_synID())
        datatype_list$files <<- list2Vector(file_list)

        # better filename checking is needed
        # TODO: crash if no file existing
        files_df <- stack(datatype_list$files)
        # adds entityID, saves it as synapse_storage_manifest.csv, then associates with synapse files
        colnames(files_df) <- c("entityId", "Filename")
        files_entity <- inner_join(submit_data, files_df, by = "Filename")

        write.csv(files_entity,
          file = "./tmp/synapse_storage_manifest.csv",
          quote = TRUE, row.names = FALSE, na = ""
        )
      }

      # associates metadata with data and returns manifest id
      manifest_id <- synapse_driver$associateMetadataWithFiles(
        synStore_obj,
        "./tmp/synapse_storage_manifest.csv",
        folder_synID(),
        manifest_record_type = "table"
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
        file = "./tmp/synapse_storage_manifest.csv", quote = TRUE,
        row.names = FALSE, na = ""
      )

      # associates metadata with data and returns manifest id
      manifest_id <- synapse_driver$associateMetadataWithFiles(
        synStore_obj,
        "./tmp/synapse_storage_manifest.csv",
        folder_synID(),
        manifest_record_type = "table"
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
    # delete tmp manifest
    unlink("./tmp/synapse_storage_manifest.csv")
  })
})