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
  folder_synID <- reactiveVal("") # selected folder synapse ID
  template_schema_name <- reactiveVal(NULL) # selected template schema name
  template_type <- NULL # type of selected template

  isUpdateFolder <- reactiveVal(FALSE)
  datatype_list <- list(projects = NULL, folders = reactiveVal(NULL), manifests = template_namedList, files = NULL)

  tabs_list <- c("tab_instructions", "tab_data", "tab_template", "tab_upload")
  clean_tags <- c("div_template", "div_validate", NS("tbl_validate", "table"), "btn_val_gsheet", "btn_submit")

  # add box effects
  boxEffect(zoom = FALSE, float = TRUE)
  # remove dashboard box initially
  shinydashboardPlus::updateBox("dashboard", action = "remove")

  ######## Initiate Login Process ########
  # synapse cookies
  session$sendCustomMessage(type = "readCookie", message = list())

  # login page
  observeEvent(input$cookie, {
    # login and update session; otherwise, notify to login to Synapse first
    tryCatch(
      {
        syn_login(sessionToken = input$cookie, rememberMe = FALSE)

        # welcome message
        # output$title <- renderUI({
        #   titlePanel(h4(sprintf("Welcome, %s", syn_getUserProfile()$userName)))
        # })

        # updating global vars with values for projects
        synStore_obj <<- synapse_driver(token = input$cookie)

        # get_projects_list(synStore_obj)
        projects_list <- synapse_driver$getStorageProjects(synStore_obj)
        datatype_list$projects <<- list2Vector(projects_list)

        # updates project dropdown
        lapply(c("header_dropdown_", "dropdown_"), function(x) {
          updateSelectInput(session, paste0(x, "project"), choices = sort(names(datatype_list$projects)))
          updateSelectInput(session, paste0(x, "template"), choices = sort(names(template_namedList)))
        })

        # update waiter loading screen once login successful
        dcWaiter("update", isLogin = TRUE, isPass = TRUE, usrName = syn_getUserProfile()$userName)
      },
      error = function(err) {
        message(err) # write log error
        dcWaiter("update", isLogin = TRUE, isPass = FALSE)
      }
    )
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
      projectID <- datatype_list$projects[[input[[paste0(x, "project")]]]]

      # gets folders per project
      folder_list <- synapse_driver$getStorageDatasetsInProject(synStore_obj, projectID) %>% list2Vector()

      # updates foldernames
      updateSelectInput(session, paste0(x, "folder"), choices = sort(names(folder_list)))

      if (x == "dropdown_") {
        project_synID <<- projectID
        datatype_list$folders(folder_list)
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
  })

  ######## Update Dashboard Stats ########
  # all functions should not be executed until dashboard visable
  load_upload <- Waiter$new(
    id = "dashboard",
    html = tagList(spin_google(), h4("Loading", style = "color: #000")),
    color = transparent(0.2)
  )

  observeEvent(input$dashboard$visible, {
    if (!input$dashboard$visible) {
      Sys.sleep(0.3) # 0.3 is optimal
      show("dashboard_switch_container")
    }
  })

  observeEvent(input$dashboard_control, {
    hide("dashboard_switch_container")
    shinydashboardPlus::updateBox("dashboard", action = "restore")
  })
  
  # get all uploaded files in project
  upload_manifest <- eventReactive(c(datatype_list$folders(), input$dashboard$visible), {
    req(input$dashboard_control != 0 & input$dashboard$visible)
    load_upload$show() # initiate partial loading screen for generating plot
    DTableServer("tbl_dashboard_validate", data.frame(NULL))
    lapply(c("box_pick_project", "box_pick_manifest"), FUN = disable) # disable selection to prevent change before finish below fun
    all_manifest <- sapply(datatype_list$folders(), function(i) {
      synapse_driver$getDatasetManifest(synStore_obj, i, 
                                        downloadFile = TRUE, 
                                        downloadPath = file.path("manifests", i)) %>% 
      collectManifestInfo()
    }) %>% extractManifests()

    lapply(c("box_pick_project", "box_pick_manifest"), FUN = enable) # enable selection btns

    return(all_manifest)
  })

  # only process getting requirements of selected manifest when manifest change & box shown - !use reactive not input$dropdown
  template_req <- eventReactive(c(template_schema_name(), input$dashboard$visible), {
    req(input$dashboard_control != 0 & input$dashboard$visible)

    # get requirements, otherwise output unamed vector of schema name
    req_data <- tryCatch(metadata_model$get_component_requirements(template_schema_name(), as_graph = TRUE), error = function() list())
    if (length(req_data) == 0) req_data <- as.character(template_schema_name()) else req_data <- list2Vector(req_data)

    return(req_data)
  })

  all_require_manifest <- eventReactive(upload_manifest(), {
    req(input$dashboard_control != 0 & input$dashboard$visible)
    all_manifest <- upload_manifest()

    all_req <- lapply(1:nrow(all_manifest), function(i) {
      out <- tryCatch(metadata_model$get_component_requirements(all_manifest$schema[i], as_graph = TRUE), error = function(err) list())

      if (length(out) == 0) {
        return(data.frame())
      } else {
        out <- list2Vector(out)
        # check if the uploaded file contains any missed requirement
        has_miss <- ifelse(any(!union(names(out), out) %in% all_manifest$schema), TRUE, FALSE)
        folder_name <- all_manifest$folder[i]
        # names(has_miss) <- folder_name
        # change upload schema to folder names
        out[which(out == all_manifest$schema[i])] <- folder_name
        df <- data.frame(from = as.character(out), to = names(out), folder = rep(folder_name, length(out)), has_miss = rep(has_miss, length(out)))
        return(df)
      }
    }) %>% bind_rows()

    return(all_req)
  })

  quick_val <- eventReactive(upload_manifest(), {
    load_upload$show()

    all_manifest <- upload_manifest()

    if (nrow(all_manifest) == 0) {
      return(data.frame())
    } else {
      t <- runTime(
        valDF <- lapply(1:nrow(all_manifest), function(i) {
          path <- all_manifest$path[i]
          component <- all_manifest$schema[i]
          manifest_df <- data.table::fread(path)
          # if no error, output is list()
          # TODO: check with backend - ValueError: c("LungCancerTier3", "BreastCancerTier3", "ScRNA-seqAssay", "MolecularTest", "NaN", "") ...
          valRes <- tryCatch(metadata_model$validateModelManifest(path, component), error = function(err) "Out of Date")
          if (is.list(valRes)) {
            res <- validationResult(valRes, component, manifest_df)
          } else {
            res <- list(validationRes = "invalid", errorType = valRes)
          }
          # manifest[[2]] <- list(res$validationRes, res$errorType)
          out <- data.frame(is_valid = res$validationRes, error_type = res$errorType)
        }) %>% bind_rows()
      )

      return(valDF)
    }
  })

  # render dashboard plots when input data updated & box shown
  observeEvent(c(upload_manifest(), template_req(), input$dashboard$visible), {
    req(input$dashboard_control != 0 & input$dashboard$visible)
    # check list of requirments of selected template
    output$dashboard_tab1_title <- renderUI(
      p(paste0("Completion of requirements on the datatype: ", sQuote(input$dropdown_template))))
    checkListServer("checklist_template", upload_manifest(), template_req())
    # networks plot for requirements of selected template
    selectDataReqNetServer("template_network", upload_manifest(), template_req(), template_schema_name())
  
    output$dashboard_tab2_title <- renderUI({
      p(paste0("Completion of requirements in the project: ", sQuote(input$dropdown_project)))
    })

    output$dashboard_tab3_title <- renderUI({
      p(HTML(paste0(
        actionButton("btn_dashboard_validate", "Validate", class = "btn-primary-color"),
        "  your existing metadata in the project: ", sQuote(input$dropdown_project)
      )))
    })

    load_upload$hide() # hide the partial loading screen
  })

  observeEvent(c(all_require_manifest(), input$dashboard$visible), {
    req(input$dashboard_control != 0 & input$dashboard$visible)
    # tree plot for requirements of all uploaded data
    uploadDataReqTreeServer("upload_tree", upload_manifest(), all_require_manifest(), input$dropdown_project)
  })

  observeEvent(input$btn_dashboard_validate, {
    valRes <- isolate(quick_val())
    if (nrow(valRes) > 0) {
      df <- data.frame(
        Datatype = paste0(
          '<a href="https://www.synapse.org/#!Synapse:',
          upload_manifest()$synID, '" target="_blank">', upload_manifest()$schema, "</a>"
        ),
        Dataset = upload_manifest()$folder,
        Status = valRes$is_valid,
        Error = ifelse(valRes$error_type == "Wrong Schema", "Out of Date", valRes$error_type),
        createOn = upload_manifest()$create,
        lastModified = upload_manifest()$modify,
        nameModified = c("HTAN user"),
        internalLinks = c("Pass/Fail")
        ) %>%
        arrange(Status)

      DTableServer("tbl_dashboard_validate", df,
        highlight = "column", escape = FALSE,
        caption = htmltools::tags$caption(HTML(
          paste0(
            "Invalid Results: <b>", sum(df$Status == "invalid"), "</b>", "<br>",
            "Schematic Version: <code>v1.0.0</code> ",
            tags$a(icon("github"), style = "color:#000;", href = "https://github.com/Sage-Bionetworks/schematic", target = "_blank"),
            "</b>", "<br><br>",
            "Click on the datatype name to download your existing metadata."
          )
        )),
        ht.color = c("#82E0AA", "#F7DC6F"), ht.value = c("valid", "invalid"), ht.column = "Status",
        options = list(dom = "t", columnDefs = list(list(className = "dt-center", targets = "_all")))
      )
    }
    load_upload$hide()
  })

  # ######## Template Google Sheet Link ########
  observeEvent(c(input$dropdown_folder, input$tabs), {
    req(input$tabs == "tab_template")
    tmp_folder_synID <- datatype_list$folders()[[input$dropdown_folder]]
    req(tmp_folder_synID != folder_synID()) # if folder changes

    dcWaiter("show", msg = paste0("Getting files in ", input$dropdown_folder, "..."))
    # update selected folder ID
    folder_synID(tmp_folder_synID)

    # get file list in selected folder
    file_list <- synapse_driver$getFilesInStorageDataset(
      synStore_obj,
      folder_synID()
    )
    datatype_list$files <<- list2Vector(file_list)
    dcWaiter("hide")
  })

  # display warning message if folder is empty and data type is assay
  observeEvent(c(folder_synID(), template_schema_name()), {

    # hide tags when users select new template
    sapply(clean_tags, FUN = hide)

    req(input$tabs == "tab_template")
    hide("div_template_warn")
    template_type <<- config$manifest_schemas$type[match(template_schema_name(), template_namedList)]
    req(length(datatype_list$files) == 0 & template_type == "assay")
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
        filenames = switch((template_type == "assay") + 1, NULL, as.list(names(datatype_list$files))),
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
        template_schema_name()
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
        DTableServer("tbl_preview", data = inFile$data(), highlight = "row", ht.column = 1, ht.value = inFile$data()[, 1])
      } else {
        DTableServer("tbl_preview",
          data = inFile$data(),
          highlight = "column", ht.column = valRes$errorDT$Column, ht.value = valRes$errorDT$Value
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

    # If an assay component selected (define assay components) note for future
    # the type to filter (eg assay) on could probably also be a config choice
    assay_schemas <- config$manifest_schemas$display_name[config$manifest_schemas$type == "assay"]
    # if folder_ID has not been updated yet
    if (folder_synID() == "") folder_synID(datatype_list$folders()[[input$dropdown_folder]])

    if (input$dropdown_template %in% assay_schemas) {
      # make into a csv or table for assay components already has entityId
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
        "./tmp/synapse_storage_manifest.csv", folder_synID()
      )
      manifest_path <- paste0("synapse.org/#!Synapse:", manifest_id)
      # if no error
      if (startsWith(manifest_id, "syn") == TRUE) {
        dcWaiter("hide")
        nx_report_success("Success!", paste0("Manifest submitted to: ", manifest_path))

        # clean up inputfile
        sapply(clean_tags, FUN = hide)
        DTableServer("tbl_preview", data.frame(NULL))
        # TODO: input file not reset yet
        # reset(c(clean_tags, "inputFile", "tbl_preview")) if reset works
      } else {
        dcWaiter("update", msg = HTML(paste0(
          "Uh oh, looks like something went wrong!",
          manifest_id,
          " is not a valid Synapse ID. Try again?"
        )), sleep = 3)
      }
    } else {
      # if not assay type tempalte
      write.csv(submit_data,
        file = "./tmp/synapse_storage_manifest.csv", quote = TRUE,
        row.names = FALSE, na = ""
      )

      # associates metadata with data and returns manifest id
      manifest_id <- synapse_driver$associateMetadataWithFiles(
        synStore_obj,
        "./tmp/synapse_storage_manifest.csv", folder_synID()
      )
      manifest_path <- paste0("synapse.org/#!Synapse:", manifest_id)

      # if uploaded provided valid synID message
      if (startsWith(manifest_id, "syn") == TRUE) {
        dcWaiter("hide")
        nx_report_success("Success!", paste0("Manifest submitted to: ", manifest_path))

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
