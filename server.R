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
    api$access, "?", "redirect_uri=", APP_URL, "&grant_type=",
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

  # logs in and gets list of projects they have access to
  synStore_obj <- NULL
  projects_namedList <- NULL

  folders_namedList <- NULL
  folder_synID <- NULL # selected foler synapse ID

  template_schema_name <- NULL # selected template schema name
  file_namedList <- NULL

  ### mapping from display name to schema name
  schema_name <- config$manifest_schemas$schema_name
  display_name <- config$manifest_schemas$display_name
  schema_to_display_lookup <- data.frame(schema_name, display_name)

  tabs_list <- c("tab_instructions", "tab_data", "tab_template", "tab_upload")
  clean_tags <- c("div_download", "div_validate", NS("tbl_validate", "table"), "btn_val_gsheet", "btn_submit")

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
        output$title <- renderUI({
          titlePanel(h4(sprintf("Welcome, %s", syn_getUserProfile()$userName)))
        })

        # updating global vars with values for projects
        synStore_obj <<- synapse_driver(token = input$cookie)

        # get_projects_list(synStore_obj)
        projects_list <- synapse_driver$getStorageProjects(synStore_obj)
        projects_namedList <<- list2Vector(projects_list)

        # updates project dropdown
        updateSelectInput(session, "dropdown_project", choices = sort(names(projects_namedList)))

        # update waiter loading screen once login successful
        dc_waiter("update", isLogin = TRUE, isPass = TRUE, usrName = syn_getUserProfile()$userName)
      },
      error = function(err) {
        dc_waiter("update", isLogin = TRUE, isPass = FALSE)
      }
    )
  })


  ######## Arrow Button ########
  lapply(1:3, function(i) {
    switchTabServer(id = paste0("switchTab", i), tabId = "tabs", tab = reactive(input$tabs)(), tabList = tabs_list, parent = session)
  })

  ######## Update Folder List ########
  observeEvent(ignoreInit = TRUE, input$dropdown_project, {
    output$folders <- renderUI({
      # get synID of selected project
      project_synID <- projects_namedList[[input$dropdown_project]]

      # gets folders per project
      folder_list <- synapse_driver$getStorageDatasetsInProject(
        synStore_obj,
        project_synID
      )
      folders_namedList <<- list2Vector(folder_list)

      # updates foldernames
      selectInput(inputId = "dropdown_folder", label = "Folder:", choices = names(folders_namedList))
    })
  })

  # update selected folder ID
  observeEvent(input$dropdown_folder, {
    # TODO: check how different from using rectivateValues()
    folder_synID <<- folders_namedList[[input$dropdown_folder]]
  })

  ######## Update Template ########
  output$manifest_display_name <- renderUI({
    selectInput(inputId = "dropdown_template", label = "Template:", choices = display_name)
  })
  # update selected schema template name
  observeEvent(input$dropdown_template, {
    template_type_df <- schema_to_display_lookup[match(input$dropdown_template, schema_to_display_lookup$display_name),
      1,
      drop = F
    ]
    template_schema_name <<- as.character(template_type_df$schema_name)
  })

  # hide tags when users select new template
  observeEvent(
    {
      input$dropdown_folder
      input$dropdown_template
    },
    {
      sapply(clean_tags, FUN = hide)
    }
  )

  ######## Template Google Sheet Link ########
  observeEvent(input$btn_download, {

    # loading screen for template link generation
    dc_waiter("show", msg = "Generating link...")

    if (is.null(input$dropdown_template)) {
      output$text_download <- renderUI({
        tags$span(class = "error_msg", HTML("Please <b>select a template</b> from the 'Select your Dataset' tab !"))
      })
    } else {
      # checks if a manifest already exists
      existing_manifestID <- synapse_driver$getDatasetManifest(
        synStore_obj,
        folder_synID
      )
      # if there isn't an existing manifest make a new one
      if (existing_manifestID == "") {
        # get file list in selected folder
        # don't put in the observation of folder dropdown
        # it will crash if users switch folders too often
        file_list <- synapse_driver$getFilesInStorageDataset(
          synStore_obj,
          folder_synID
        )
        file_namedList <<- list2Vector(file_list)

        manifest_url <- metadata_model$getModelManifest(paste0(config$community," ", input$template_type), 
                                                        template_type, 
                                                        filenames = as.list(filename_list),
                                                        datasetId = folder_synID)
        # make sure not scalar if length of list is 1 in R
        # add in the step to convert names later
      } else {
        # if the manifest already exists
        manifest_entity <- syn_get(existing_manifestID)
        manifest_url <- metadata_model$populateModelManifest(paste0(
          config$community,
          " ", input$dropdown_template
        ), manifest_entity$path, template_schema_name)
      }

      output$text_download <- renderUI({
        tags$a(href = manifest_url, manifest_url, target = "_blank") ### add link to data dictionary when we have it ###
      })
    }

    dc_waiter("hide", sleep = 1)
    # display link
    show("div_download") # TODO: add progress bar on (loading) screen
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
    dc_waiter("show", msg = "Validating...")

    try(
      silent = TRUE,
      annotation_status <- metadata_model$validateModelManifest(
        inFile$raw()$datapath,
        template_schema_name
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
          options = list(
            pageLength = 50, scrollX = TRUE,
            scrollY = min(50 * length(annotation_status), 400), lengthChange = FALSE,
            info = FALSE, searching = FALSE
          )
        )
        show(NS("tbl_validate", "table"))
      }

      # highlight invalue cells in preview table
      if (valRes$errorType == "Wrong Schema") {
        DTableServer("tbl_preview", data = inFile$data(), highlight = "full")
      } else {
        DTableServer("tbl_preview",
          data = inFile$data(),
          highlight = "partial", hightlight.col = valRes$errorDT$Column, hightlight.value = valRes$errorDT$Value
        )
      }

      if (valRes$validationRes == "valid") {
        # show submit button
        output$submit <- renderUI({
          actionButton("btn_submit", "Submit to Synapse")
        })
        dc_waiter("update", msg = paste0(valRes$errorType, " Found !!! "), spin = spin_inner_circles(), sleep = 2.5)
      } else {
        show("btn_val_gsheet")
        dc_waiter("update", msg = paste0(valRes$errorType, " Found !!! "), spin = spin_pulsar(), sleep = 2.5)
      }
    } else {
      dc_waiter("hide")
    }

    show("div_validate")
  })

  # if user click gsheet_btn, generating gsheet
  observeEvent(input$btn_val_gsheet, {
    # loading screen for Google link generation
    dc_waiter("show", msg = "Generating link...")

    filled_manifest <- metadata_model$populateModelManifest(paste0(
      config$community,
      " ", input$dropdown_template
    ), inFile$raw()$datapath, template_schema_name)

    # rerender and change button to link
    output$val_gsheet <- renderUI({
      HTML(paste0("<a target=\"_blank\" href=\"", filled_manifest, "\">Edit on the Google Sheet.</a>"))
    })

    dc_waiter("hide")
  })


  ######## Submission Section ########
  observeEvent(input$btn_submit, {
    # loading screen for submitting data
    dc_waiter("show", msg = "Submitting...")

    # reads file csv again
    submit_data <- csvInfileServer("inputFile")$data()

    # IF an assay component selected (define assay components) note for future
    # the type to filter (eg assay) on could probably also be a config choice
    assay_schemas <- config$manifest_schemas$display_name[config$manifest_schemas$type ==
      "assay"]

    # and adds entityID, saves it as synapse_storage_manifest.csv, then associates
    # with synapse files
    if (input$dropdown_template %in% assay_schemas) {
      # make into a csv or table for assay components already has entityId
      if ("entityId" %in% colnames(submit_data)) {
        write.csv(submit_data,
          file = "./files/synapse_storage_manifest.csv",
          quote = TRUE, row.names = FALSE, na = ""
        )
      } else {
        file_list <- synapse_driver$getFilesInStorageDataset(
          synStore_obj,
          folder_synID
        )
        file_namedList <<- list2Vector(file_list)

        # better filename checking is needed
        files_df <- stack(file_namedList) # crash if no file existing
        colnames(files_df) <- c("entityId", "Filename")
        files_entity <- inner_join(submit_data, files_df, by = "Filename")

        write.csv(files_entity,
          file = "./files/synapse_storage_manifest.csv",
          quote = TRUE, row.names = FALSE, na = ""
        )
      }

      # associates metadata with data and returns manifest id
      manifest_id <- synapse_driver$associateMetadataWithFiles(
        synStore_obj,
        "./files/synapse_storage_manifest.csv", folder_synID
      )
      manifest_path <- paste0("synapse.org/#!Synapse:", manifest_id)
      # if no error
      if (startsWith(manifest_id, "syn") == TRUE) {
        rm("./files/synapse_storage_manifest.csv")
        dc_waiter("hide")
        nx_report_success("Success!", paste0("Manifest submitted to: ", manifest_path))

        # clean up inputfile
        sapply(clean_tags, FUN = hide)
        DTableServer("tbl_preview", data.frame(NULL))
        # TODO: input file not reset yet
        # reset(c(clean_tags, "inputFile", "tbl_preview")) if reset works
      } else {
        dc_waiter("update", msg = HTML(paste0(
          "Uh oh, looks like something went wrong!",
          manifest_id,
          " is not a valid Synapse ID. Try again?"
        )), sleep = 3)
        rm("/tmp/synapse_storage_manifest.csv")
      }
    } else {
      # if not assay type tempalte
      write.csv(submit_data,
        file = "./files/synapse_storage_manifest.csv", quote = TRUE,
        row.names = FALSE, na = ""
      )

      # associates metadata with data and returns manifest id
      manifest_id <- synapse_driver$associateMetadataWithFiles(
        synStore_obj,
        "./files/synapse_storage_manifest.csv", folder_synID
      )
      print(manifest_id)
      manifest_path <- paste0("synapse.org/#!Synapse:", manifest_id)

      # if uploaded provided valid synID message
      if (startsWith(manifest_id, "syn") == TRUE) {
        nx_report_success("Success!", paste0("Manifest submitted to: ", manifest_path))
        rm("./files/synapse_storage_manifest.csv")

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
        dc_waiter("update", msg = HTML(paste0(
          "Uh oh, looks like something went wrong!",
          manifest_id, " is not a valid Synapse ID. Try again?"
        )), sleep = 3)
        rm("/tmp/synapse_storage_manifest.csv")
      }
    }
  })
})
