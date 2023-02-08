#' get all uploaded manifests based on provided folder list
#'
#' @param syn.store synapse storage object
#' @param datasets a list of folder syn Ids, named by folder names
#' @param ncores number of cpu to run parallelization
#' @return data frame that contains manifest essential information for dashboard
get_dataset_metadata <- function(syn.store, datasets, ncores = 1, schematic_api="reticulate",
                                 access_token, fileview) {
  # TODO: if the component could be retrieve directly from storage object:
  # remove codes to download all manifests
  # get data for all manifests within the specified datasets
  file_view <- switch(schematic_api,
                      reticulate = syn.store$storageFileviewTable,
                      rest = get_asset_view_table(url = file.path(api_uri, "v1/storage/assets/tables"),
                                                  input_token = access_token,
                                                  asset_view=fileview)
                      ) %>%
    filter(grepl("synapse_storage_manifest_", name) & parentId %in% datasets)

  # datasets don't have a manifest
  ds_no_manifest <- datasets[which(!datasets %in% file_view$parentId)]

  manifest_info <- list()
  modified_user <- list()
  manifest_dfs <- list()
  # return empty data frame if no manifest or no component in the manifest
  metadata <- data.frame()
  
  # create with column names to prevent dplyr funcs from failing on empty df
  cols <- c(
    "SynapseID",
    "Component",
    "CreatedOn",
    "ModifiedOn",
    "ModifiedUser",
    "Path",
    "Folder",
    "FolderSynId"
  )
  cols <- setNames(rep("", length(cols)), cols)
  metadata <- bind_rows(cols)[0, ]

  lapply(file_view$parentId, function(dataset) {
    # get manifest's synapse id(s) in each dataset folder
    manifest_ids <- file_view$id[file_view$parentId == dataset]

    if (length(manifest_ids) > 0) {
      # in case, multiple manifests exist in the same dataset
      for (id in manifest_ids) {
        if (schematic_api == "reticulate"){
          info <- syn$get(id)
          manifest_info <<- append(manifest_info, info)
          user <- syn$getUserProfile(info["properties"]["modifiedBy"])["userName"]
          modified_user <<- append(modified_user, user)
        } else if (schematic_api == "rest"){
          info <- synapse_get(id = id, auth = access_token)
          manifest <- manifest_download(
            url = file.path(api_uri, "v1/manifest/download"),
            input_token = access_token,
            asset_view = fileview,
            dataset_id = info$parentId,
            as_json = TRUE
          )
          
          # refactor this not to write files but save in a object
          #tmp_man <- tempfile()
          info$Path <- NA_character_
          #write_csv(manifest, tmp_man)
          manifest_dfs[[id]] <<- manifest
          manifest_info <<- append(manifest_info, list(unlist(info)))
          user <- synapse_user_profile(auth=access_token)[["userName"]]
          modified_user <<- append(modified_user, user)
        }
        
      }
    }
  })

  if (length(manifest_info) > 0) {
    metadata <- parallel::mclapply(seq_along(manifest_info), function(i) {
      if (schematic_api == "reticulate"){
        info <- manifest_info[[i]]
        # extract manifest essential information for dashboard
        manifest_path <- info["path"]
        manifest_df <- data.table::fread(manifest_path)
        # keep invalid component values as 'Missing'
        manifest_component <- ifelse("Component" %in% colnames(manifest_df) & nrow(manifest_df) > 0,
                                     manifest_df$Component[1], "Unknown"
        )
        metadata <- data.frame(
          SynapseID = info["properties"]["id"],
          Component = manifest_component,
          CreatedOn = as.Date(info["properties"]["createdOn"]),
          ModifiedOn = as.Date(info["properties"]["modifiedOn"]),
          ModifiedUser = paste0("@", modified_user[[i]]),
          Path = manifest_path,
          Folder = names(datasets)[which(datasets == info["properties"]["parentId"])],
          FolderSynId = info["properties"]["parentId"]
        )
      } else if (schematic_api == "rest"){
        info <- manifest_info[[i]]
        # extract manifest essential information for dashboard
        manifest_path <- info["Path"]
        # See above - don't read from file, read from object
        #manifest_df <- data.table::fread(manifest_path)
        manifest_df <- manifest_dfs[[i]]
        # keep all manifests used for validation, even if it has invalid component value
        # if manifest doesn't have "Component" column, or empty, return NA for component
        manifest_component <- ifelse("Component" %in% colnames(manifest_df) & nrow(manifest_df) > 0,
                                     manifest_df$Component[1], NA_character_
        )
        metadata <- tibble(
          SynapseID = info["id"],
          Component = manifest_component,
          CreatedOn = as.Date(info["createdOn"]),
          ModifiedOn = as.Date(info["modifiedOn"]),
          ModifiedUser = paste0("@", modified_user[[i]]),
          Path = manifest_path,
          Folder = names(datasets)[which(datasets == info["parentId"])],
          FolderSynId = info["parentId"],
          manifest = manifest_df
        )
      }
    }, mc.cores = ncores) %>% bind_rows()
  }

  # add empty dataset ids even if there are no manifests
  metadata <- bind_rows(
    metadata,
    data.frame(
      SynapseID = ds_no_manifest,
      Folder = names(ds_no_manifest),
      FolderSynId = ds_no_manifest
    )
  )
  return(metadata)
}


#' validate all manifests in the metadata of a dataset
#'
#' @param metadata output from \code{get_dataset_metadata}.
#' @param project.scope list of project ids used for cross-manifest validation
#' @return data frame contains required data types for tree plot
validate_metadata <- function(metadata, project.scope, schematic_api, schema_url) {
  stopifnot(is.list(project.scope))

  if (nrow(metadata) == 0) {
    return(metadata)
  }

  lapply(1:nrow(metadata), function(i) {
    manifest <- metadata[i, ]
    if (is.na(manifest$Component)) {
      data.frame(
        Result = "invalid",
        ErrorType = "Out of Date",
        errorMsg = "No manifest found",
        WarnMsg = "No manifest found"
      )
    } else if (manifest$Component == "Unknown") {
      data.frame(
        Result = "invalid",
        ErrorType = "Out of Date",
        errorMsg = "'Component' is missing",
        WarnMsg = "'Component' is missing"
      )
    } else {
      validation_res <- switch(schematic_api,
        reticulate = manifest_validate_py(
          manifestPath = manifest$Path,
          rootNode = manifest$Component,
          restrict_rules = TRUE, # set true to disable great expectation
          project_scope = project.scope
        ),
        rest = manifest_validate(url=file.path(api_uri, "v1/model/validate"),
                                 data_type=manifest$Component,
                                 schema_url = schema_url,
                                 json_str = jsonlite::toJSON(manifest$manifest))
      )
      # clean validation res from schematicpy
      clean_res <- validationResult(validation_res, manifest$Component, dashboard = TRUE)

      data.frame(
        Result = clean_res$result,
        # change wrong schema to out-of-date type
        ErrorType = if_else(clean_res$error_type == "Wrong Schema", "Out of Date", clean_res$error_type),
        errorMsg = if_else(is.null(clean_res$error_msg[1]), "Valid", clean_res$error_msg[1]),
        WarnMsg = if_else(length(clean_res$warning_msg) == 0, "Valid", clean_res$warning_msg[1])
      )
    }
  }) %>%
    bind_rows() %>%
    cbind(metadata, .) # expand metadata with validation results
}

#' create a list of requirements for selected data type
#'
#' @param schema data type of selected data type or template.
#' @return list of requirements for \code{schema} or string of \code{schema} if no requirements found
get_schema_nodes <- function(schema, schematic_api, url, schema_url) {
  requirement <- tryCatch(
    switch(schematic_api,
           reticulate = get_component_requirements_py(schema, as_graph = TRUE),
           rest = model_component_requirements(
             url=url,
             schema_url=schema_url,
             source_component = schema,
             as_graph = TRUE)),
    error = function(e) {
      warning("'get_schema_nodes' failed: ", sQuote(schema), ":\n", e$message)
      return(list())
    }
  )

  if (length(requirement) == 0) {
    # return data type itself without name
    return(as.character(schema))
  } else {
    # return a list of requirements of the data type
    return(list2Vector(requirement))
  }
}


#' create data frame of data type requirements for all manifests
#'
#' @param metadata output from \code{get_dataset_metadata}.
#' @return data frame of nodes contains source and target used for tree plot
get_metadata_nodes <- function(metadata, ncores = 1, schematic_api,
                               schema_url, url) {
  if (nrow(metadata) == 0) {
    return(data.frame(from = NA, to = NA, folder = NA, folderSynId = NA, nMiss = NA))
  } else {
    parallel::mclapply(1:nrow(metadata), function(i) {
      manifest <- metadata[i, ]
      # get all required data types
      nodes <- tryCatch(
        switch(schematic_api,
               reticulate = get_component_requirements_py(manifest$Component, as_graph = TRUE),
               rest = model_component_requirements(
                 url=url,
                 schema_url=schema_url,
                 source_component = manifest$Component,
                 as_graph = TRUE)
               ),
        error = function(e) {
          warning("'get_metadata_nodes' failed: ", sQuote(manifest$Component), ":\n", e$message)
          return(list())
        }
      ) %>% list2Vector()

      source <- as.character(nodes)
      target <- names(nodes)

      # count how many requirements are missing in each dataset
      n_miss <- sum(!union(target, source) %in% metadata$Component)

      # create data frame for tree plot
      data.frame(
        from = c(paste0("f:", manifest$Folder), source),
        to = c(manifest$Component, target),
        folder = c(manifest$Folder),
        folder_id = c(manifest$FolderSynId),
        n_miss = c(n_miss)
      )
    }, mc.cores = ncores) %>% bind_rows()
  }
  
}
