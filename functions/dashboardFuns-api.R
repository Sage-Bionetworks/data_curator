#' get all uploaded manifests based on provided folder list
#'
#' @param syn.store synapse storage object
#' @param datasets a list of folder syn Ids, named by folder names
#' @param ncores number of cpu to run parallelization
#' @return data frame that contains manifest essential information for dashboard
get_dataset_metadata <- function(syn.store, datasets, ncores = 1, access_token, fileview) {
  # TODO: if the component could be retrieve directly from storage object:
  # remove codes to download all manifests
  # get data for all manifests within the specified datasets
  #file_view <- syn.store$storageFileviewTable %>%
  # either use branch that returns JSON or wait until it's available in devel
  file_view <- get_asset_view_table(url = file.path(api_uri, "v1/storage/assets/tables"), access_token = access_token,
                                    asset_view = fileview)
  file_view <- file_view %>%
    filter(name == "synapse_storage_manifest.csv" & parentId %in% datasets)

  manifest_info <- list()
  modified_user <- list()
  manifest_dfs <- list()
  # return empty data frame if no manifest or no component in the manifest
  metadata <- data.frame()

  lapply(file_view$parentId, function(dataset) {
    # get manifest's synapse id(s) in each dataset folder
    manifest_ids <- file_view$id[file_view$parentId == dataset]

    if (length(manifest_ids) > 0) {
      # in case, multiple manifests exist in the same dataset
      for (id in manifest_ids) {
        #info <- syn$get(id)
        info <- datacurator::synapse_get(id = id, auth = access_token)
        manifest <- manifest_download(
          url = file.path(api_uri, "v1/manifest/download"),
          asset_view = fileview,
          manifest_id = info$parentId,
          as_json = TRUE
        )
        
        # refactor this not to write files but save in a object
        #tmp_man <- tempfile()
        info$Path <- NA_character_
        #write_csv(manifest, tmp_man)
        manifest_dfs[[id]] <<- manifest
        manifest_info <<- append(manifest_info, list(unlist(info)))
        #user <- syn$getUserProfile(info["properties"]["modifiedBy"])["userName"]
        user <- datacurator::synapse_user_profile(auth = access_token)[["userName"]]
        modified_user <<- append(modified_user, user)
      }
      # manifest_info <- lapply(manifest_ids, function(x) datacurator::synapse_get(id = x, auth = access_token))
      # manifest_info <- bind_rows(manifest_info)
    }
  })
  
  if (length(manifest_info) > 0) {
    metadata <- parallel::mclapply(seq_along(manifest_info), function(i) {
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
        manifest = list(manifest_df)
      )
    }, mc.cores = ncores) %>% bind_rows()
  }

  return(metadata)
}


#' validate all manifests in the metadata of a dataset
#'
#' @param metadata output from \code{get_dataset_metadata}.
#' @param project.scope list of project ids used for cross-manifest validation
#' @return data frame contains required data types for tree plot
validate_metadata <- function(metadata, project.scope, access_token) {
  stopifnot(is.list(project.scope))
  if (nrow(metadata) == 0) {
    return(metadata)
  }
  lapply(seq_len(nrow(metadata)), function(i) {
    manifest <- metadata[i, ]
    # validate manifest, if no error, output is list()
    # for invalid components, it will return NULL and relay as 'Out of Date', e.g.:
    # "LungCancerTier3", "BreastCancerTier3", "ScRNA-seqAssay", "MolecularTest", "NaN", "" ...
    validation_res <- manifest_validate(url = file.path(api_uri, "v1/model/validate"),
                                        data_type = manifest$Component,
                                        schema_url = Sys.getenv("DCA_MODEL_INPUT_DOWNLOAD_URL"),
                                        json_str = jsonlite::toJSON(manifest$manifest),
                                        access_token=access_token,
                                        )
                                        #csv_file = manifest$Path)
    # Wait until you can pass a JSON to manifest_validate
    # clean validation res from schematicpy
    clean_res <- validationResult(validation_res, manifest$Component, dashboard = TRUE)
    data.frame(
      Result = clean_res$result,
      ErrorType = clean_res$error_type,
      WarnMsg = if_else(length(clean_res$warning_msg) == 0, "Valid", paste(clean_res$warning_msg, collapse = "; "))
    )
  }) %>%
    bind_rows() %>%
    cbind(metadata, .) # expand metadata with validation results
}

#' create a list of requirements for selected data type
#'
#' @param schema data type of selected data type or template.
#' @return list of requirements for \code{schema} or string of \code{schema} if no requirements found
get_schema_nodes <- function(schema, url = file.path(api_uri, "v1/model/component-requirements"), schema_url) {
  requirement <- tryCatch(
    model_component_requirements(
      url = url,
      schema_url = schema_url,
      source_component = schema,
      as_graph = TRUE
    ),
    error = function(err) list()
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
get_metadata_nodes <- function(metadata, ncores = 1, schema_url = Sys.getenv("DCA_MODEL_INPUT_DOWNLOAD_URL"), url = file.path(api_uri, "v1/model/component-requirements")) {
  if (nrow(metadata) == 0) {
    return(data.frame(from = NA, to = NA, folder = NA, folderSynId = NA, nMiss = NA))
  } else {
    metadata <- drop_na(metadata, "Component")
    parallel::mclapply(seq_len(nrow(metadata)), function(i) {
      manifest <- metadata[i, ]
      # get all required data types
      # nodes <- tryCatch(
      #   metadata_model$get_component_requirements(manifest$Component, as_graph = TRUE),
      #   error = function(err) list()
      # ) %>% list2Vector()
      nodes <- tryCatch(model_component_requirements(
        url,
        schema_url,
        source_component = manifest$Component,
        as_graph = TRUE
      ), error = function(err) list()) %>% list2Vector()

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
    }, mc.cores = ncores) %>% 
      bind_rows()
  }
}
