#' get all uploaded manifests based on provided folder list
#'
#' @param syn.store synapse storage object
#' @param datasets a list of folder syn Ids, named by folder names
#' @param ncores number of cpu to run parallelization
#' @return data frame that contains manifest essential information for dashboard
get_dataset_metadata <- function(syn.store, datasets, ncores = 1) {
  # TODO: if the component could be retrieve directly from storage object:
  # remove codes to download all manifests
  # get data for all manifests within the specified datasets
  file_view <- syn.store$storageFileviewTable %>%
    filter(name == "synapse_storage_manifest.csv" & parentId %in% datasets)

  # datasets don't have a manifest
  ds_no_manifest <- datasets[which(!datasets %in% file_view$parentId)]

  manifest_info <- list()
  modified_user <- list()
  # return empty data frame if no manifest or no component in the manifest
  # create with column names to prevent dplyr funcs failed
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
        info <- syn$get(id)
        manifest_info <<- append(manifest_info, info)
        user <- syn$getUserProfile(info["properties"]["modifiedBy"])["userName"]
        modified_user <<- append(modified_user, user)
      }
    }
  })

  if (length(manifest_info) > 0) {
    metadata <- parallel::mclapply(seq_along(manifest_info), function(i) {
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
    }, mc.cores = ncores) %>% bind_rows()
  }

  # add datasets even if there are no manifests
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
validate_metadata <- function(metadata, project.scope) {
  stopifnot(is.list(project.scope))

  if (nrow(metadata) == 0) {
    return(metadata)
  }

  parallel::mclapply(1:nrow(metadata), function(i) {
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
      validation_res <- tryCatch(
        metadata_model$validateModelManifest(
          manifestPath = manifest$Path,
          rootNode = manifest$Component,
          restrict_rules = TRUE, # set true to disable great expectation
          project_scope = project.scope
        ),
        # for invalid components, it will return NULL and relay as 'Out of Date', e.g.:
        # "LungCancerTier3", "BreastCancerTier3", "ScRNA-seqAssay", "MolecularTest", "NaN", "" ...
        error = function(e) {
          warning("'validateModelManifest' failed: ", sQuote(manifest$SynapseID), ":\n", e$message)
          return(NULL)
        }
      )
      # clean validation res from schematicpy
      clean_res <- validationResult(validation_res, manifest$Component, dashboard = TRUE)

      data.frame(
        Result = clean_res$result,
        # change wrong schema to out-of-date type
        ErrorType = if_else(clean_res$error_type == "Wrong Schema", "Out of Date", clean_res$error_type),
        errorMsg = if_else(is.null(clean_res$error_msg[1]), "Valid", clean_res$error_msg[1]),
        WarnMsg = if_else(length(clean_res$warning_msg) == 0, "Valid", clean_res$warning_msg)
      )
    }
  }, mc.cores = ncores) %>%
    bind_rows() %>%
    cbind(metadata, .) # expand metadata with validation results
}

#' create a list of requirements for selected data type
#'
#' @param schema data type of selected data type or template.
#' @return list of requirements for \code{schema} or string of \code{schema} if no requirements found
get_schema_nodes <- function(schema) {
  requirement <- tryCatch(
    metadata_model$get_component_requirements(schema, as_graph = TRUE),
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
get_metadata_nodes <- function(metadata, ncores = 1) {
  if (nrow(metadata) == 0) {
    return(data.frame(from = NA, to = NA, folder = NA, folderSynId = NA, nMiss = NA))
  } else {
    parallel::mclapply(1:nrow(metadata), function(i) {
      manifest <- metadata[i, ]
      # get all required data types
      nodes <- tryCatch(
        metadata_model$get_component_requirements(manifest$Component, as_graph = TRUE),
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
