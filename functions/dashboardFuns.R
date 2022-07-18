#' get all uploaded manifests based on provided folder list
#'
#' @param syn.store synapse storage object
#' @param datasets a list of folder syn Ids, named by folder names
#' @return data frame that contains manifest essential information for dashboard
get_dataset_metadata <- function(syn.store, datasets) {
  # TODO: if the component could be retrieve directly from storage object:
  # remove codes to download all manifests
  # get data for all manifests within the specified datasets
  file_view <- syn.store$storageFileviewTable %>%
    filter(name == "synapse_storage_manifest.csv" & parentId %in% datasets)

  manifest_info <- list()
  # return empty tibble if no manifest or no component in the manifest
  metadata <- NULL

  lapply(file_view$parentId, function(dataset) {
    # get manifest's synapse id(s) in each dataset folder
    manifest_ids <- file_view$id[file_view$parentId == dataset]

    if (length(manifest_ids) > 0) {
      # in case, multiple manifests exist in the same dataset
      for (id in manifest_ids) {
        manifest_info <<- append(manifest_info, syn$get(id))
      }
    }
  })

  if (length(manifest_info) > 0) {
    metadata <- lapply(manifest_info, function(info) {
      # extract manifest essential information for dashboard
      manifest_path <- info["path"]
      manifest_df <- data.table::fread(manifest_path)
      # keep all manifests used for validation, even if it has invalid component value
      # if manifest doesn't have "Component" column, or empty, return NA for component
      manifest_component <- ifelse("Component" %in% colnames(manifest_df) & nrow(manifest_df) > 0,
        manifest_df$Component[1], NA_character_
      )
      modified_user <- syn$getUserProfile(info["properties"]["modifiedBy"])["userName"]
      metadata <- data.frame(
        SynapseID = info["properties"]["id"],
        Component = manifest_component,
        CreatedOn = as.Date(info["properties"]["createdOn"]),
        ModifiedOn = as.Date(info["properties"]["modifiedOn"]),
        ModifiedUser = paste0("@", modified_user),
        Path = manifest_path,
        Folder = names(datasets)[which(datasets == info["properties"]["parentId"])],
        FolderSynId = info["properties"]["parentId"]
      )
    }) %>% bind_rows()
  }

  return(metadata)
}


#' validate all manifests in the metadata of a dataset
#'
#' @param metadata output from \code{get_dataset_metadata}.
#' @param project.scope list of project ids used for cross-manifest validation
#' @return data frame contains required data types for tree plot
validate_metadata <- function(metadata, project.scope) {
  stopifnot(is.list(project.scope))

  lapply(1:nrow(metadata), function(i) {
    manifest <- metadata[i, ]
    # validate manifest, if no error, output is list()
    # for invalid components, it will return NULL and relay as 'Out of Date', e.g.:
    # "LungCancerTier3", "BreastCancerTier3", "ScRNA-seqAssay", "MolecularTest", "NaN", "" ...
    validation_res <- tryCatch(
      metadata_model$validateModelManifest(
        manifest$Path,
        manifest$Component,
        restrict_rules = TRUE,
        project_scope = project.scope
      ),
      error = function(err) NULL
    )
    # clean validation res from schematicpy
    clean_res <- validationResult(validation_res, manifest$Component, dashboard = TRUE)
    data.frame(
      Result = clean_res$result,
      ErrorType = clean_res$error_type,
      WarnMsg = if_else(length(clean_res$warning_msg) == 0, "Valid", clean_res$warning_msg)
    )
  }) %>%
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
get_metadata_nodes <- function(metadata) {
  if (nrow(metadata) == 0) {
    return(data.frame(from = NA, to = NA, folder = NA, folderSynId = NA, nMiss = NA))
  } else {
    lapply(1:nrow(metadata), function(i) {
      manifest <- metadata[i, ]
      # get all required data types
      nodes <- tryCatch(
        metadata_model$get_component_requirements(manifest$Component, as_graph = TRUE),
        error = function(err) list()
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
    }) %>% bind_rows()
  }
}