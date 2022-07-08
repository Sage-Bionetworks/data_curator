#' get all uploaded manifests based on provided folder list
#'
#' @param syn.store synapse storage object.
#' @param datasets a list of folder syn Ids, named by folder names
#' @param project.scope list of project ids used for cross-manifest validation
#' @return data frame that contains manifest essential information for dashboard
get_manifests <- function(syn.store, datasets, project.scope) {
  stopifnot(is.list(project.scope))
  # get table of all manifests information
  all_files <- syn.store$storageFileviewTable
  all_files <- all_files[all_files$name == "synapse_storage_manifest.csv", ]

  lapply(datasets, function(id) {
    # get manifest synapse id in each dataset folder
    # will a folder has multiple manifests? if so, need to iterate each manifest
    manifest_id <- all_files[all_files$parentId == id, "id"]

    # return empty tibble if no manifest or no component in the manifest
    df <- tibble()

    if (length(manifest_id) > 0) {
      manifest <- syn$get(manifest_id)
      # extract manifest essential information for dashboard
      manifest_path <- manifest["path"]
      manifest_df <- data.table::fread(manifest_path, data.table = F)
      modified_user <- syn$getUserProfile(manifest["properties"]["modifiedBy"])["userName"]

      if ("Component" %in% colnames(manifest_df) & nrow(manifest_df) > 0) {
        manifest_component <- manifest_df$Component[1]
        # validate manifest, if no error, output is list()
        # TODO: check with backend - ValueError: c("LungCancerTier3", "BreastCancerTier3", "ScRNA-seqAssay", "MolecularTest", "NaN", "") ...
        annotation_status <- tryCatch(
          metadata_model$validateModelManifest(
            manifest_path,
            manifest_component,
            restrict_rules = TRUE,
            project_scope = project.scope
          ),
          error = function(err) NULL
        )
        # clean validation res from schematic
        res <- validationResult(annotation_status, manifest_component, manifest_df)

        df <- tibble(
          synID = manifest["properties"]["id"],
          schema = manifest_component,
          createdOn = as.Date(manifest["properties"]["createdOn"]),
          modifiedOn = as.Date(manifest["properties"]["modifiedOn"]),
          modifiedUser = paste0("@", modified_user),
          path = manifest_path,
          folder = names(datasets)[which(datasets == id)],
          folderSynId = as.character(id),
          result = res$result,
          errorType = res$error_type,
          warnMsg = ifelse(is.null(res$warning_msg), "Valid", res$warning_msg)
        ) %>%
          filter(schema != "", schema != "NaN") # in case invalid manifest
      }
    }
    return(df)
  }) %>% bind_rows()
}

#' create data frame of data type requirements for selected data type
#'
#' @param datatype data type of selected data type or template.
#' @return list of requirements for \code{datatype} or string of \code{datatype} if no requirements found
get_requirement <- function(datatype) {
  requirement <- tryCatch(
    metadata_model$get_component_requirements(datatype, as_graph = TRUE),
    error = function(err) list()
  )

  # get a list of requirements, otherwise output unamed vector of datatype name
  if (length(requirement) == 0) {
    # it will be used to detect whether output has name in network
    requirement <- as.character(datatype)
  } else {
    requirement <- list2Vector(requirement)
  }

  return(requirement)
}


#' create data frame of data type requirements for all manifests
#'
#' @param manifest output from \code{get_manifests}.
#' @return data frame contains required data types for tree plot
get_all_requirements <- function(manifest, ncores = 5) {
  if (nrow(manifest) == 0) {
    return(data.frame(from = NA, to = NA, folder = NA, folderSynId = NA, nMiss = NA))
  } else {
    parallel::mclapply(1:nrow(manifest), function(i) {
      # get all required data types
      out <- tryCatch(
        metadata_model$get_component_requirements(manifest$schema[i], as_graph = TRUE),
        error = function(err) list()
      )
      # convert to a named list, output (name: to, value: from)
      out <- list2Vector(out)
      # calculate how many requirements are missing in each dataset
      n_miss <- sum(!union(names(out), out) %in% manifest$schema)
      # add data from dataset to its data type name
      from <- c(paste0("f:", manifest$folder[i]), as.character(out))
      to <- c(manifest$schema[i], names(out))
      # output nodes data as data frame


      data.frame(
        from = from,
        to = to,
        folder = c(manifest$folder[i]),
        folderSynId = c(manifest$folderSynId[i]),
        nMiss = c(n_miss)
      )
    }, mc.cores = ncores) %>% bind_rows()
  }
}