#' get all uploaded manifests based on provided folder list
#'
#' @param synStoreObj synapse storage object.
#' @param datasets a list of folder syn Ids, named by folder names
#' @return data frame that contains manifest essential information for dashboard
getManifests <- function(synStoreObj, datasets) {
  all_files <- synStoreObj$storageFileviewTable
  # only uses the file named as 'synapse_storage_manifest.csv'
  all_files <- all_files[all_files$name == "synapse_storage_manifest.csv", ]

  sapply(datasets, function(id) {
    manifest_id <- all_files[all_files$parentId == id, "id"]

    # return empty tibble if no manifest or no component in the manifest
    df <- tibble()

    if (length(manifest_id) != 0) {
      manifest <- syn$get(manifest_id)
      # extract manifest essential information for dashboard
      manifest_path <- manifest["path"]
      manifest_df <- data.table::fread(manifest_path)
      mofified_user <- syn$getUserProfile(manifest["properties"]["modifiedBy"])["userName"]

      if ("Component" %in% colnames(manifest_df) & nrow(manifest_df) > 0) {
        manifest_component <- manifest_df[["Component"]][1]

        # validate manifest, if no error, output is list()
        # TODO: check with backend - ValueError: c("LungCancerTier3", "BreastCancerTier3", "ScRNA-seqAssay", "MolecularTest", "NaN", "") ...
        val_res <- tryCatch(
          metadata_model$validateModelManifest(
            manifest_path,
            manifest_component,
            restrict_rules = TRUE
          )[[1]],
          error = function(err) "Out of Date"
        )
        # clean validation res from schematic
        if (is.list(val_res)) {
          res <- validationResult(val_res, manifest_component, manifest_df)
        } else {
          res <- list(validationRes = "invalid", errorType = val_res)
        }

        df <- tibble(
          synID = manifest["properties"]["id"],
          schema = manifest_component,
          createdOn = as.Date(manifest["properties"]["createdOn"]),
          modifiedOn = as.Date(manifest["properties"]["modifiedOn"]),
          modifiedUser = paste0("@", mofified_user),
          path = manifest_path,
          folder = names(datasets)[which(datasets == id)],
          folderSynId = as.character(id),
          isValid = ifelse(res$validationRes == "valid", TRUE, FALSE),
          errorType = res$errorType
        ) %>%
          filter(schema != "" & schema != "NaN") # in case empty rows
      }
    }

    return(df)
  }) %>% bind_rows()
}

#' create data frame of data type requirements for selected data type
#'
#' @param datatype data type of selected template.
#' @return list of requirements for \code{datatype} or string of \code{datatype} if no requirements found
getDatatypeRequirement <- function(datatype) {
  requirement <- tryCatch(metadata_model$get_component_requirements(datatype, as_graph = TRUE), error = function(err) list())

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
#' @param manifest output from \code{getManifests}.
#' @return data frame contains required data types for tree plot
getManifestRequirements <- function(manifest) {
  if (nrow(manifest) == 0) {
    data.frame(from = NA, to = NA, folder = NA, folderSynId = NA, nMiss = NA)
  } else {
    lapply(1:nrow(manifest), function(i) {
      # get all required data types
      out <- tryCatch(metadata_model$get_component_requirements(manifest$schema[i], as_graph = TRUE), error = function(err) list())
      # convert to a named list, output (name: to, value: from)
      out <- list2Vector(out)
      # calculate how many misisng requirements each dataset
      n_miss <- sum(!union(names(out), out) %in% manifest$schema)
      # add data from dataset to its data type name
      from <- c(paste0("f:", manifest$folder[i]), as.character(out))
      to <- c(manifest$schema[i], names(out))
      # output nodes data as data frame
      data.frame(from = from, to = to, folder = c(manifest$folder[i]), folderSynId = c(manifest$folderSynId[i]), nMiss = c(n_miss))
    }) %>% bind_rows()
  }
}