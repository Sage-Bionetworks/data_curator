#' get all uploaded manifests based on provided folder list
#' 
#' @param synStoreObj synapse storage object.
#' @param datasets a list of folder syn Ids, named by folder names
#' @return data frame that contains manifest essential information for dashboard
getManifests <- function(synStoreObj, datasets) {

  all_files <- synStoreObj$storageFileviewTable
  all_files <- all_files[all_files$name == basename(synStoreObj$manifest), ]

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
        val_res <- tryCatch(metadata_model$validateModelManifest(manifest_path, manifest_component), error = function(err) "Out of Date")
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
          isValid = ifelse(res$validationRes == "valid", TRUE, FALSE),
          errorType = res$errorType
        ) %>% 
          filter(schema != "" & schema != "NaN")
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
#' @return data frame contains required data types for network plot
getManifestRequirements <- function(manifest) {

  lapply(1:nrow(manifest), function(i) {
    out <- tryCatch(metadata_model$get_component_requirements(manifest$schema[i], as_graph = TRUE), error = function(err) list())

    if (length(out) == 0) {
      return(data.frame())
    } else {
      out <- list2Vector(out)
      # check if the uploaded file contains any missed requirement
      has_miss <- ifelse(any(!union(names(out), out) %in% manifest$schema), TRUE, FALSE)
      folder_name <- manifest$folder[i]
      # names(has_miss) <- folder_name
      # change upload schema to folder names
      out[which(out == manifest$schema[i])] <- folder_name
      df <- data.frame(
        from = as.character(out),
        to = names(out),
        folder = rep(folder_name, length(out)),
        has_miss = rep(has_miss, length(out))
      )
      return(df)
    }
  }) %>% bind_rows()
}