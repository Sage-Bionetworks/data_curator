#' get all uploaded manifests based on provided folder list
#' 
#' @param syn synapse objecy.
#' @param datasets a list of folder syn Ids, named by folder names
#' @param downloadFolder the folder to save manifest, only if downloadFile is TRUE
#' @return data frame that contains manifest essential information for dashboard
getManifests <- function(syn, datasets, downloadFile=TRUE, downloadFolder) {

  sapply(datasets, function(id) {
    manifest <- synapse_driver$getDatasetManifest(syn, id,
      downloadFile = downloadFile,
      downloadPath = file.path(downloadFolder, id)
    ) 
    
    # return empty tibble if no manifest or no component in the manifest
    df <- tibble()
  
    # extract manifest essential information for dashboard
    if (manifest != "") {
      manifest_path <- manifest["path"]
      manifest_df <- data.table::fread(manifest_path)
      mofified_user <- syn_getUserProfile(manifest["properties"]["modifiedBy"])["userName"]

      if ("Component" %in% colnames(manifest_df) & nrow(manifest_df) > 0) {
        manifest_component <- manifest_df[["Component"]][1]

        df <- tibble(
          synID = manifest["properties"]["id"],
          schema = manifest_component,
          createdOn = as.Date(manifest["properties"]["createdOn"]),
          modifiedOn = as.Date(manifest["properties"]["modifiedOn"]),
          modifiedUser = paste0("@", mofified_user),
          path = manifest_path,
          folder = names(datasets)[which(datasets == id)]
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

  # get a list of requirements, otherwise output unamed vector of schema name
  if (length(requirement) == 0) {
    requirement <- as.character(template())
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


#' create validation table for uploaded data
#' 
#' @param manifest output from \code{getManifests}.
#' @return data frame contains quick validation results for dashboard
getManifestValidation <- function(manifest) {

  if (nrow(manifest) == 0) {
    valDF <- data.frame()
  } else {
    valDF <- lapply(1:nrow(manifest), function(i) {
      path <- manifest$path[i]
      component <- manifest$schema[i]
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

    data.frame(
      dataType = paste0(
        '<a href="https://www.synapse.org/#!Synapse:',
        manifest$synID, '" target="_blank">', manifest$schema, "</a>"
      ),
      Dataset = manifest$folder,
      Status = valDF$is_valid,
      Error = ifelse(valDF$error_type == "Wrong Schema", "Out of Date", valDF$error_type),
      createdOn = manifest$createdOn,
      lastModified = manifest$modifiedOn,
      userModified = manifest$modifiedUser,
      internalLinks = c("Pass/Fail")
    ) %>% arrange(Status)
  }
}
