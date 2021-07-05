
# get manifest info from getDatasetManifest()
collectManifestInfo <- function(manifest_info) {

  manifest <- NULL

  if (manifest_info != "") {

    manifest_id <- manifest_info["properties"]["id"]
    manifest_name = manifest_info["properties"]["name"]
    manifest_path <- manifest_info["path"]
    create_date <- manifest_info["createdOn"]
    modified_date <- manifest_info["modifiedOn"]

    manifest_df <- data.table::fread(manifest_path)

    if ("Component" %in% colnames(manifest_df) & nrow(manifest_df) > 0) {
      
      manifest_component <- manifest_df[["Component"]][1]
      manifest <- list(list(manifest_id, manifest_component, create_date, modified_date))
    }
  }
  
  return(manifest)
}

# get information of manifest from output list of collectManifestInfo()
extractManifests <- function(list) {

  list <- list[lengths(list) != 0]
  df <- data.frame()

  if (length(list) != 0) {
    df <- data.frame(
      synID = sapply(list, `[[`, c(1, 1)),
      schema = sapply(list, `[[`, c(1, 2)),
      create = sapply(list, `[[`, c(1, 3)),
      modify = sapply(list, `[[`, c(1, 4))
    ) %>% 
      filter(schema != "" & schema != "NaN") %>% 
      distinct(schema, .keep_all = TRUE)
  }

  return(df)
}
