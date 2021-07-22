
# convert [list] parent_list: child_list1, child_list2
# to [vector]: child_list1 with name as child_list2
list2Vector <- function(list) {
  child <- sapply(list, `[[`, 1)
  names(child) <- sapply(list, `[[`, 2)

  return(child)
}

# get manifest info from getDatasetManifest()
collectManifestInfo <- function(manifest_info) {
  manifest <- NULL

  if (manifest_info != "") {
    manifest_id <- manifest_info["properties"]["id"]
    manifest_name <- manifest_info["properties"]["name"]
    manifest_path <- manifest_info["path"]
    create_date <- manifest_info["createdOn"]
    modified_date <- manifest_info["modifiedOn"]

    manifest_df <- data.table::fread(manifest_path)

    if ("Component" %in% colnames(manifest_df) & nrow(manifest_df) > 0) {
      manifest_component <- manifest_df[["Component"]][1]
      manifest <- list(list(manifest_id, manifest_component, create_date, modified_date, manifest_path))
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
      create = sapply(list, `[[`, c(1, 3)) %>% as.Date(),
      modify = sapply(list, `[[`, c(1, 4)) %>% as.Date(),
      path = sapply(list, `[[`, c(1, 5))
    ) %>% 
      filter(schema != "" & schema != "NaN") %>%
      # uncomment this to get unique component, otherwise use multiple manifest with the same component name
      # distinct(schema, .keep_all = TRUE) %>%
      tibble::rownames_to_column("folder")
  }

  return(df)
}

runTime <- function(expr) {
  t1 <- Sys.time()
  expr
  t2 <- Sys.time() - t1
  return(t2)
}