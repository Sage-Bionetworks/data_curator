### if synapser used 
# detach("package:synapser", unload=TRUE)
# unloadNamespace("synapser")

library(reticulate)

use_condaenv('py3.5', required = TRUE ) ### use path to python 3.5 env

reticulate::import("sys")
reticulate::import_from_path("MetadataModel", path = "HTAN-data-pipeline")

source_python("synStore_Session.py")

### functions to get filename, foldername

### get folder_synID 
get_folder_synID <- function(synStore_obj, project_synID, selected_folder) {
  folder_list <- get_folder_list(synStore_obj, project_synID)
  folders_namedList <- c()
  for (i in seq_along(folder_list)) {
    folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
  }
  
  folder_synID <- folders_namedList[[selected_folder]]
  return(folder_synID)
}


### get file list
get_filename_list <- function(synStore_obj, folder_synID) {
  file_list <- get_file_list(synStore_obj, folder_synID)
  
  file_namedList <- c()
  for (i in seq_along(file_list)) {
    file_namedList[file_list[[i]][[2]]] <- file_list[[i]][[1]]
  }
  filename_list <- names(file_namedList)
  return(filename_list)
}
