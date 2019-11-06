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

###### dashboard dataframe 
### logs in and gets list of projects they have access to
projects_list <- get_projects_list
projects_namedList <- c()
for (i in seq_along(projects_list)) {
  projects_namedList[projects_list[[i]][[2]]] <- projects_list[[i]][[1]]
}
### for each component (biosamp, assay, clinical) 
### right now only assay 
### makes summary df
df <- data.frame()
for (i in seq_along(projects_namedList)) {
  ### get project
  projectName <- names(projects_namedList[i])
  project_ID <- projects_namedList[[i]]
  print(projectName)
  print(project_ID)
  df[i,1] <- projectName
  
  ### get folders
  folder_list <- get_folder_list(project_ID)
  # print(folder_list)
  folders_namedList <- c()
  for (i in seq_along(folder_list)) {
    folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
  }
  print(folders_namedList)
  
  folderNames <- names(folders_namedList)
  folder_ID <- folders_namedList[[i]]
  for (f in seq_along(folders_namedList)) {
    print(f)
    folderName <- names(folders_namedList[f])
    folder_ID <- folders_namedList[[f]]
    
    df[i,2] <- folderName
    print(folderName)
    print(folder_ID)
    
    manifest <- get_storage_manifest_path(folder_ID)
    if ( !is.null(manifest)) {
      manifest_df <- read.csv(manifest)
      ### how to cound empty cells vs all cells
      dim_val <- dim(manifest_df)
      total_cells <- dim_val[1] * dim_val[2]
      empty_cells <- table(is.na(manifest_df))
      per_filled <- (empty_cells[1] / total_cells )* 100
      df[i,3] <- total_cells
      df[i,4] <- per_filled
    }
  }
}