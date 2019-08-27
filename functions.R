# detach("package:synapser", unload=TRUE)
# unloadNamespace("synapser")

library(reticulate) 
py_discover_config() #absolutely cannot use synapser or else the paths gets messed up
# py_available()
# conda_python()
use_condaenv('py3.5', required = TRUE ) ### use path to python 3.5 env

reticulate::import("sys")
reticulate::import_from_path("MetadataModel", path = "HTAN-data-pipeline")
### need pygsheets installed, pandas

source_python("synLoginFun.py")

source_python("metadataModelFuns.py")

# source_python("./HTAN-data-pipeline/storage_test_driver.py")

source_python("synStoreFuns.py")

### logs in and gets list of projects they have access to
projects_list <- get_projects_list
projects_namedList <- c()
for (i in seq_along(projects_list)) {
  projects_namedList[projects_list[[i]][[2]]] <- projects_list[[i]][[1]]
}


# selected_project <- "HTAN manifest test"
# synID <- projects_namedList[[selected_project]] ### get synID of selected project
# folder_list <- get_folder_list(synID)
# synID <- folder_list[[1]][[1]]
# file_list <- get_file_list(synID)
# filename_list <- rep(NA, length(file_list)) ### initialize list of needed length
# ### gets synID of the manifest
# for (i in seq_along(file_list) ) {
#   if (file_list[[i]][[2]][1] == "synapse_storage_manifest.csv") {
#     manifestID <- print(file_list[[i]][[1]][1]  )
#     entity <-syn_get(manifestID)
#     entity$path
#     populateModelManifest(entity$path, "scRNAseq")
#   }
# }