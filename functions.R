# detach("package:synapser", unload=TRUE)
# unloadNamespace("synapser")

library(reticulate) 
# py_available()
# conda_python()
use_condaenv('py3.5', required = TRUE ) ### use path to python 3.5 env
py_discover_config() #absolutely cannot use synapser or else the paths gets messed up

reticulate::import("sys")
reticulate::import_from_path("MetadataModel", path = "HTAN-data-pipeline")
### need pygsheets installed, pandas

source_python("synLoginFun.py")
syn_login()

source_python("metadataModelFuns.py")

# source_python("./HTAN-data-pipeline/storage_test_driver.py")

source_python("synStoreFuns.py")

### logs in and gets list of projects they have access to
projects_list <- get_projects_list
projects_namedList <- c()
for (i in seq_along(projects_list)) {
  projects_namedList[projects_list[[i]][[2]]] <- projects_list[[i]][[1]]
}


### test for multiple folders per project
# synID <- projects_namedList[["HCA immune cells census"]] ### get synID of selected project
# folder_list <- get_folder_list(synID)
# folders_namedList <- c()
# for (i in seq_along(folder_list)) {
#   folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
# }
# 
# folderNames <- names(folders_namedList)
