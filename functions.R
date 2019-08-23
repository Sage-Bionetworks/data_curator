# detach("package:synapser", unload=TRUE)
# unloadNamespace("synapser")

library(reticulate) 
py_discover_config() #absolutely cannot use synapser or else the paths gets messed up
# py_available()
# conda_python()
use_condaenv('py3.5', required = TRUE ) ### use path to python 3.5 env
# use_python("/Users/xdoan/anaconda2/envs/py3.5/bin/python", required = TRUE)

reticulate::import("sys")
reticulate::import_from_path("MetadataModel", path = "HTAN-data-pipeline")
### need pygsheets installed

source_python("metadataModelFuns.py")

# source_python("./HTAN-data-pipeline/storage_test_driver.py")

source_python("synStoreFuns.py")
#named list of projects (name) and synID (value)

# projects_list <- get_projects_list
# projects_namedList <- c()
# for (i in seq_along(projects_list)) {
#   projects_namedList[projects_list[[i]][[2]]] <- projects_list[[i]][[1]]
# }
# projects_namedList[["HCA immune cells census"]]
# folder_list <- get_folder_list("syn20687304")
# folder <- folder_list[[1]][[2]]

# folder_list <- get_folder_list("syn20687304")
# folder <- folder_list[[1]][[2]]
# 
# file_list <- get_file_list("syn19557948")
# filename_list <- rep(NA, length(file_list)) ### initialize list of needed length
# for (i in seq_along(file_list) ) {
#   filename_list[i] <- file_list[[i]][[2]][1]
# }
# 
# get_manifest_syn_id("./HTAN-data-pipeline/synapse_storage_manifest.csv", "syn20685746")

