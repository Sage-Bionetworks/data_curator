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
# folder_list <- get_folder_list("syn20687304")
# folders_namedList <- c()
# for (i in seq_along(folder_list)) {
#   folders_namedList[folder_list[[i]][[2]]] <- folder_list[[i]][[1]]
# }
# 
# folder_synID <- folders_namedList[[ "HCA_Census_of_Immune_Cells" ]]
# 
# file_list <- get_file_list(folder_synID)
# file_namedList <- c()
# for (i in seq_along(file_list)) {
#   file_namedList[file_list[[i]][[2]]] <- file_list[[i]][[1]]
# }
# names(file_namedList)
# infile <- readr::read_csv("/tmp/synapse_storage_manifest.csv", na = c("", "NA"))
# files_df <- stack(file_namedList)
# colnames(files_df) <- c("entityId", "Filename" )
# files_entity <- inner_join(infile, files_df, by = "Filename")
# write.csv(files_entity, file= "/tmp/synapse_storage_manifest.csv", quote = FALSE, row.names = FALSE, na = "")
