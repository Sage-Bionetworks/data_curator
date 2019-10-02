# detach("package:synapser", unload=TRUE)
# unloadNamespace("synapser")

library(reticulate)
 
# # py_available()
# # conda_python()
use_condaenv('py3.5', required = TRUE ) ### use path to python 3.5 env
# py_discover_config() #absolutely cannot use synapser or else the paths gets messed up
# 
reticulate::import("sys")
reticulate::import_from_path("MetadataModel", path = "HTAN-data-pipeline")
### need pygsheets installed, pandas
# 
# source_python("synLoginFun.py")
# syn_login()

# source_python("metadataModelFuns.py")

# source_python("synStoreFuns.py", input$cookie)
source_python("synStore_Session.py")
# 6c6617b1-cbb0-423c-b9c5-e96905373cf1
# 
# synStore_obj <- syn_store("syn20446927", syn_login(sessionToken="6c6617b1-cbb0-423c-b9c5-e96905373cf1", rememberMe = FALSE) )
# get_projects_list(synStore_obj)

# syn_store("syn20446927", syn_login())
# source_python("synStoreFuns.py")


### logs in and gets list of projects they have access to
# projects_list <- get_projects_list(syn_store("syn20446927", syn_login(sessionToken=input$cookie, rememberMe = FALSE) ))
# projects_namedList <- c()
# for (i in seq_along(projects_list)) {
#   projects_namedList[projects_list[[i]][[2]]] <- projects_list[[i]][[1]]
# }

