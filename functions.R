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

