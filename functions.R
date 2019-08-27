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

get_manifest_syn_id("./HTAN-data-pipeline/synapse_storage_manifest.csv", "syn20685746")

