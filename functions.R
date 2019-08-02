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

source_python("HTAN-data-pipeline/metadataModelFuns.py")

### ? only get filenames from bucket in filename view until data is actually uploaded after validation?

# source_python("HTAN-data-pipeline/metadata_model_driver.py")

# filled_manifest <- populateModelManifest("HTAN-data-pipeline/manifest.csv", "scRNASeq")
# getModelManifest("scRNASeq", filenames = c("1.txt", "2.txt", "3.txt"))
# anno_error <- validateModelManifest("HTAN-data-pipeline/manifest.csv", "scRNASeq")
### row, column, input value, allowed values

# str_names <- sprintf("str_%d", seq(length(anno_error)))
# in_vals <- sprintf("input_%d", seq(length(anno_error)))
# 
# for (i in seq_along(anno_error)) {
#   print(i)
#   row <- anno_error[[i]][1]
#   column <- anno_error[[i]][2]
#   in_val <- anno_error[[i]][3]
#   allowed_vals <- anno_error[[i]][4]
#   if (unlist(in_val) == "") {
#     in_val <- NA
#   } 
#   allowed_vales <- gsub("\t", ",", allowed_vals)
#   str_names[i] <- paste("At spreadsheet row ",
#                         row, "column ", column,
#                         "your value ", in_val,
#                         "is not an allowed value ", allowed_vals, sep=" ")
#   in_vals[i] <- in_val
# }
# list(unlist(in_vals))

# detach("package:synapser", unload=TRUE)
# unloadNamespace("synapser")

# render_dt = function(data, editable = 'cell', server = TRUE, ...) {
#   renderDT(data, selection = 'none', server = server, editable = editable, ...)
# }

# source_python("~/Shell/HTAN-data-pipeline/get_url_test.py")

# test <- source_python("/Users/xdoan/Shell/HTAN-data-pipeline/validate_metadata_test.py")

# write(manifest_url, file = "manifest_url.txt")
# manifest_url <- readLines("manifest_url.txt")
# manifest_url <- colnames(manifest_url)
# validateModelManifest("/Users/xdoan/Desktop/HTAN_scRNASeq - Sheet1.csv", "scRNASeq")
