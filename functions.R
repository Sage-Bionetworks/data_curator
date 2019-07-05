library(reticulate)
use_condaenv('py3.5', required = TRUE)

reticulate::import("sys")
reticulate::import_from_path("MetadataModel", path = "HTAN-data-pipeline")

source_python("HTAN-data-pipeline/metadataModelFuns.py")

render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}

# source_python("~/Shell/HTAN-data-pipeline/get_url_test.py")

# test <- source_python("/Users/xdoan/Shell/HTAN-data-pipeline/validate_metadata_test.py")

# write(manifest_url, file = "manifest_url.txt")
# manifest_url <- readLines("manifest_url.txt")
# manifest_url <- colnames(manifest_url)
# validateModelManifest("/Users/xdoan/Desktop/HTAN_scRNASeq - Sheet1.csv", "scRNASeq")
