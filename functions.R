library(reticulate)
use_condaenv('py3.5', required = TRUE)

reticulate::import("sys")
reticulate::import_from_path("MetadataModel", path = "HTAN-data-pipeline")

source_python("metadataModelFuns.py")

# test <- source_python("/Users/xdoan/Shell/HTAN-data-pipeline/validate_metadata_test.py")

# write(manifest_url, file = "manifest_url.txt")
# manifest_url <- readLines("manifest_url.txt")
# manifest_url <- colnames(manifest_url)
# validateModelManifest("/Users/xdoan/Desktop/HTAN_scRNASeq - Sheet1.csv", "scRNASeq")
