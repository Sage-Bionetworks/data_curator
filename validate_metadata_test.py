from MetadataModel import MetadataModel

#inputMModelLocation = "./schemas/exampleSchemaReq.jsonld"
inputMModelLocation = "/Users/xdoan/Shell/HTAN-data-pipeline/schemas/scRNASeq.jsonld"
inputMModelLocationType = "local"
datasetType = "scRNASeq"

mm = MetadataModel(inputMModelLocation, inputMModelLocationType)

# manifest_path = "/Users/xdoan/Shell/HTAN-data-pipeline/manifest.csv"
manifest_path = "/Users/xdoan/Desktop/HTAN_scRNASeq - Sheet1.csv"

annotation_status = mm.validateModelManifest(manifest_path, datasetType)

print(annotation_status)