from MetadataModel import MetadataModel

import argparse
parser = argparse.ArgumentParser()
parser.parse_args()

#inputMModelLocation = "./schemas/exampleSchemaReq.jsonld"
inputMModelLocation = "./HTAN-data-pipeline/schemas/scRNASeq.jsonld"
inputMModelLocationType = "local"
# datasetType = "scRNASeq"

mm = MetadataModel(inputMModelLocation, inputMModelLocationType)

### function for getting model Manifest
# manifest_url = mm.getModelManifest(datasetType, filenames = ["1.txt", "2.txt", "3.txt"])
getModelManifest = mm.getModelManifest

### function for validating manifest
# mm.validateModelManifest(manifest_path, datasetType)
validateModelManifest = mm.validateModelManifest
