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

### get a sheet prepopulated with an existing manifest; returns a url to a google sheet
# prepopulated_manifest_url = mm.populateModelManifest(manifest_path, datasetType)
populateModelManifest = mm.populateModelManifest
