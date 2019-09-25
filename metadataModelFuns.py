from MetadataModel import MetadataModel

import argparse
parser = argparse.ArgumentParser()
parser.parse_args()

#inputMModelLocation = "./schemas/exampleSchemaReq.jsonld"
#inputMModelLocation = "./HTAN-data-pipeline/schemas/scRNASeq.jsonld"
inputMModelLocation = "./HTAN-data-pipeline/schemas/HTAPP.jsonld"
inputMModelLocationType = "local"
# datasetType = "scRNASeq"

mm = MetadataModel(inputMModelLocation, inputMModelLocationType)

### function for getting model Manifest
#  mm.getModelManifest(datasetType, additionalMetadata = {"Filename":["MantonCB1_HiSeq_1_S1_L001_R1_001.fastq.gz"]} )
getModelManifest = mm.getModelManifest

### function for validating manifest
# mm.validateModelManifest(manifest_path, datasetType)
validateModelManifest = mm.validateModelManifest


### populates manifest with path to csv
populateModelManifest = mm.populateModelManifest
