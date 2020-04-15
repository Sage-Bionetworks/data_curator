# import json
# import os
from MetadataModel import MetadataModel
from ManifestGenerator import ManifestGenerator

#inputMModelLocation = "./schemas/exampleSchemaReq.jsonld"
#inputMModelLocation = "./HTAN-data-pipeline/schemas/scRNASeq.jsonld"
# inputMModelLocation = "./HTAN-data-pipeline/schemas/HTAPP.jsonld"
inputMModelLocation = "./HTAN-data-pipeline/schemas/HTAN.jsonld"
inputMModelLocationType = "local"
# datasetType = "scRNASeq"
# modelType = "TableA"

mm = MetadataModel(inputMModelLocation, inputMModelLocationType)

### function for getting model Manifest
#  mm.getModelManifest(modelType, additionalMetadata = {"Filename":["MantonCB1_HiSeq_1_S1_L001_R1_001.fastq.gz"]} )
getModelManifest = mm.getModelManifest

### function for validating manifest
# mm.validateModelManifest(manifest_path, datasetType)
validateModelManifest = mm.validateModelManifest

### populates manifest with path to csv
populateModelManifest = mm.populateModelManifest

### gets dependencies
# "Generating dependency graph and ordering dependencies")
# dependencies = mm.getOrderedModelNodes(component, "requiresDependency")
getDependencies = mm.getOrderedModelNodes
