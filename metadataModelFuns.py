# import json
# import os
from ingresspipe.models.metadata import MetadataModel
from ingresspipe.manifest.generator import ManifestGenerator

#inputMModelLocation = "./schemas/exampleSchemaReq.jsonld"
#inputMModelLocation = "./HTAN-data-pipeline/schemas/scRNASeq.jsonld"
# inputMModelLocation = "./HTAN-data-pipeline/schemas/HTAPP.jsonld"
inputMModelLocation = "./data/schema_org_schemas/HTAN.jsonld"
inputMModelLocationType = "local"
# datasetType = "scRNASeq"
# modelType = "TableA"

metadata_model = MetadataModel(inputMModelLocation, inputMModelLocationType)

### function for getting model Manifest
#  mm.getModelManifest(modelType, additionalMetadata = {"Filename":["MantonCB1_HiSeq_1_S1_L001_R1_001.fastq.gz"]} )
# getModelManifest = mm.getModelManifest

# ### function for validating manifest
# # mm.validateModelManifest(manifest_path, datasetType)
# validateModelManifest = mm.validateModelManifest

# ### populates manifest with path to csv
# populateModelManifest = mm.populateModelManifest

# ### gets dependencies
# # "Generating dependency graph and ordering dependencies")
# # dependencies = mm.getOrderedModelNodes(component, "requiresDependency")
# getDependencies = mm.getOrderedModelNodes
