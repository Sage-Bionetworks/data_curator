# import json
from schematic.models.metadata import MetadataModel

from schematic import CONFIG

config = CONFIG.load_config("./schematic/config.yml")

#inputMModelLocation = "./schemas/exampleSchemaReq.jsonld"
#inputMModelLocation = "./HTAN-data-pipeline/schemas/scRNASeq.jsonld"
# inputMModelLocation = "./HTAN-data-pipeline/schemas/HTAPP.jsonld"
inputMModelLocation = CONFIG["model"]["input"]["location"]
inputMModelLocationType = CONFIG["model"]["input"]["file_type"]

manifest_title = CONFIG["manifest"]["title"]
manifest_data_type = CONFIG["manifest"]["data_type"]
# datasetType = "scRNASeq"
# modelType = "TableA"

metadata_model = MetadataModel(inputMModelLocation, inputMModelLocationType)
metadata_model.getModelManifest(title=manifest_title, 
                                rootNode=manifest_data_type)

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
