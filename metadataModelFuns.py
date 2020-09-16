# import json
import os
import sys
from schematic.models.metadata import MetadataModel
from schematic.manifest.generator import ManifestGenerator

from schematic.utils.config_utils import load_yaml
from definitions import ROOT_DIR, CONFIG_PATH, CREDS_PATH, DATA_PATH

# load config data from yaml file into config_data dict
config_data = load_yaml(CONFIG_PATH)

if config_data is None:
    sys.exit("Your config file may be empty.")

#inputMModelLocation = "./schemas/exampleSchemaReq.jsonld"
#inputMModelLocation = "./HTAN-data-pipeline/schemas/scRNASeq.jsonld"
# inputMModelLocation = "./HTAN-data-pipeline/schemas/HTAPP.jsonld"
inputMModelLocation = os.path.join(DATA_PATH, config_data["model"]["input"]["location"])
inputMModelLocationType = config_data["model"]["input"]["file_type"]
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
