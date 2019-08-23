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
#  mm.getModelManifest(datasetType, additionalMetadata = {"Filename":["MantonCB1_HiSeq_1_S1_L001_R1_001.fastq.gz"]} )
getModelManifest = mm.getModelManifest

### function for validating manifest
# mm.validateModelManifest(manifest_path, datasetType)
validateModelManifest = mm.validateModelManifest



### "Testing retrieval of project list from Synapse
get_projects_list = syn_store.getStorageProjects()

###print("Testing retrieval of folder list within a given storage project from Synapse")
#"syn19557917"
get_folder_list = syn_store.getStorageDatasetsInProject

### print("Testing retrieval of file list within a given storage dataseyt from Synapse")
# "syn19557948"
get_file_list = syn_store.getFilesInStorageDataset


### print("Testing association of antities with annotation from manifest")
# "./synapse_storage_manifest.csv", "syn20685746"
get_manifest_syn_id = syn_store.associateMetadataWithFiles
