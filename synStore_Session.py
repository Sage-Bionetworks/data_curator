import synapseclient
from SynapseStorage import SynapseStorage

syn_store = SynapseStorage

### "Testing retrieval of project list from Synapse
get_projects_list = syn_store.getStorageProjects

###print("Testing retrieval of folder list within a given storage project from Synapse")
get_folder_list = syn_store.getStorageDatasetsInProject

### print("Testing retrieval of file list within a given storage dataseyt from Synapse")
get_file_list = syn_store.getFilesInStorageDataset

### print("Testing association of antities with annotation from manifest")
get_manifest_syn_id = syn_store.associateMetadataWithFiles
