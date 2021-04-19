###!!!! use synStore_Session for server instead
import synapseclient
from SynapseStorage import SynapseStorage

syn_get = syn.get

storage_fileview = "syn16858331"

syn = synapseclient.Synapse()
syn.login()

syn_store = SynapseStorage(storage_fileview, syn)

### "Testing retrieval of project list from Synapse
get_projects_list = syn_store.getStorageProjects()

###print("Testing retrieval of folder list within a given storage project from Synapse")
#"syn19557917"
get_folder_list = syn_store.getStorageDatasetsInProject

### print("Testing retrieval of file list within a given storage dataset from Synapse")
# "syn19557948"
get_file_list = syn_store.getFilesInStorageDataset

### print("Testing association of antities with annotation from manifest")
# "./synapse_storage_manifest.csv", "syn20685746"
get_manifest_syn_id = syn_store.associateMetadataWithFiles



