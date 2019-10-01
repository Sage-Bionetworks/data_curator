
import synapseclient
from SynapseStorage import SynapseStorage

storage_fileview = "syn20446927"

### get session token from commandline
# import sys
# session_token = sys.argv[1]

syn = synapseclient.Synapse()
# syn.login(sessioToken = session_token)
# syn.login()

syn_store = SynapseStorage(storage_fileview, syn)

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

syn_get = syn.get

