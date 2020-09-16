import uuid
import json
import os
import time

import synapseclient
from synapseclient import File

from schematic.synapse.store import SynapseStorage

syn_store = SynapseStorage

### "Testing retrieval of project list from Synapse
# get_projects_list = syn_store.getStorageProjects

###print("Testing retrieval of folder list within a given storage project from Synapse")
# get_folder_list = syn_store.getStorageDatasetsInProject

### print("Testing retrieval of file list within a given storage dataseyt from Synapse")
# get_file_list = syn_store.getFilesInStorageDataset

### print("Testing association of antities with annotation from manifest")
# get_associated_manifestId = syn_store.associateMetadataWithFiles

### getting all manifests associated with a project accessible by user
### returns a list, empty if manifest isn't there
# [('syn20687304', 'HCA immune cells census'),
#       ('syn20703799', 'Ischaemic Sensitivity of Human Tissue'),
#       []),
#    (  ('syn20687304', 'HCA immune cells census'),
#       ('syn21682582', 'Test'),
#       ('syn21682585', 'synapse_storage_manifest.csv'))]
# get_all_manifests = syn_store.getAllManifests

### updating fileset in a manifest associated with a dataset
#manifestId = syn_store.update_dataset_manifest_files(dataset_id)
# returns '' if no manifest exists
# depends on fileview so it may take a few min for new files to show
# get_update_manifestId = syn_store.updateDatasetManifestFiles
