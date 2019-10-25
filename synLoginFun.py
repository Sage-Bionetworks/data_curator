import synapseclient

syn = synapseclient.Synapse()
syn_login = syn.login
syn_getUserProfile = syn.getUserProfile
syn_tableQuery = syn.tableQuery

### function to get previously uploaded manifest
### returns statement if no manifest uploaded instead of path
def get_storage_manifest_path (token, folderID):
    syn = synapseclient.Synapse()
    syn.login(sessionToken = token)
    entity = syn.tableQuery(  ("select id, name from syn20446927 where parentId = '"+ folderID + "' " ) )
    df = entity.asDataFrame()
    if ("synapse_storage_manifest.csv" in df.loc[:,'name'].values ) == True:
        row = df.loc[df['name'] == "synapse_storage_manifest.csv"]
        manifestID = row.loc[:,'id'].values
        fh = syn.get(manifestID.item())
        path_to_file = fh.path
        return(path_to_file)
    else:
        return None
