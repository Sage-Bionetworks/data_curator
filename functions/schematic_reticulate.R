setup_synapse_driver <- function(){
  # old way
  # library(reticulate)
  # use_virtualenv(file.path(getwd(), ".venv"), required = TRUE)
  # syn <- import("synapseclient")$Synapse()
  # source_python("functions/metadataModel.py") # schemaGenerator object
  # synapse_driver <- import("schematic.store.synapse")$SynapseStorage
  
  # new way
  reticulate::use_virtualenv(file.path(getwd(), ".venv"), required = TRUE)
  syn <- reticulate::import("synapseclient")$Synapse()
  
  MetadataModel <<- reticulate::import("schematic.models.metadata")$MetadataModel
  CONFIG <<- reticulate::import("schematic")$CONFIG
  SchemaGenerator <<- reticulate::import("schematic.schemas.generator")$SchemaGenerator
  
  config = CONFIG$load_config("schematic_config.yml")
  
  inputMModelLocation = config$model$input$location
  inputMModelLocationType = config$model$input$file_type
  
  manifest_title = config$manifest$title
  manifest_data_type = config$manifest$data_type[1]
  
  metadata_model <<- MetadataModel(inputMModelLocation, inputMModelLocationType)
  
  # create schema generator object for associateMetadataWithFiles
  schema_generator <<- SchemaGenerator(inputMModelLocation)
  
  synapse_driver <<- reticulate::import("schematic.store.synapse")$SynapseStorage
  
}

storage_projects_py <- function(synapse_driver, access_token) {
  
  tryCatch(
    {
      # get syn storage
      syn_store <<- synapse_driver(access_token = access_token)
      # get user's common projects
      syn_store$getStorageProjects()
    },
    error = function(e) {
      message(e$message)
      return(NULL)
    }
  )
}

storage_projects_datasets_py <- function(synapse_driver, project_id) {
  syn_store$getStorageDatasetsInProject(project_id) #%>% list2Vector()
}

storage_dataset_files_py <- function(project_id) {
  file_list <- syn_store$getFilesInStorageDataset(project_id)
}

manifest_generate_py <- function(title, rootNode, filenames=NULL, datasetId){
  metadata_model$getModelManifest(
    title = title,
    rootNode = rootNode,
    filenames = filenames,
    datasetId = datasetId
  )
}

manifest_validate_py <- function(manifestPath, rootNode, restrict_rules=TRUE, project_scope){
  tryCatch(
    metadata_model$validateModelManifest(
      manifestPath = manifestPath,
      rootNode = rootNode,
      restrict_rules = TRUE, # set true to disable great expectation
      project_scope = project_scope
    ),
    error = function(e) {
      message("'validateModelManifest' failed:\n", e$message)
      return(NULL)
    }
  )
}

manifest_populate_py <- function(title, manifestPath, rootNode) {
  metadata_model$populateModelManifest(
    title = title,
    manifestPath = manifestPath,
    rootNode = rootNode
  )
}

model_submit_py <- function(SchemaGenerator, metadataManifestPath, datasetId, manifest_record_type="table", restrict_manifest=FALSE) {
  syn_store$associateMetadataWithFiles(
    schemaGenerator = SchemaGenerator,
    metadataManifestPath = metadataManifestPath,
    datasetId = datasetId,
    manifest_record_type = manifest_record_type,
    restrict_manifest = restrict_manifest
  )
}

synase_user_profile_py <- function() syn$getUserProfile()$userName
