#' Download an existing manifest
#' @param url URI of API endpoint
#' @param input_token Synapse PAT
#' @param asset_view ID of view listing all project data assets
#' @param dataset_id the parent ID of the manifest
#' @param as_json if True return the manifest in JSON format
#' @returns a csv of the manifest
#' @export
manifest_download <- function(url="http://localhost:3001/v1/manifest/download",
                              input_token, asset_view, dataset_id, as_json=TRUE){
  req <- httr::GET(url,
                   query = list(
                     asset_view = asset_view,
                     dataset_id = dataset_id,
                     as_json = as_json,
                     input_token = input_token
                   ))
  manifest <- httr::content(req, as = "text")
  jsonlite::fromJSON(manifest)
}

#' schematic rest api to generate manifest
#'
#' @param title Name of dataset 
#' @param data_type Type of dataset 
#' @param oauth true or false STRING passed to python
#' @param use_annotations true or false STRING passed to python 
#' @param dataset_id Synapse ID of existing manifest
#' 
#' @returns a URL to a google sheet
#' @export
manifest_generate <- function(url="http://localhost:3001/v1/manifest/generate",
                              schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld", #nolint
                              title, data_type, oauth="true",
                              use_annotations="false", dataset_id) {
  
  req <- httr::GET(url,
                   query = list(
                     schema_url=schema_url,
                     title=title,
                     data_type=data_type,
                     oauth=oauth,
                     use_annotations=use_annotations,
                     dataset_id=dataset_id
                   ))
  
  manifest_url <- httr::content(req)
  manifest_url
}

#' Populate a manifest sheet
#' 
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld 
#' @param data_type Type of dataset
#' @param title Title of csv
#' @param csv_file Filepath of csv to validate
#' @export
manifest_populate <- function(url="http://localhost:3001/v1/manifest/populate",
                              schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld", #notlint
                              data_type, title, csv_file) {
  
  req <- httr::POST(url,
                    query=list(
                      schema_url=schema_url,
                      data_type=data_type,
                      title=title),
                    body=list(csv_file=httr::upload_file(csv_file))
  )
  req
  
}


#' schematic rest api to validate metadata
#' 
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld 
#' @param data_type Type of dataset
#' @param csv_file Filepath of csv to validate
#' 
#' @returns An empty list() if sucessfully validated. Or a list of errors.
#' @export
manifest_validate <- function(url="http://localhost:3001/v1/model/validate",
                              schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld", #nolint
                              data_type, csv_file) {
  req <- httr::POST(url,
                     query=list(
                       schema_url=schema_url,
                       data_type=data_type),
                     body=list(csv_file=httr::upload_file(csv_file))
  )
  
  if (httr::http_status(req)$category == "Server error") return(list(list(), list()))
  annotation_status <- httr::content(req)
  annotation_status
}


#' schematic rest api to submit metadata
#' 
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld 
#' @param data_type Type of dataset
#' @param dataset_id Synapse ID of existing manifest
#' @param input_token Synapse login cookie, PAT, or API key.
#' @param csv_file Filepath of csv to validate
#' 
#' @returns TRUE if successful upload or validate errors if not.
#' @export
model_submit <- function(url="http://localhost:3001/v1/model/submit",
                         schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld", #notlint
                         data_type, dataset_id, input_token, csv_file) {
  req <- httr::POST(url,
                    #add_headers(Authorization=paste0("Bearer ", pat)),
                    query=list(
                      schema_url=schema_url,
                      data_type=data_type,
                      dataset_id=dataset_id,
                      input_token=input_token),
                    body=list(csv_file=httr::upload_file(csv_file))
  )
  
  manifest_id <- httr::content(req)
  manifest_id
}

#' Given a source model component (see https://w3id.org/biolink/vocab/category for definnition of component), return all components required by it.
#' 
#' @param schema_url Data Model URL
#' @param source_component an attribute label indicating the source component. (i.e. Patient, Biospecimen, ScRNA-seqLevel1, ScRNA-seqLevel2)
#' @param as_graph if False return component requirements as a list; if True return component requirements as a dependency graph (i.e. a DAG)
#' 
#' @returns A list of required components associated with the source component.
#' @export
model_component_requirements <- function(url="http://localhost:3001/v1/model/component-requirements",
                                         schema_url, source_component,
                                         as_graph = FALSE) {
  
  req <- httr::GET(url,
                   query =  list(
                     schema_url = schema_url,
                     source_component = source_component,
                     as_graph = as_graph
                   ))
  
  if (httr::http_error(req)) stop(httr::http_status(req)$reason)
  cont <- httr::content(req)
  
  if (inherits(cont, "xml_document")){
    err_msg <- xml2::xml_text(xml2::xml_child(cont, "head/title"))
    stop(sprintf("%s", err_msg))
  }
  
  cont
  
}
  

#' Gets all datasets in folder under a given storage project that the current user has access to.
#' 
#' @param url URL to schematic API endpoint
#' @param syn_master_file_view synapse ID of master file view.
#' @param syn_master_file_name Synapse storage manifest file name.
#' @param project_id synapse ID of a storage project.
#' @param input_token synapse PAT
#'
#'@export
storage_project_datasets <- function(url="http://localhost:3001/v1/storage/project/datasets",
                                     asset_view,
                                     project_id,
                                     input_token) {
  
  req <- httr::GET(url,
                    #add_headers(Authorization=paste0("Bearer ", pat)),
                    query=list(
                      asset_view=asset_view,
                      project_id=project_id,
                      input_token=input_token)
  )
  
  httr::content(req)
}

#' Get all storage projects the current user has access to
#' 
#' @param url URL to schematic API endpoint
#' @param syn_master_file_view synapse ID of master file view.
#' @param syn_master_file_name Synapse storage manifest file name.
#' @param input_token synapse PAT
#'
#' @export
storage_projects <- function(url="http://localhost:3001/v1/storage/projects",
                             asset_view,
                             input_token) {
  
  req <- httr::GET(url,
                   query = list(
                     asset_view=asset_view,
                     input_token=input_token
                   ))
  
  httr::content(req)
}

#' /storage/dataset/files
#'
#' @param url URL to schematic API endpoint
#' @param syn_master_file_view synapse ID of master file view.
#' @param syn_master_file_name Synapse storage manifest file name.
#' @param dataset_id synapse ID of a storage dataset.
#' @param file_names a list of files with particular names (i.e. Sample_A.txt). If you leave it empty, it will return all dataset files under the dataset ID.
#' @param full_path Boolean. If True return the full path as part of this filename; otherwise return just base filename
#' @param input_token synapse PAT
#'
#' @export
storage_dataset_files <- function(url="http://localhost:3001/v1/storage/dataset/files",
                                  asset_view,
                                  dataset_id, file_names=list(),
                                  full_path=FALSE, input_token) {
  
  req <- httr::GET(url,
                   #add_headers(Authorization=paste0("Bearer ", pat)),
                   query=list(
                     asset_view=asset_view,
                     dataset_id=dataset_id,
                     file_names=file_names,
                     full_path=full_path,
                     input_token=input_token))
  httr::content(req)
                   
}

#' /storage/asset/table
#' 
#' @param url URL to schematic API endpoint
#' @param input_token synapse PAT
#' @param asset_view Synapse ID of asset view
#' @export
get_asset_view_table <- function(url="http://localhost:3001/v1/storage/assets/tables",
                                 input_token, asset_view, as_json=TRUE) {
  
  req <- httr::GET(url,
                   query=list(
                     asset_view=asset_view,
                     input_token=input_token,
                     as_json=as_json))
  
  if (httr::http_status(req)$category == "Success") {
    if (isTRUE(as_json)) {
      return(dplyr::bind_rows(httr::content(req)))
    } else {
    csv <- readr::read_csv(httr::content(req))
    return(csv)
    }
  } else stop("File could not be downloaded from Synapse.")
  
}

