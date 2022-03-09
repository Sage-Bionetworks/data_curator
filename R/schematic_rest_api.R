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
