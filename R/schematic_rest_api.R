#' @description Check if a httr request succeeded.
#' @param x An httr response object
check_success <- function(x){
  if (!inherits(x, "response")) stop("Input must be an httr reponse object")
  status <- httr::http_status(x)
  if (tolower(status$category) == "success") {
    return()
  } else {
    stop(sprintf("Response from server: %s", status$reason))
  }
}

#' @description Download an existing manifest
#' @param url URI of API endpoint
#' @param access_token Synapse PAT
#' @param asset_view ID of view listing all project data assets
#' @param manifest_id the parent ID of the manifest
#' @param as_json if True return the manifest in JSON format
#' @returns a csv of the manifest
#' @export
manifest_download <- function(url = "http://localhost:3001/v1/manifest/download", access_token, manifest_id, as_json=TRUE, new_manifest_name=NULL) {

  req <- httr2::request(url) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 503)
    ) |>
    httr2::req_error(is_error = \(r) FALSE)
  resp <- req |>
    httr2::req_headers(Authorization = sprintf("Bearer %s", access_token)) |>
    httr2::req_url_query(
      manifest_id = manifest_id,
      as_json = as_json,
      new_manifest_name = new_manifest_name
    ) |>
    httr2::req_perform()
  resp |> httr2::resp_body_string() |>
    (function(d) gsub('NaN', '"NA"', x = d))() |>
    jsonlite::fromJSON()
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
                              title, data_type,
                              use_annotations="false", dataset_id=NULL,
                              asset_view, output_format, access_token = NULL,
                              strict_validation = FALSE) {
  
  req <- httr::GET(url,
                   httr::add_headers(Authorization = sprintf("Bearer %s", access_token)),
                   query = list(
                     schema_url=schema_url,
                     title=title,
                     data_type=data_type,
                     use_annotations=use_annotations,
                     dataset_id=dataset_id,
                     asset_view=asset_view,
                     output_format=output_format,
                     strict_validation = strict_validation
                   ))
  
  check_success(req)
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
                              data_type, title, return_excel=FALSE, csv_file) {
  
  req <- httr::POST(url,
                    query=list(
                      schema_url=schema_url,
                      data_type=data_type,
                      title=title,
                      return_excel=return_excel),
                    body=list(csv_file=httr::upload_file(csv_file, type = "text/csv"))
  )
  check_success(req)
  req
  
}


#' schematic rest api to validate metadata
#' 
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld 
#' @param data_type Type of dataset
#' @param file_name Filepath of csv to validate
#' 
#' @returns An empty list() if sucessfully validated. Or a list of errors.
#' @export
manifest_validate <- function(url="http://localhost:3001/v1/model/validate",
                              schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld", #nolint
                              data_type, file_name = NULL, restrict_rules=FALSE, project_scope = NULL,
                              access_token, asset_view = NULL, json_str = NULL) {
  flattenbody <- function(x) {
    # A form/query can only have one value per name, so take
    # any values that contain vectors length >1 and
    # split them up
    # list(x=1:2, y="a") becomes list(x=1, x=2, y="a")
    if (all(lengths(x)<=1)) return(x);
    do.call("c", mapply(function(name, val) {
      if (length(val)==1 || any(c("form_file", "form_data") %in% class(val))) {
        x <- list(val)
        names(x) <- name
        x
      } else {
        x <- as.list(val)
        names(x) <- rep(name, length(val))
        x
      }
    }, names(x), x, USE.NAMES = FALSE, SIMPLIFY = FALSE))
  }
  
  if (all(is.null(json_str), is.null(file_name))) {
    stop("Must provide either a file to upload or a json")
  }
  
  if (is.null(json_str)) {
    reqs <- httr2::request(url) |>
      httr2::req_retry(
        max_tries = 3,
        is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 503, 504)
      ) |>
      httr2::req_error(is_error = \(reqs) FALSE)
    resp <- reqs |>
      httr2::req_headers(Authorization = sprintf("Bearer %s", access_token)) |>
      httr2::req_url_query(
        schema_url=schema_url,
        data_type=data_type,
        restrict_rules=restrict_rules,
        project_scope = project_scope,
        asset_view = asset_view
      ) |>
      httr2::req_body_multipart(file_name=curl::form_file(file_name)) |>
      httr2::req_perform()
  } else {
    req <- httr2::request(url) |>
      httr2::req_throttle(1)
    resp <- req |>
      httr2::req_headers(Authorization = sprintf("Bearer %s", access_token)) |>
      httr2::req_url_query(
        schema_url=schema_url,
        data_type=data_type,
        restrict_rules=restrict_rules,
        project_scope = project_scope,
        asset_view = asset_view,
        json_str = json_str
      ) |>
      #httr2::req_retry(
      #  max_tries = 3,
      #  is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 503, 504)
      #) |>
      #httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
  }
  
  # Format server error in a way validationResult can handle
  # if (httr2::resp_is_error(resp)) {
  #   return(
  #     list(
  #       list(
  #         "errors" = list(
  #           Row = NA, Column = NA, Value = NA,
  #           Error = sprintf("Cannot validate manifest: %s",
  #                           httr2::resp_status_desc(resp)
  #           )
  #         )
  #       )
  #     )
  #   )
  # }
  httr2::resp_body_json(resp)
}


#' schematic rest api to submit metadata
#' 
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld 
#' @param data_type Type of dataset
#' @param dataset_id Synapse ID of existing manifest
#' @param access_token Synapse login cookie, PAT, or API key.
#' @param csv_file Filepath of csv to validate
#' 
#' @returns TRUE if successful upload or validate errors if not.
#' @export
model_submit <- function(url="http://localhost:3001/v1/model/submit",
                         schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld", #notlint
                         data_type, dataset_id, restrict_rules=FALSE, access_token, json_str=NULL, asset_view,
                         use_schema_label=TRUE, manifest_record_type="table_and_file", file_name,
                         table_manipulation="replace", hide_blanks=FALSE) {
  req <- httr::POST(url,
                    httr::add_headers(Authorization = sprintf("Bearer %s", access_token)),
                    query=list(
                      schema_url=schema_url,
                      data_type=data_type,
                      dataset_id=dataset_id,
                      restrict_rules=restrict_rules,
                      json_str=json_str,
                      asset_view=asset_view,
                      use_schema_label=use_schema_label,
                      manifest_record_type=manifest_record_type,
                      table_manipulation=table_manipulation,
                      hide_blanks=hide_blanks),
                    body=list(file_name=httr::upload_file(file_name))
                    #body=list(file_name=file_name)
  )
  
  check_success(req)
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
  
  reqs <- httr2::request(url) |>
    httr2::req_retry(
      max_tries = 5,
      is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 503)
    ) |>
    httr2::req_error(is_error = \(r) FALSE)
  resp <- reqs |>
    httr2::req_url_query(
    schema_url = schema_url,
    source_component = source_component,
    as_graph = as_graph
  ) |>
    #httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  if (httr2::resp_is_error(resp)) {
    warning(sprintf("model/component-requirement failed for %s. returning empty list. %s", 
                    source_component, httr2::resp_body_json(resp)$title))
    return(list())
  }
  resp |>
    httr2::resp_body_json()
  
}
  

#' Gets all datasets in folder under a given storage project that the current user has access to.
#' 
#' @param url URL to schematic API endpoint
#' @param syn_master_file_view synapse ID of master file view.
#' @param syn_master_file_name Synapse storage manifest file name.
#' @param project_id synapse ID of a storage project.
#' @param access_token synapse PAT
#'
#'@export
storage_project_datasets <- function(url="http://localhost:3001/v1/storage/project/datasets",
                                     asset_view,
                                     project_id,
                                     access_token) {
  
  req <- httr::GET(url,
                   httr::add_headers(Authorization = sprintf("Bearer %s", access_token)),
                    query=list(
                      asset_view=asset_view,
                      project_id=project_id)
  )

  check_success(req)
  httr::content(req)
}

#' Get all storage projects the current user has access to
#' 
#' @param url URL to schematic API endpoint
#' @param syn_master_file_view synapse ID of master file view.
#' @param syn_master_file_name Synapse storage manifest file name.
#' @param access_token synapse PAT
#'
#' @export
storage_projects <- function(url="http://localhost:3001/v1/storage/projects",
                             asset_view,
                             access_token) {
  
  req <- httr::GET(url,
                   httr::add_headers(Authorization = sprintf("Bearer %s", access_token)),
                   query = list(
                     asset_view=asset_view
                   ))
  
  check_success(req)
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
#' @param access_token synapse PAT
#'
#' @export
storage_dataset_files <- function(url="http://localhost:3001/v1/storage/dataset/files",
                                  asset_view,
                                  dataset_id, file_names=list(),
                                  full_path=FALSE, access_token) {
  
  req <- httr::GET(url,
                   httr::add_headers(Authorization = sprintf("Bearer %s", access_token)),
                   query=list(
                     asset_view=asset_view,
                     dataset_id=dataset_id,
                     file_names=file_names,
                     full_path=full_path))
  check_success(req)
  httr::content(req)
                   
}

#' /storage/asset/table
#' 
#' @param url URL to schematic API endpoint
#' @param access_token synapse PAT
#' @param asset_view Synapse ID of asset view
#' @export
get_asset_view_table <- function(url="http://localhost:3001/v1/storage/assets/tables",
                                 access_token, asset_view, return_type="json") {
  
  req <- httr::GET(url,
                   httr::add_headers(Authorization = sprintf("Bearer %s", access_token)),
                   query=list(
                     asset_view=asset_view,
                     return_type=return_type))
  
  check_success(req)
  if (return_type=="json") {
    return(list2DF(fromJSON(httr::content(req))))
  } else {
  csv <- readr::read_csv(httr::content(req), show_col_types = FALSE)
  return(csv)
  }
  
}

#' @param url URL of schematic API endpoint
#' @param schema_url URL of data model
#' @param relationship Argument to schematic graph_by_edge_type
#' @export
#' @importFrom httr GET content
graph_by_edge_type <- function(url = "https://schematic-dev.api.sagebionetworks.org/v1/schemas/get/graph_by_edge_type",
                               schema_url, relationship = "requiresDependency") {
  req <- httr::GET(url = url,
                   query = list(
                     schema_url = schema_url,
                     relationship = relationship
                   ))
  httr::content(req)
}
