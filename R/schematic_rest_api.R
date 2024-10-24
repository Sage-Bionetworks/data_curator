#' @description Check if a httr request succeeded.
#' @param x An httr response object
check_success <- function(x){
  if (!inherits(x, "response")) stop("Input must be an httr reponse object")
  status <- httr::http_status(x)
  if (tolower(status$category) == "success") {
    return()
  } else {
    # Return content text for Data Type errors
    if (grepl("LookupError: The DataType", httr::content(x, "text"))) {
      stop(httr::content(x, "text"))
    }
    
    stop(sprintf("Response from server: %s", status$reason))
  }
}

#' Download a manifest
#' @description Download an existing manifest
#' @param url URI of API endpoint
#' @param access_token Synapse PAT
#' @param asset_view ID of view listing all project data assets
#' @param manifest_id the parent ID of the manifest
#' @param as_json if True return the manifest in JSON format
#' @param new_manifest_name Name of new manifest (Default = NULL)
#' @returns a csv of the manifest
#' @export

manifest_download <- function(
    url = "http://localhost:3001/v1/manifest/download", 
    access_token,
    manifest_id, 
    as_json = TRUE, 
    new_manifest_name = NULL) {

  req <- httr2::request(url) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 503, 403)
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
#' @param url schematic endpoint url
#' @param schema_url url of jsonld schema
#' @param title Name of dataset 
#' @param data_type Type of dataset 
#' @param use_annotations true or false 
#' @param dataset_id Synapse ID of existing manifest
#' @param asset_view Synapse ID of asset view
#' @param output_format Desired format of generated manifest (excel, google_sheet)
#' @param access_token Synapse PAT
#' @param strict_validation If using Google Sheets, can set the strictness of Google Sheets regex match validation. True (default) will block users from entering incorrect values, False will throw a warning to users.
#' @param data_model_labels Which labels to use (class_label, display_label)
#' 
#' @returns a URL to a google sheet
#' @export
manifest_generate <- function(url = "http://localhost:3001/v1/manifest/generate",
                              schema_url,
                              title,
                              data_type,
                              use_annotations = "false",
                              dataset_id = NULL,
                              asset_view,
                              output_format = c("google_sheet", "excel"),
                              access_token = NULL,
                              strict_validation = FALSE,
                              data_model_labels = c("class_label", "display_label")) {
  
  # do not accept dataframe as input, it's not supported at this time
  match.arg(output_format)
  match.arg(data_model_labels)
  
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
                     strict_validation = strict_validation,
                     data_model_labels = data_model_labels
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
#' @param return_excel Return Excel? TRUE/FALSE
#' @param data_model_labels Which labels to use (class_label, display_label)
#' @param csv_file Filepath of csv to validate
#' @export

manifest_populate <- function(url="http://localhost:3001/v1/manifest/populate",
                              schema_url,
                              data_type,
                              title,
                              return_excel=FALSE,
                              data_model_labels = "class_label",
                              csv_file) {
  
  req <- httr::POST(url,
                    query=list(
                      schema_url=schema_url,
                      data_type=data_type,
                      title=title,
                      return_excel=return_excel,
                      data_model_labels=data_model_labels),
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
#' @param restrict_rules If True, validation suite will only run with in-house validation rule. If False, the Great Expectations suite will be utilized and all rules will be available.
#' @param project_scope List, a subset of the projects contained within the asset view that are relevant for the current operation. Speeds up some operations that interact with Synapse. Relevant for validating manifests involving cross-manifest validation, but optional.
#' @param access_token Synapse PAT
#' @param asset_view SynID of asset view
#' @param json_str JSON string to validate
#' @param data_model_labels Which labels to use (class_label, display_label)
#' @returns An empty list() if sucessfully validated. Or a list of errors.
#' @export

manifest_validate <- function(url="http://localhost:3001/v1/model/validate",
                              schema_url,
                              data_type,
                              file_name,
                              restrict_rules=FALSE,
                              project_scope = NULL,
                              access_token,
                              asset_view = NULL,
                              json_str = NULL,
                              data_model_labels = "class_label",
                              dataset_scope = NULL) {
  
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
        is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 503, 504, 403)
      ) |>
      httr2::req_throttle(1/2) |>
      httr2::req_error(is_error = \(reqs) FALSE)
    resp <- reqs |>
      httr2::req_headers(Authorization = sprintf("Bearer %s", access_token)) |>
      httr2::req_url_query(
        schema_url=schema_url,
        data_type=data_type,
        restrict_rules=restrict_rules,
        project_scope = project_scope,
        data_model_labels = data_model_labels,
        asset_view = asset_view,
        dataset_scope = dataset_scope
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
        data_model_labels = data_model_labels,
        json_str = json_str,
        dataset_scope = dataset_scope
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
  if (httr2::resp_is_error(resp)) {
    list(list(
      "errors" = list(
        Row = NA, Column = NA, Value = NA,
        Error = httr2::resp_body_string(resp)
      )
    ))
  } else httr2::resp_body_json(resp)
}


#' schematic rest api to submit metadata
#' 
#' @param url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld 
#' @param data_type Type of dataset
#' @param dataset_id Synapse ID of existing manifest
#' @param restrict_rules Default = FALSE
#' @param access_token Synapse login cookie, PAT, or API key
#' @param json_str Json string to submit
#' @param asset_view Synapse fileview
#' @param manifest_record_type Default = "table_and_file"
#' @param file_name Name of file
#' @param table_manipulation Default = "replace"
#' @param hide_blanks Default = FALSE
#' @param table_column_names Default = "class_and_label"
#' @param annotation_keys Default = "class_and_label"
#' @param data_model_labels Default = "class_and_label"
#' @param upload_file_annotations Default = TRUE
#' 
#' @returns TRUE if successful upload or validate errors if not.
#' @export
model_submit <- function(url="http://localhost:3001/v1/model/submit",
                         schema_url,
                         data_type,
                         dataset_id,
                         restrict_rules=FALSE,
                         access_token,
                         json_str=NULL,
                         asset_view,
                         manifest_record_type="table_and_file",
                         file_name,
                         table_manipulation="replace",
                         hide_blanks=FALSE,
                         table_column_names="class_label",
                         annotation_keys="class_label",
                         data_model_labels="class_label",
                         file_annotations_upload=TRUE) {
  req <- httr::POST(url,
                    httr::add_headers(Authorization = sprintf("Bearer %s", access_token)),
                    query=list(
                      schema_url=schema_url,
                      data_type=data_type,
                      dataset_id=dataset_id,
                      restrict_rules=restrict_rules,
                      json_str=json_str,
                      asset_view=asset_view,
                      manifest_record_type=manifest_record_type,
                      table_manipulation=table_manipulation,
                      table_column_names=table_column_names,
                      annotation_keys=annotation_keys,
                      data_model_labels=data_model_labels,
                      hide_blanks=hide_blanks,
                      file_annotations_upload=file_annotations_upload),
                    body=list(file_name=httr::upload_file(file_name))
                    #body=list(file_name=file_name)
  )
  
  check_success(req)
  manifest_id <- httr::content(req)
  manifest_id
}

#' Given a source model component (see https://w3id.org/biolink/vocab/category for definnition of component), return all components required by it.
#' 
#' @param url URL to schematic API endpoint
#' @param schema_url Data Model URL
#' @param source_component an attribute label indicating the source component. (i.e. Patient, Biospecimen, ScRNA-seqLevel1, ScRNA-seqLevel2)
#' @param as_graph if False return component requirements as a list; if True return component requirements as a dependency graph (i.e. a DAG)
#' @param data_model_labels Which labels to use (class_label, display_label)
#' 
#' @returns A list of required components associated with the source component.
#' @export
model_component_requirements <- function(url="http://localhost:3001/v1/model/component-requirements",
                                         schema_url,
                                         source_component,
                                         as_graph = FALSE,
                                         data_model_labels = "class_label") {
  
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
    data_model_labels = data_model_labels,
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
#' @param asset_view synapse ID of master file view.
#' @param project_id Synapse storage manifest file name.
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
#' @param asset_view synapse ID of master file view.
#' @param access_token Synapse storage manifest file name.
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
#' @param asset_view synapse ID of master file view.
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

#' /storage/asset/table endpoint
#' 
#' @param url URL to schematic API endpoint
#' @param access_token synapse PAT
#' @param asset_view Synapse ID of asset view
#' @param return_type Output format (json, csv)
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
    return(list2DF(jsonlite::fromJSON(httr::content(req))))
  } else {
  csv <- readr::read_csv(httr::content(req), show_col_types = FALSE)
  return(csv)
  }
  
}

#' graph by edge type endpoint
#' @param url URL of schematic API endpoint
#' @param schema_url URL of data model
#' @param relationship Argument to schematic graph_by_edge_type
#' @param data_model_labels Which labels to use (class_label, display_label)
#' @export
#' @importFrom httr GET content
graph_by_edge_type <- function(url = "https://schematic-dev.api.sagebionetworks.org/v1/schemas/get/graph_by_edge_type",
                               schema_url,
                               relationship = "requiresDependency",
                               data_model_labels = "class_label") {
  req <- httr::GET(url = url,
                   query = list(
                     schema_url = schema_url,
                     relationship = relationship,
                     data_model_labels = data_model_labels
                   ))
  httr::content(req)
}
