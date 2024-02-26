#' @title Get Synapse user profile info
#' @details
#' For information on authenticating synapse REST API queries
#' https://docs.synapse.org/rest/#org.sagebionetworks.auth.controller.AuthenticationController
#' Authentication to Synapse services requires an access token passed in the
#' HTTP Authorization header, as per the HTTP bearer authorization standard. 
#' https://datatracker.ietf.org/doc/html/rfc6750#section-2.1
#' https://docs.synapse.org/rest/#org.sagebionetworks.repo.web.controller.UserProfileController
#' PAT is the synapse personal access token OR the browser cookie from a logged-in session.
#' 
#' @param url Synapse REST API url for userProfile
#' @param auth Synapse PAT or authorization token
#' 
#' @export
synapse_user_profile <- function(
  url="https://repo-prod.prod.sagebase.org/repo/v1/userProfile", auth=NULL) {
  req <- httr::GET(url, httr::add_headers(Authorization=paste0("Bearer ", auth)))
  httr::content(req)
}

#' @title Is a Synapse user certified?
#' @details
#' https://rest-docs.synapse.org/rest/GET/user/id/certifiedUserPassingRecord.html
#' Based on python client https://github.com/Sage-Bionetworks/synapsePythonClient/blob/3da21b9e2542c7da8cdab9925926737fe2162a54/synapseclient/client.py#L606
#' 
#' @param url URL to Synapse REST API for certification
#' @param endpoint The Synapse API endpoint
#' @param auth Synapse PAT or authorization token
#'
#' @export
synapse_is_certified <- function(url="https://repo-prod.prod.sagebase.org/repo/v1/user",
                                 endpoint="certifiedUserPassingRecord",
                                 auth=NULL) {
  
  # Get user profile and ownerId
  user_profile <- synapse_user_profile(auth=auth)
  if (!"ownerId" %in% names(user_profile)) return(FALSE)
  ownerid <- user_profile[["ownerId"]]
  url_req <- file.path(url, ownerid, endpoint)
  req <- httr::GET(url_req)
  httr::content(req)[["passed"]]
  
}


#content(GET(sprintf("https://repo-prod.prod.sagebase.org/repo/v1/user/%s/certifiedUserPassingRecord", "3438856")))
#https://repo-prod.prod.sagebase.org/repo/v1/user

#' @title GET Synapse Entity
#' @description Wrapper for https://rest-docs.synapse.org/rest/GET/entity/id.html
#' 
#' @param url URL of synapse REST API GET table entity endpoint
#' @param id ID of synapse table
#' @param auth Synapse PAT
#' 
#' @export
synapse_get <- function(url = "https://repo-prod.prod.sagebase.org/repo/v1/entity",
                        id, auth) {
  
  if (is.null(id)) stop("id cannot be NULL")
  req <- httr2::request(file.path(url, id))
  resp <- req |>
    httr2::req_retry(
      max_tries = 5,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 503, 403)
    ) |>
    httr2::req_throttle(1/2) |>
    httr2::req_headers(Authorization = sprintf("Bearer %s", auth)) |>
    httr2::req_perform()
  resp |> httr2::resp_body_json()
}


#' @title Check Access Permissions to a Synapse Entity
#' @description wrapper for https://rest-docs.synapse.org/rest/GET/entity/id/access.html
#' 
#' @param url URL to REST API endpoint
#' @param id Synapse ID
#' @param access Access Type to check
#' @param auth Synapse authentication token
#'
#' @export
synapse_access <- function(url = "https://repo-prod.prod.sagebase.org/repo/v1/entity",
                        id, access, auth) {
  
  if (is.null(id)) stop("id cannot be NULL")
  req_url <- file.path(url, id, "access")
  req <- httr::GET(req_url,
                   httr::add_headers(Authorization=paste0("Bearer ", auth)),
                   query = list(accessType=access))
  
  # Send error if unsuccessful query
  status <- httr::http_status(req)
  if (status$category != "Success") stop(status$message)
  
  cont <- httr::content(req)
  cont$result
  
}

#' @title Get children of a synapse entity
#' https://rest-docs.synapse.org/rest/POST/entity/children.html
#' @param url Synapse api endpoint
#' @param auth Synapse token
#' @param parentId Synapse ID of parent folder
#' @param nextPageToken Synapse next page token
#' @param includeTypes Types to return
#' @param sortBy Variable to sort by 
#' @param sortDirection sort direction 
#' @param includeTotalChildCount boolean include count of children 
#' @param includeSumFileSizes boolean include sum of file sizes 
synapse_entity_children <- function(url = "https://repo-prod.prod.sagebase.org/repo/v1/entity/children",
                                    auth, parentId=NULL, nextPageToken=NULL, includeTypes="project", sortBy="NAME",
                                    sortDirection="ASC", includeTotalChildCount=FALSE, includeSumFileSizes=FALSE) {
  
  output <- list()
  req <- httr::POST(url,
                    httr::add_headers(Authorization=paste0("Bearer ", auth)),
                    body = 
                      list(parentId=parentId,
                           nextPageToken=nextPageToken,
                           includeTypes=includeTypes,
                           sortBy=sortBy,
                           sortDirection=sortDirection,
                           includeTotalChildCount=includeTotalChildCount,
                           includeSumFileSizes=includeSumFileSizes),
                    encode="json")
  
  resp <- httr::content(req)
  output <- resp$page
  
  while (!is.null(resp$nextPageToken)) {
    req <- httr::POST(url,
                      httr::add_headers(Authorization=paste0("Bearer ", auth)),
                      body = 
                        list(parentId=parentId,
                             nextPageToken=resp$nextPageToken,
                             includeTypes=includeTypes,
                             sortBy=sortBy,
                             sortDirection=sortDirection,
                             includeTotalChildCount=includeTotalChildCount,
                             includeSumFileSizes=includeSumFileSizes),
                      encode="json")
    resp <- httr::content(req)
    output <- c(output, resp$page)
  }
  bind_rows(output)
  
}

#' @title Get projects a user has access to
#' 
#' @param url Synapse api endpoint
#' @param auth Synapse token
#' @param nextPageToken Synapse next page token
synapse_projects_user <- function(url = "https://repo-prod.prod.sagebase.org/repo/v1/projects/user", auth, nextPageToken=NULL) {
  principalId <- synapse_user_profile(auth = auth)[["ownerId"]]
  hreq <- httr::GET(url = file.path(url, principalId),
                    query = list(nextPageToken=nextPageToken))
  output <- list()
  resp <- httr::content(hreq)
  output <- resp$results
  while (!is.null(resp$nextPageToken)) {
    hreq <- httr::GET(url = file.path(url, principalId),
                      query = list(nextPageToken=resp$nextPageToken))
    resp <- httr::content(hreq)
    output <- c(output, resp$results)
  }
  dplyr::bind_rows(output)
}

#' @title Get projects within scope of Synapse project
#' 
#' @param url Synapse api endpoint
#' @param id Synapse ID
#' @param auth Synapse token
synapse_get_project_scope <- function(url = "https://repo-prod.prod.sagebase.org/repo/v1/entity/",
                                      id, auth) {
  if (is.null(id)) stop("id cannot be NULL")
  req_url <- file.path(url, id)
  req <- httr::GET(req_url,
                   httr::add_headers(Authorization=paste0("Bearer ", auth)))
  
  # Send error if unsuccessful query
  status <- httr::http_status(req)
  if (status$category != "Success") stop(status$message)
  
  cont <- httr::content(req)
  unlist(cont$scopeIds)
}

#' @param title Query a Synapse Table
#' https://rest-docs.synapse.org/rest/GET/entity/id/table/query/async/get/asyncToken.html
#' @param id Synapse table ID
#' @param auth Synapse token
#' @param query An sql query
#' @param partMask The part of the Synapse response to get. Defaults to everything.
synapse_table_query <- function(id, auth, query, partMask=0x7F) {
  url <- file.path("https://repo-prod.prod.sagebase.org/repo/v1/entity",id, "table/query/async/start")
  req <- httr::POST(url = url,
                    httr::add_headers(Authorization=paste0("Bearer ", auth)),
                    body = list(
                      query = list(sql=query),
                      partMask = partMask
                    ),
                    encode = "json"
  )
  httr::content(req)
}

#' @param title Get results of synapse_table_query
#' https://rest-docs.synapse.org/rest/GET/entity/id/table/query/async/get/asyncToken.html
#' @param id Synapse table ID
#' @param async_token Token from synapse_table_query
#' @param auth Synapse token
synapse_table_get <- function(id, async_token, auth) {
  url <- file.path("https://repo-prod.prod.sagebase.org/repo/v1/entity", id,"table/query/async/get", async_token)
  request <- httr2::request(url)
  response <- request |>
    httr2::req_retry(
      max_tries = 5,
      is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 503, 202, 403)
    ) |>
    httr2::req_throttle(1/2) |>
    httr2::req_headers(Authorization = sprintf("Bearer %s", auth)) |>
    httr2::req_perform()
  httr2::resp_body_json(response)
    
}

#' @title Get column names from a Synapse table
#' https://rest-docs.synapse.org/rest/GET/entity/id/table/query/async/get/asyncToken.html
#' Uses a table query to get the column names from a Synapse table
#' @param id Synapse ID of table
#' @param auth Synapse token
get_synapse_table_names <- function(id, auth) {
  query <- sprintf("select id from %s limit 1", id)
  request <- synapse_table_query(id, auth, query, partMask = 0x10)
  Sys.sleep(1)
  response <- synapse_table_get(id, request$token, auth)
  vapply(response$columnModels, function(x) x$name, character(1L))
}

#' @title Get storage projects within a Synapse table
#' https://rest-docs.synapse.org/rest/GET/entity/id/table/query/async/get/asyncToken.html
#' @param id Synapse ID of table
#' @param auth Synapse token
#' @param select_cols Columns to get from table
synapse_storage_projects <- function(id, auth, select_cols = c("id", "name", "parentId", "projectId", "type", "columnType")) {
  table_cols <- get_synapse_table_names(id, auth)
  select_cols <- intersect(select_cols, table_cols)
  select_cols_format <- paste(select_cols, collapse = ", ")
  query <- sprintf("select distinct %s from %s", select_cols_format, id)
  request <- synapse_table_query(id, auth, query, partMask = 0x1)
  response <- synapse_table_get(id, request$token, auth)
  
  setNames(
    tibble::as_tibble(
      t(
        vapply(
          response$queryResult$queryResults$rows, function(x) {
            unlist(x$values)
          },
          character(length(select_cols))))),
    select_cols)
}

#' @title Download a synapse file from its URL
#' https://rest-docs.synapse.org/rest/GET/file/id.html
#' @param dataFileHandleId The dataFileHandleId from an entity
#' @param id The synapse ID of the file to download
#' @param auth Synapse token
#' @param filepath Optional path to download data. If NULL, return a data frame.
synapse_download_file_handle <- function(dataFileHandleId, id, auth, filepath=NULL) {
  url <- sprintf("https://repo-prod.prod.sagebase.org/file/v1/file/%s", dataFileHandleId)
  request <- httr::GET(url = url,
                       httr::add_headers( Authorization=paste0("Bearer ", auth)),
                       query = list(
                         redirect = FALSE,
                         fileAssociateId = id,
                         fileAssociateType = "FileEntity"
                       )
  )
  download_url <- httr::content(request)
  destfile <- ifelse(is.null(filepath), tempfile(), filepath)
  download.file(download_url, destfile)
  if (is.null(filepath)) readr::read_csv(destfile, show_col_types = FALSE)
  
}

#' @title Download the storage manifest records from an asset view table
synapse_get_manifests_in_asset_view <- function(id, auth) {
  request <- synapse_table_query(
    id = id,
    auth = auth,
    query = paste("select * from",
                  id,
                  "where name like 'synapse|_storage|_manifest|_%' escape '|'"),
    partMask = 0x11)
  response <- synapse_table_get(
    id = id,
    async_token = request$token,
    auth = auth)
  # Format the query results by reshaping the results list and getting column
  # names. partMask 0x11 gets queryResults and column names
  setNames(
    tibble::as_tibble(
      t(
        vapply(
          response$queryResult$queryResults$rows, function(x) {
            null_ind <- which(sapply(x$values, is.null))
            x$values[null_ind] <- NA
            unlist(x$values)
          },
          character(length(response$columnModels))))),
    vapply(response$columnModels, function(x) x$name,character(1L)))
}
