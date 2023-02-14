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
synapse_get <- function(url = "https://repo-prod.prod.sagebase.org/repo/v1/entity/",
                        id, auth) {
  
  if (is.null(id)) stop("id cannot be NULL")
  req_url <- file.path(url, id)
  req <- httr::GET(req_url,
             httr::add_headers(Authorization=paste0("Bearer ", auth)))
  
  # Send error if unsuccessful query
  status <- httr::http_status(req)
  if (status$category != "Success") stop(status$message)
  
  cont <- httr::content(req)
  dplyr::bind_rows(cont)
  
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
