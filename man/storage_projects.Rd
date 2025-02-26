% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schematic_rest_api.R
\name{storage_projects}
\alias{storage_projects}
\title{Get all storage projects the current user has access to}
\usage{
storage_projects(
  url = "http://localhost:3001/v1/storage/projects",
  asset_view,
  access_token
)
}
\arguments{
\item{url}{URL to schematic API endpoint}

\item{asset_view}{synapse ID of master file view.}

\item{access_token}{Synapse storage manifest file name.}
}
\description{
Get all storage projects the current user has access to
}
