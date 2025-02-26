% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schematic_rest_api.R
\name{storage_project_datasets}
\alias{storage_project_datasets}
\title{Gets all datasets in folder under a given storage project that the current user has access to.}
\usage{
storage_project_datasets(
  url = "http://localhost:3001/v1/storage/project/datasets",
  asset_view,
  project_id,
  access_token
)
}
\arguments{
\item{url}{URL to schematic API endpoint}

\item{asset_view}{synapse ID of master file view.}

\item{project_id}{Synapse storage manifest file name.}

\item{access_token}{synapse PAT}
}
\description{
Gets all datasets in folder under a given storage project that the current user has access to.
}
