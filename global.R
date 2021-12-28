suppressPackageStartupMessages({
  # env
  library(httr)
  library(yaml)
  library(reticulate)
  library(jsonlite)
  # shiny function
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(sass)
  library(shinypop) # dreamRs/shinypop
  library(waiter)
  # tidyverse
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  # dashboard
  library(networkD3)
  library(data.tree)
  library(r2d3)
})

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

oauth_client <- yaml.load_file("config.yaml")

client_id <- toString(oauth_client$CLIENT_ID)
client_secret <- toString(oauth_client$CLIENT_SECRET)

if (interactive()) {
  # for local development
  # change port number associated with your client, here
  options(shiny.port = 8100)
}

app_url <- toString(oauth_client$APP_URL)
conda_name <- toString(oauth_client$CONDA_NAME)

if (is.null(client_id)) stop("config.yaml is missing CLIENT_ID")
if (is.null(client_secret)) stop("config.yaml is missing CLIENT_SECRET")
if (is.null(app_url)) stop("config.yaml is missing APP_URL")
if (is.null(conda_name)) stop("config.yaml is missing CONDA_ENV")

app <- oauth_app("shinysynapse",
  key = client_id,
  secret = client_secret,
  redirect_uri = app_url
)

# These are the user info details ('claims') requested from Synapse:
claims <- list(
  family_name = NULL,
  given_name = NULL,
  email = NULL,
  email_verified = NULL,
  userid = NULL,
  orcid = NULL,
  is_certified = NULL,
  is_validated = NULL,
  validated_given_name = NULL,
  validated_family_name = NULL,
  validated_location = NULL,
  validated_email = NULL,
  validated_company = NULL,
  validated_at = NULL,
  validated_orcid = NULL,
  company = NULL
)

claimsParam <- toJSON(list(id_token = claims, userinfo = claims))
api <- oauth_endpoint(
  authorize = paste0("https://signin.synapse.org?claims=", claimsParam),
  access = "https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
)

# The 'openid' scope is required by the protocol for retrieving user information.
scope <- "openid view download modify"

# Activate conda env
# Don't necessarily have to set `RETICULATE_PYTHON` env variable
reticulate::use_condaenv(conda_name, required = TRUE)

# import synapse client
syn <- import("synapseclient")$Synapse()
# import schematic modules
source_python("functions/metadataModel.py")
synapse_driver <- import("schematic.store.synapse")$SynapseStorage
# Import functions/modules
source_files <- list.files(c("functions", "modules"), pattern = "*\\.R$", recursive = TRUE, full.names = TRUE)
sapply(source_files, FUN = source)

# Global variables
dropdown_types <- c("project", "folder", "template")
options(sass.cache = FALSE)
