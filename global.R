library(yaml)

oauth_client <- yaml.load_file("config.yaml")

client_id <- toString(oauth_client$CLIENT_ID)
client_secret <- toString(oauth_client$CLIENT_SECRET)
conda_name <- toString(oauth_client$CONDA_NAME)

if (is.null(client_id) || nchar(client_id)==0) stop("config.yaml is missing CLIENT_ID")
if (is.null(client_secret) || nchar(client_secret)==0) stop("config.yaml is missing CLIENT_SECRET")
if (is.null(conda_name) || nchar(conda_name)==0) stop("config.yaml is missing CONDA_ENV_NAME")

# unzip <conda_name>.zip
utils::unzip(paste0(conda_name, ".zip"))
system(sprintf("chmod -R +x %s", conda_name))
message(sprintf("unzipped %s.zip to %s", conda_name, getwd()))
message(paste(dir(), collapse="\n"))
# Activate conda env
# Don't necessarily have to set `RETICULATE_PYTHON` env variable
Sys.unsetenv("RETICULATE_PYTHON")
d<-"/opt/R/4.1.2/lib/R/library/reticulate/config/"
message(sprintf("contents of %s:", d))
message(paste(dir(d), collapse="\n"))
reticulate::use_virtualenv(file.path(getwd(),conda_name))


suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(rjson)
  library(shinyjs)
  library(dplyr)
  library(shinythemes)
  library(shinydashboard)
  library(stringr)
  library(DT)
  library(jsonlite)
  library(reticulate)
  library(ggplot2)
  library(purrr)
  library(plotly)
  library(shinypop)
  library(waiter)
  library(readr)
  library(sass)
  library(shinydashboardPlus)
})

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

if (interactive()) {
  # for local development
  options(shiny.port = 8100)
  app_url <- "http://localhost:8100/"
} else {
  # deployed url
  app_url <- toString(oauth_client$APP_URL)
}

if (is.null(app_url) || nchar(app_url)==0) stop("config.yaml is missing APP_URL")

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

# Import functions/modules
source_files <- list.files(c("functions", "modules"), pattern = "*\\.R$", recursive = TRUE, full.names = TRUE)
sapply(source_files, FUN = source)

# Global variables
datatypes <- c("project", "folder", "template")
options(sass.cache = FALSE)
