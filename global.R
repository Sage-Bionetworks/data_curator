suppressPackageStartupMessages({
  library(yaml)
  library(reticulate)
  library(httr)
  library(shiny)
  library(shinyjs)
  library(dplyr)
  library(tidyr)
  library(shinythemes)
  library(shinydashboard)
  library(stringr)
  library(DT)
  library(jsonlite)
  library(shinypop)
  library(waiter)
  library(readr)
  library(sass)
  library(shinydashboardPlus)
})

## Set Up OAuth
oauth_client <- yaml.load_file("oauth_config.yml")

client_id <- toString(oauth_client$CLIENT_ID)
client_secret <- toString(oauth_client$CLIENT_SECRET)
app_url <- toString(oauth_client$APP_URL)

if (is.null(client_id) || nchar(client_id) == 0) stop("oauth_config.yml is missing CLIENT_ID")
if (is.null(client_secret) || nchar(client_secret) == 0) stop("oauth_config.yml is missing CLIENT_SECRET")
if (is.null(app_url) || nchar(app_url) == 0) stop("oauth_config.yml is missing APP_URL")

# update port if running app locally
if (interactive()) {
  port <- httr::parse_url(app_url)$port
  if (is.null(port)) stop("running locally requires a TCP port that the application should listen on")
  options(shiny.port = as.numeric(port))
}

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

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

## Set Up Virtual Environment
# ShinyAppys has a limit of 7000 files which this app' grossly exceeds
# due to its Python dependencies.  To get around the limit we zip up
# the virtual environment before deployment and unzip it here.

# unzip virtual environment, named as ".venv.zip"
if (!file.exists(".venv")) utils::unzip(".venv.zip")

# We get a '126' error (non-executable) if we don't do this:
system("chmod -R +x .venv")

# Don't necessarily have to set `RETICULATE_PYTHON` env variable
Sys.unsetenv("RETICULATE_PYTHON")
reticulate::use_virtualenv(file.path(getwd(), ".venv"), required = TRUE)

## Import functions/modules
source_files <- list.files(c("functions", "modules"), pattern = "*\\.R$", recursive = TRUE, full.names = TRUE)
sapply(source_files, FUN = source)

## Read config.json
if (!file.exists("www/config.json")) {
  schematic_config <- yaml.load_file("schematic_config.yml")
  system(sprintf(
    "python3 .github/generate_config_json.py -jd %s -schema %s -service %s",
    schematic_config$model$input$location, schematic_config$model$input$repo, "Sage-Bionetworks/schematic"
  ))
}
config_file <- fromJSON("www/config.json")

## Global variables
datatypes <- c("project", "folder", "template")