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
  library(promises)
  library(future)
  # dashboard
  library(purrr)
  library(data.table)
  library(networkD3)
  library(data.tree)
  library(r2d3)
})

# Set up futures/promises for asynchronous calls
ncores <- availableCores()
message(sprintf("Available cores: %s", ncores))
plan(multicore, workers = ncores)

# import R files
source_files <- list.files(c("functions", "modules"), pattern = "*\\.R$", recursive = TRUE, full.names = TRUE)
sapply(source_files, FUN = source)

if (Sys.getenv("DCA_DCC_CONFIG") == "") stop("missing DCA_DCC_CONFIG environment variable")
dca_dcc_config <- read_json(Sys.getenv("DCA_DCC_CONFIG"), simplifyVector = TRUE)
tenants_config <- dca_dcc_config$tenants
config_dir <- dirname(Sys.getenv("DCA_DCC_CONFIG"))

## Set Up OAuth
client_id <- Sys.getenv("DCA_CLIENT_ID")
client_secret <- Sys.getenv("DCA_CLIENT_SECRET")
app_url <- Sys.getenv("DCA_APP_URL")

if (is.null(client_id) || nchar(client_id) == 0) stop("missing DCA_CLIENT_ID environmental variable")
if (is.null(client_secret) || nchar(client_secret) == 0) stop("missing DCA_CLIENT_SECRET environmental variable")
if (is.null(app_url) || nchar(app_url) == 0) stop("missing DCA_APP_URL environmental variable")

schematic_config <- yaml.load_file("schematic_config.yml")
manifest_basename <- schematic_config$synapse$manifest_basename

dca_schematic_api <- Sys.getenv("DCA_SCHEMATIC_API_TYPE")
if (!dca_schematic_api %in% c("rest", "reticulate", "offline")) {
  stop(sprintf("DCA_SCHEMATIC_API_TYPE environment variable must be one of: %s", c("rest", "reticulate", "offline")))
}
if (dca_schematic_api == "rest") {
  api_uri <- ifelse(Sys.getenv("DCA_API_PORT") == "",
  Sys.getenv("DCA_API_HOST"),
  paste(Sys.getenv("DCA_API_HOST"),
  Sys.getenv("DCA_API_PORT"),
  sep = ":")
  )
  
  # Get Schematic version
  get_schematic_version <- try(httr::GET(file.path(api_uri, "v1/version")), silent=TRUE)
  if (inherits(get_schematic_version, "try-error")) {
    schematic_version <- ""
  } else if (httr::http_error(get_schematic_version)) {
    schematic_version <- ""
  } else schematic_version <- httr::content(get_schematic_version)
}

dca_version <- Sys.getenv("DCA_VERSION")

dca_synapse_api <- Sys.getenv("DCA_SYNAPSE_PROJECT_API")

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
if (dca_schematic_api == "reticulate"){
  if (!file.exists(".venv")) utils::unzip(".venv.zip")
  
  # We get a '126' error (non-executable) if we don't do this:
  system("chmod -R +x .venv")
  # Don't necessarily have to set `RETICULATE_PYTHON` env variable
  Sys.unsetenv("RETICULATE_PYTHON")
  #setup_synapse_driver()
  
  ## Read config.json
  if (!file.exists("www/config.json")) {
  #    system(
  #      "python3 .github/config_schema.py -c schematic_config.yml --service_repo 'Sage-Bionetworks/schematic' --overwrite"
  #    )
  }
}

## Global variables
dropdown_types <- c("project", "folder", "template")
options(sass.cache = FALSE)
