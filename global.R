suppressPackageStartupMessages({
  library(yaml)
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
  # dashboard
  library(purrr)
  library(data.table)
  library(networkD3)
  library(data.tree)
  library(r2d3)
})

## Set Up OAuth
client_id <- Sys.getenv("DCA_CLIENT_ID")
client_secret <- Sys.getenv("DCA_CLIENT_SECRET")
app_url <- Sys.getenv("DCA_APP_URL")

message(Sys.getenv("DCA_SCHEMATIC_API_TYPE"))
message(paste(readLines(".Renviron"), collapse="\n"))

if (is.null(client_id) || nchar(client_id) == 0) stop("missing DCA_CLIENT_ID environmental variable")
if (is.null(client_secret) || nchar(client_secret) == 0) stop("missing DCA_CLIENT_SECRET environmental variable")
if (is.null(app_url) || nchar(app_url) == 0) stop("missing DCA_APP_URL environmental variable")

dca_schematic_api <- Sys.getenv("DCA_SCHEMATIC_API_TYPE")
if (dca_schematic_api == "rest") {
  api_uri <- ifelse(Sys.getenv("DCA_API_PORT") == "",
                    Sys.getenv("DCA_API_HOST"),
                      paste(Sys.getenv("DCA_API_HOST"),
                        Sys.getenv("DCA_API_PORT"),
                        sep = ":")
  )
}

update_logo <- function(project = "sage") {
  
  img <- switch(project,
                 syn20446927 = list(href = "https://humantumoratlas.org/",
                                    img_src = "img/HTAN_text_logo.png"),
                 syn27210848 = list(href = "https://cancercomplexity.synapse.org/",
                                    img_src = "img/cckp_logo.png"),
                 syn30109515 = list(href = "https://https://includedcc.org/",
                                    img_src = "img/INCLUDE DCC Logo-01.png"),
                 list(href = "https://synapse.org",
                      img_src = "img/synapse_logo.png")
   )

  tags$li(
      class = "dropdown", id = "logo",
      tags$a(
          href = img$href,
          target = "_blank",
          tags$img(
              height = "40px", alt = "LOGO",
              src = img$img_src
            )
        )
    )
}

syn_themes <- c(
    "syn20446927" = "www/dca_themes/htan_theme_config.rds",
    "syn27210848" = "www/dca_themes/mc2_theme_config.rds",
    "syn30109515" = "www/dca_themes/include_theme_config.rds"
  )

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

# parse environment variables for configuration
parse_env_var <- function(x, el_delim=",", kv_delim=":"){
  # assume string of key-value pairs
  elements <- stringr::str_split(x, el_delim, simplify = TRUE)
  unlist(lapply(elements, function(y){
    kv <- stringr::str_split(y, kv_delim, n=2)
    setNames(kv[[1]][[2]], kv[[1]][[1]])
  }))
}

# import R files
source_files <- list.files(c("functions", "modules"), pattern = "*\\.R$", recursive = TRUE, full.names = TRUE) %>%
  .[!grepl("dashboard", .)]
sapply(source_files, FUN = source)

## Set Up Virtual Environment
# ShinyAppys has a limit of 7000 files which this app' grossly exceeds
# due to its Python dependencies.  To get around the limit we zip up
# the virtual environment before deployment and unzip it here.
# unzip virtual environment, named as ".venv.zip"
if (dca_schematic_api == "reticulate"){
  if (!file.exists(".venv")) utils::unzip(".venv.zip")
  
  # We get a '126' error (non-executable) if we don't do this:
  system("chmod -R +x .venv")
  
  library(reticulate)
  #reticulate::use_virtualenv(file.path(getwd(), ".venv"), required = TRUE)
  # syn <<- reticulate::import("synapseclient")$Synapse()
  # 
  # MetadataModel <<- reticulate::import("schematic.models.metadata")$MetadataModel
  # CONFIG <<- reticulate::import("schematic")$CONFIG
  # SchemaGenerator <<- reticulate::import("schematic.schemas.generator")$SchemaGenerator
  # 
  # config = CONFIG$load_config("schematic_config.yml")
  # 
  # inputMModelLocation = config$model$input$location
  # inputMModelLocationType = config$model$input$file_type
  # 
  # manifest_title = config$manifest$title
  # manifest_data_type = config$manifest$data_type[1]
  # 
  # metadata_model <<- MetadataModel(inputMModelLocation, inputMModelLocationType)
  # 
  # # create schema generator object for associateMetadataWithFiles
  # schema_generator <<- SchemaGenerator(inputMModelLocation)
  # 
  # synapse_driver <<- reticulate::import("schematic.store.synapse")$SynapseStorage
  #setup_synapse_driver()
  
  ## Read config.json
#  if (!file.exists("www/config.json")) {
#    system(
#      "python3.10 .github/config_schema.py -c schematic_config.yml --service_repo 'Sage-Bionetworks/schematic' --overwrite"
#    )
#  }
}
#config_file <- fromJSON("www/config.json")


## Global variables
dropdown_types <- c("project", "folder", "template")
# set up cores used for parallelization
ncores <- parallel::detectCores() - 1
