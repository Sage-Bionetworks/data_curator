library(shiny)
library(httr)
library(rjson)
library(yaml)


# APP_URL <- "https://shinypro.synapse.org/users/spatil/HTAN-oauth/"

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

oauth_client = yaml.load_file("config.yaml")

client_id <- toString(oauth_client$client_id)
client_secret <- oauth_client$client_secret
APP_URL <- oauth_client$APP_URL
if (is.null(client_id)) stop("config.yaml is missing client_id")
if (is.null(client_secret)) stop("config.yaml is missing client_secret")
if (is.null(APP_URL)) stop("config.yaml is missing client_secret")

app <- oauth_app("shinysynapse",
                 key = client_id,
                 secret = client_secret, 
                 redirect_uri = APP_URL)

# These are the user info details ('claims') requested from Synapse:
claims=list(
  family_name=NULL, 
  given_name=NULL,
  email=NULL,
  email_verified=NULL,
  userid=NULL,
  orcid=NULL,
  is_certified=NULL,
  is_validated=NULL,
  validated_given_name=NULL,
  validated_family_name=NULL,
  validated_location=NULL,
  validated_email=NULL,
  validated_company=NULL,
  validated_at=NULL,
  validated_orcid=NULL,
  company=NULL
)

claimsParam<-toJSON(list(id_token = claims, userinfo = claims))
api <- oauth_endpoint(
  authorize=paste0("https://signin.synapse.org?claims=", claimsParam),
  access="https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
)

# The 'openid' scope is required by the protocol for retrieving user information.
scope <- "openid view download modify"
