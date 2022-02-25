# Deploying Data Curator to shinyapps.io
23FEB2022
Anthony Williams

Deploying an app to shinyapps.io is done with the [rsconnect package](https://github.com/rstudio/rsconnect/)
This can be done manually or automated in a [GitHub action](https://docs.github.com/en/actions/quickstart).

The GitHub action installs data curator's system and python dependencies on an Ubuntu 
[Rstudio server image](https://hub.docker.com/r/rocker/rstudio) then pushes 
the entire directory to shinyapps.io using rsconnect.

# Access shinyapps.io
Ask IT for access to the shinyapps.io account info in the Shared-Sage LastPass
folder. Make sure you can view the site password. To login, enter credentials on 
https://www.shinyapps.io/admin/#/login.

# Add GitHub Secrets
Add shinyapps.io and synapse OAuth and schematic info to 
[GitHub secrets](https://docs.github.com/en/actions/security-guides/encrypted-secrets) 
for the repo.
OAUTH_CLIENT_ID
OAUTH_CLIENT_SECRET
RSCONNECT_USER
RSCONNECT_TOKEN
RSCONNECT_SECRET
SCHEMATIC_SYNAPSE_CONFIG - .synapseConfig
SCHEMATIC_SERVICE_ACCT_CREDS - schematic_service_account_creds.json
SCHEMATIC_CREDS_PATH - credentials.json
SCHEMATIC_TOKEN_PICKLE - token.pickle

# Configure the GitHub workflow file

## Specify branches to push to which instances
data_curator/.github/workflows/shinyapps_deploy.yml.
There are two instances of data curator on shinyapps.io, release and staging.
The action is configured to push branches named "release*" to the 
release instance, while other hard-coded branches, like shiny-server-main, are
pushed to the staging instance.

## Install data curator dependencies
Currently, data curator requires several system dependencies to run on shinyapps.io 
in addition to schematic's python dependencies. libcurl4-openssl-dev

Set up a python virtual environment, install the devel version of schematic 
with poetry, and install necessary R packages as instructed in the README. 

## Update schematic configurations
Schematic requires configuration for data curator, so use a prepopulated file. 
Download a [schematic_config.yml file](https://github.com/Sage-Bionetworks/data_curator/blob/shiny-server-main/.github/schematic_config.yml) and update its fields with GitHub secret information.
SCHEMATIC_SYNAPSE_CONFIG - .synapseConfig
SCHEMATIC_SERVICE_ACCT_CREDS - schematic_service_account_creds.json
SCHEMATIC_CREDS_PATH - credentials.json
SCHEMATIC_TOKEN_PICKLE - token.pickle

## Deploy the app with rsconnect
Set the RSCONNECT user, secret, and token and create a
config.yml file with GitHub secrets. Set the shinyapps.io instance to release
or staging based on the name of the branch. Deply with rsconnect::deployApp().
