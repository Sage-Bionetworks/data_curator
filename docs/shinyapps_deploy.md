# Deploying Data Curator to shinyapps.io

Anthony Williams

 02-23-2022

Deploying an app to shinyapps.io is done with the [rsconnect package](https://github.com/rstudio/rsconnect/).
This can be done manually or automated in a [GitHub action](.github/workflows/shinyapps_deploy.yml).

The GitHub action installs data curator's system and python dependencies on an Ubuntu [Rstudio server image](https://hub.docker.com/r/rocker/rstudio), and then pushes 
the entire directory to shinyapps.io using `rsconnect`.

## Setup Configuration of OAuth and Schematic
- See ["Data Curator App Setup #2"](https://github.com/Sage-Bionetworks/data_curator#data-curator-app-setup) to set up `oauth_config.yml` schematic. 
- See ["Schematic Setup #3"](https://github.com/Sage-Bionetworks/data_curator#schematic-setup) to set up `schematic_config.yml`

## Access shinyapps.io
Ask IT for access to the shinyapps.io account info in the Shared-Sage LastPass folder. Make sure you can view the site password. To login, enter credentials on [shinyapps.io](https://www.shinyapps.io/admin/#/login).

## Add GitHub Secrets
Add shinyapps.io and synapse OAuth and schematic info to 
[GitHub secrets](https://docs.github.com/en/actions/security-guides/encrypted-secrets) 
for the repo.
```bash
*OAUTH_CLIENT_ID # your OAuth client ID
*OAUTH_CLIENT_SECRET # your OAuth client secret
*SCHEMATIC_SYNAPSE_CONFIG # content of .synapseConfig
*SCHEMATIC_SERVICE_ACCT_CREDS # content of schematic_service_account_creds.json
*SCHEMATIC_CREDS_PATH # content of credentials.json
*SCHEMATIC_TOKEN_PICKLE # content of token.pickle
RSCONNECT_USER # see the shinyapps.io record in LastPass
RSCONNECT_TOKEN # see the shinyapps.io record in LastPass
RSCONNECT_SECRET # see the shinyapps.io record in LastPass
REPO_PAT # GitHub personal access token
```

To automatically add credentials with * to GH secrets via [GitHub CLI](https://cli.github.com/manual/index) (Note: you still need to manually add shinyapps.io's credentials and GH PAT):

```bash
# follow instructions to configure GH CLI
gh auth login
# add secrets for OAuth and schematic  
Rscript set_gh_secrets.R oauth_config.yml schematic_config.yml
```

## Configure the [GitHub workflow file](.github/workflows/shinyapps_deploy.yml)

### Specify branches to push to which instances
There are two instances of data curator on shinyapps.io, "production" and "staging". By default, the action is configured to deploy the app to the "production" instance if there are pushes on tags named as semantic versions (e.g. `v1.0.0`). While pushing changes to branches named "main" or "develop" will auto-deploy app to the staging instance.

### Install data curator dependencies
Currently, data curator requires several system dependencies to run on shinyapps.io in addition to schematic's python dependencies. libcurl4-openssl-dev

Set up a python virtual environment, install the `develop` version of schematic with `poetry`, and install necessary R packages as instructed in the [README](https://github.com/Sage-Bionetworks/data_curator/blob/main/README.md). 

### Update schematic configurations
Use GitHub secrets to write out the credentials files for OAuth and schematic, which are required for the configuration of data curator.

### Deploy the app with rsconnect
To configure `rsconnect`, use `rsconnect::setAccountInfo(user, token, secret)` with the GH secretes started with "RSCONNECT". To deploy the app to "production" or "staging" instances based on what is setup in the workflow using `rsconnect::deployApp()`.
