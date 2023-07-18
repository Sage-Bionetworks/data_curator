# Deploying Data Curator to shinyapps.io

Anthony Williams

Updated 15-03-2023

Deploying an app to shinyapps.io is done with the [rsconnect package](https://github.com/rstudio/rsconnect/).
This can be done manually or automated in a [GitHub action](.github/workflows/shinyapps_deploy.yml).

The GitHub action installs data curator's system and python dependencies on an Ubuntu [Rstudio server image](https://hub.docker.com/r/rocker/rstudio), and then pushes 
the entire directory to shinyapps.io using `rsconnect`.

## Setup Configuration of OAuth and Schematic
- Follow [these instructions](https://help.synapse.org/docs/Using-Synapse-as-an-OAuth-Server.2048327904.html) to set up an OAuth client for your application.
- For schematic-related credentials, follow [these instructions](https://github.com/Sage-Bionetworks/schematic/blob/develop/docs/md/details.md). You will need to generate `service_account_creds.json` 
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
RSCONNECT_USER # see the shinyapps.io record in LastPass
RSCONNECT_TOKEN # see the shinyapps.io record in LastPass
RSCONNECT_SECRET # see the shinyapps.io record in LastPass
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
There are three instances of data curator on shinyapps.io, "production" and "staging", and "testing". By default, the action is configured to deploy the app to the "production" instance tagged commits with semantic versions (e.g. `v1.0.0`) are pushed. While pushing changes to branches named "main" or "develop*" will auto-deploy app to the staging and testing instances, respectively.

### Install data curator dependencies
Currently, data curator requires several system dependencies to run on shinyapps.io in addition to schematic's python dependencies. libcurl4-openssl-dev

### Determine which schematic API to use
By default, DCA will use schematic via reticulate by setting the R env var `DCA_SCHEMATIC_API_TYPE="reticulate"`. You can set this to "rest" if you want to use Schematic's REST API. In thise case, also set `DCA_API_HOST` to the REST API URL. This is useful if you want the latest features of Schematic, because at the moment shinyapps.io cannot use any release or development version of Schematic after 23.1.1.

### Python set up for reticulate API
Note, currently shinyapps.io cannot use the development releases of Schematic. You can only install schematic via `pip install schematicpy=-23.1.1` 

Outdated: Set up a python virtual environment, install the `develop` version of schematic with `poetry`, and install necessary R packages as instructed in the [README](https://github.com/Sage-Bionetworks/data_curator/blob/main/README.md). 

### Update schematic configurations
Use GitHub secrets to write out the credentials files for OAuth and schematic, which are required for the configuration of data curator.

### Deploy the app with rsconnect
To configure `rsconnect`, use `rsconnect::setAccountInfo(user, token, secret)` with the GH secretes started with "RSCONNECT". To deploy the app to "production" or "staging" instances based on what is setup in the workflow using `rsconnect::deployApp()`.
