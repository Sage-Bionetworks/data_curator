# Data Curator App

## Introduction

This branch is to test the data curator app using the latest updates from `schematic-dev` branch and not use `pip install` to install `schematicpy`.


## Setup

### Data Curator App Setup
Follow the steps below to make sure the _Data Curator App_ is fully setup to work with the [schematic](https://github.com/Sage-Bionetworks/schematic/tree/main):

Navigate to the location where you want to setup the application (i.e., the _Shiny Server_). Clone the code on this github branch (_shiny-server-main_):

    git clone --single-branch --branch use-schematic-dev https://github.com/Sage-Bionetworks/data_curator.git

Create a conda environment in the cloned directory from the `environment.yml` file which has all the required package dependencies:

    conda env create -f environment.yml

Here, our conda environment name `data_curator_env` is set from the `environment.yml` file .

Activate the `data_curator_env` environment:

    conda activate schematic_dev


### Schematic Setup

1. Clone the [schematic](https://github.com/Sage-Bionetworks/schematic/tree/develop) (backend) as a folder `schematic` inside the `data_curator` folder and tie it together with this frontend. 
```
git clone --single-branch --branch develop https://github.com/Sage-Bionetworks/schematic.git
```

2. Install `schematicpy`, check [here](https://github.com/Sage-Bionetworks/schematic/blob/develop/CONTRIBUTION.md#setup-project-for-development-and-testing)

3. Set up configuration for `schematic`

### App Configuration File

Use the app configuration file `www/config.json` to adapt this app to your DCC. 

* `manifest schemas`: defines the list of schemas displayed under the "Choose a Metadata Template Type:" dropdown in the application.
    * `display_name` : The display name for the dropdown. (e.g. _scRNA-seq Level 1_)
    * `schema_name`: The name of the manifest in the JSON-LD schema (e.g. _ScRNA-seqLevel1_)  
    * `type`: The type of manifest. As currently configured in `app.R`, will only display manifests of type _assay_.

* `main_fileview` : The Synapse ID of a fileview that is scoped to all files, folders, & projects in your community.  (e.g. _syn20446927_)
* `community` : the abbreviated name of the community or project. (e.g. _HTAN_)


### Authentication (OAuth)

This utilizes a Synapse OAuth client (code motivated by [ShinyOAuthExample](https://github.com/brucehoff/ShinyOAuthExample) and [app.R](https://gist.github.com/jcheng5/44bd750764713b5a1df7d9daf5538aea). Each application is required to have its own OAuth client as these clients cannot be shared between one another. View instructions [here](https://docs.synapse.org/articles/using_synapse_as_an_oauth_server.html) to learn how to request a client. Once you obtain the `client_id` and `client_secret` make sure to add it to the configuration file.

```
cp example_config.yaml config.yaml
# Edit config.yaml
chmod 400 config.yaml
```