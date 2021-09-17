
<<<<<<< HEAD
\**Please note, "releases" only pertain to changes in the JSON-LD schema file.**

# NF Data Curator App

## Introduction

The _Data Curator App_ is an R Shiny app that serves as the _frontend_ to the schematic Python package. It allows data contributors to easily annotate, validate and submit their metadata.

## Shiny app configuration
=======
## Introduction

The _Data Curator App_ is an R Shiny app that serves as the _frontend_ to the schematic Python package. It allows data contributors to easily annotate, validate and submit their metadata.


## Setup

### Data Curator App Setup
Follow the steps below to make sure the _Data Curator App_ is fully setup to work with the [schematic](https://github.com/Sage-Bionetworks/schematic/tree/main):
>>>>>>> 4eb5ba33273c7f9c5a940018ba01c057272943cd

There are two editions of the front end: 

- https://shiny.synapse.org/users/rallaway/NF_data_curator_staging/ (pulling from the `develop` branch)
- https://shiny.synapse.org/users/rallaway/NF_data_curator/ (pulling from the `production` branch)

## Setup

### Data Curator App Setup
Follow the steps below to make sure the _Data Curator App_ is fully setup to work with the [schematic](https://github.com/Sage-Bionetworks/schematic/tree/main):
\
    git clone --single-branch --branch production https://github.com/nf-osi/NF_data_curator.git


Create a conda environment in the cloned directory from the `environment.yml` file which has all the required package dependencies:

    conda env create -f environment.yml

Here, our conda environment name `data_curator_env` is set from the `environment.yml` file .

Activate the `data_curator_env` environment:

    conda activate data_curator_env
<<<<<<< HEAD

=======


### Schematic Setup
>>>>>>> 4eb5ba33273c7f9c5a940018ba01c057272943cd

### Schematic Setup
The next step is to install the latest release of the [schematic](https://github.com/Sage-Bionetworks/schematic/tree/main) (backend) as a folder `schematic` inside the `data_curator` folder and tie it together with this frontend. 

To do so, follow the instructions on the `schematic` repository [README](https://github.com/Sage-Bionetworks/schematic/tree/develop#12-installation-requirements-and-pre-requisites).


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