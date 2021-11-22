# Data Curator App

## Introduction

The _Data Curator App_ is an R Shiny app that serves as the _frontend_ to the schematic Python package. It allows data contributors to easily annotate, validate and submit their metadata.

---

## Get Started and Installation

Follow the steps below to make sure the _Data Curator App_ is fully setup to work with the [schematic]:

### Data Curator App Setup

1.  Clone this repo (front-end) with one single branch (i.e., _shiny-server-main_):

        git clone --single-branch --branch shiny-server-main https://github.com/Sage-Bionetworks/data_curator.git

2.  Create and modify the configuration file:

        cp example_config.yaml config.yaml
        chmod 400 config.yaml

    - `CLIENT_ID` and `CLIENT_SECRET`: [how to obtain OAuth Credential](https://github.com/Sage-Bionetworks/data_curator#Authentication)
    - `APP_URL`: the redirection url to your app
    - `CONDA_NAME`: conda environment name

3.  Create and activate the conda environment:

        export CONDA_NAME=$(tail -n1 config.yaml | cut -f2 -d':')
        conda env create -f environment.yml -n $CONDA_NAME
        conda activate $CONDA_NAME

4.  Install required R pacakges dependencies:

        R -e "renv::restore()"

### Schematic Setup

1.  Clone the [schematic] (backend) as a folder `schematic` inside the `data_curator` folder:

        git clone --single-branch --branch develop https://github.com/Sage-Bionetworks/schematic.git

2.  Install the latest release of the `schematic` via `pip`:

        python -m pip install schematicpy

    For development and test with the latest update from `schematic`, install the `schematic` via `poetry`:

        cd schematic
        poetry build
        pip install dist/schematicpy-*-py3-none-any.whl

3.  Set up the `schematic` configuration. To do so, follow the instructions on the `schematic` repository [README](https://github.com/Sage-Bionetworks/schematic/tree/develop#12-installation-requirements-and-pre-requisites)

### Data Model Configuration

Use the app configuration file `www/config.json` to adapt this app to your DCC.

- `manifest schemas`: defines the list of schemas displayed under the "Choose a Metadata Template Type:" dropdown in the application.
  - `display_name` : The display name for the dropdown. (e.g. _scRNA-seq Level 1_)
  - `schema_name`: The name of the manifest in the JSON-LD schema (e.g. _ScRNA-seqLevel1_)
  - `type`: The type of manifest. As currently configured in `app.R`, will only display manifests of type _assay_.
- `main_fileview` : The Synapse ID of a fileview that is scoped to all files, folders, & projects in your community. (e.g. _syn20446927_)
- `community` : the abbreviated name of the community or project. (e.g. _HTAN_)

---

## Authentication

This utilizes a Synapse Authentication (OAuth) client (code motivated by [ShinyOAuthExample](https://github.com/brucehoff/ShinyOAuthExample) and [app.R](https://gist.github.com/jcheng5/44bd750764713b5a1df7d9daf5538aea). Each application is required to have its own OAuth client as these clients cannot be shared between one another. View instructions [here](https://docs.synapse.org/articles/using_synapse_as_an_oauth_server.html) to learn how to request a client. Once you obtain the `CLIENT_ID` and `CLIENT_SECRET` make sure to add it to the configuration yaml file.

---

## Contributors

Main contributors and developers:

- [Rongrong Chai](https://github.com/rrchai)
- [Xengie Doan](https://github.com/xdoan)
- [Milen Nikolov](https://github.com/milen-sage)
- [Sujay Patil](https://github.com/sujaypatil96)
- [Robert Allaway](https://github.com/allaway)
- [Bruno Grande](https://github.com/BrunoGrandePhD)

<!-- Links -->

[schematic]: https://github.com/Sage-Bionetworks/schematic/tree/develop
