# Data Curator App

## Introduction

The _Data Curator App_ is an R Shiny app that serves as the _frontend_ to the schematic Python package. It allows data contributors to easily annotate, validate and submit their metadata.

---

## Get Started and Installation

Follow the steps below to make sure the _Data Curator App_ is fully setup to work with the [schematic]:

### Data Curator App Setup

1.  Clone this repo (front-end) with one single branch (i.e., _main_):

        git clone --single-branch --branch main https://github.com/Sage-Bionetworks/data_curator.git

2.  Create and modify the configuration file ([How to obtain OAuth Credential](https://github.com/Sage-Bionetworks/data_curator#Authentication)):

        cp example_oauth_config.yml oauth_config.yml
        chmod 400 oauth_config.yml

3.  Create and activate a virtual environment within which you can install the package:

        python -m venv .venv
        source .venv/bin/activate

4.  Install required R pacakges dependencies:

        R -f install-pkgs.R

### Schematic Setup

1.  Clone the [schematic] (backend) as a folder `schematic` inside the `data_curator` folder:

        git clone --single-branch --branch develop https://github.com/Sage-Bionetworks/schematic.git

2.  Install the latest release of the `schematic` via `pip`. IF NOT USING CONDA, install the devel version below:

        python -m pip install schematicpy

    For development and test with the latest update from `schematic`, install the `schematic` via [poetry]:

        cd schematic
        poetry build
        pip install dist/schematicpy-1.0.0-py3-none-any.whl

3.  Modify the `schematic_config.yml` to set up schematic configuration. To do so, follow the instructions on the [schematic's documentation](https://sage-schematic.readthedocs.io/en/develop/index.html#package-installation-and-setup)

### Data Model Configuration

Use the app configuration file `www/config.json` to adapt this app to your DCC.

- `manifest schemas`: defines the list of schemas displayed under the "Choose a Metadata Template Type:" dropdown in the application.
  - `display_name` : The display name for the dropdown. (e.g. _scRNA-seq Level 1_)
  - `schema_name`: The name of the manifest in the JSON-LD schema (e.g. _ScRNA-seqLevel1_)
  - `type`: The type of manifest. Either _file (e.g. scRNAseq-level1)_ or _record (e.g. biosepcimen and clinical data)_.
- `community` (optional): The abbreviated name of the community or project. (e.g. _HTAN_)
- `schema` (optional): The version of data model
- `schematic_service` (optional): The version of schematic service

---

## Authentication

This utilizes a Synapse Authentication (OAuth) client (code motivated by [ShinyOAuthExample](https://github.com/brucehoff/ShinyOAuthExample) and [app.R](https://gist.github.com/jcheng5/44bd750764713b5a1df7d9daf5538aea). Each application is required to have its own OAuth client as these clients cannot be shared between one another. View instructions [here](https://docs.synapse.org/articles/using_synapse_as_an_oauth_server.html) to learn how to request a client. Once you obtain the client, make sure to add it to the configuration yaml file:

- `CLIENT_ID` and `CLIENT_SECRET`
- `APP_URL`: the redirection url to your app

---

## Deployment

To deploy the app to shinyapps.io, please follow the instructions in the [shinyapps_deploy.md](./shinyapps_deploy.md).

## Contributors

Main contributors and developers:

- [Rongrong Chai](https://github.com/rrchai)
- [Anthony Williams](https://github.com/afwillia)
- [Milen Nikolov](https://github.com/milen-sage)
- [Lauren Wolfe](https://github.com/lakikowolfe)
- [Robert Allaway](https://github.com/allaway)
- [Bruno Grande](https://github.com/BrunoGrandePhD)
- [Xengie Doan](https://github.com/xdoan)
- [Sujay Patil](https://github.com/sujaypatil96)

<!-- Links -->

[schematic]: https://github.com/Sage-Bionetworks/schematic/tree/develop
[poetry]: https://github.com/python-poetry/poetry
