# NF Data Curator App

## Shiny app configuration

There are two editions of the front end: 

- https://shiny.synapse.org/users/rallaway/NF_data_curator_staging/ (pulling from the `develop` branch)
- https://shiny.synapse.org/users/rallaway/NF_data_curator/ (pulling from the `production` branch)

## Making changes 

To make changes, please branch off of `develop`, make changes, and then file a PR against `develop` (no review needed, but PR for easy revert). Once the changes have been deployed and tested, PR against `production`. Note - as with all Shiny apps, `touch restart.txt` any time you make changes to make sure they are reflected in your testing. 

For the latest documentation on how to deploy this app and backend, please visit https://github.com/Sage-Bionetworks/HTAN_data_curator and  https://github.com/Sage-Bionetworks/schematic.

We've made forks of both of these repositories here and here: https://github.com/Sage-Bionetworks/schematic.

## Test/Demo Project

Use [this](https://www.synapse.org/#!Synapse:syn22410511/files/) sandbox Synapse project (shortlink: synapse.org/nfdemo) for demos and testing. 

## Development Environment Setup

You likely won't need the instructions below unless a major change happens, but we've left them here just in case: 

### Data Curator App Setup (frontend)
Follow the steps below to make sure the _Data Curator App_ (frontend) is fully setup to work with the [Data Ingress Pipeline](https://github.com/nf-osi/schematic/) (backend):

Navigate to the location where you want to setup the application (i.e., the _Shiny Server_). Clone the code on this github branch (_production_):

    git clone --single-branch --branch production https://github.com/nf-osi/NF_data_curator.git

Create a conda environment in the cloned directory from the `environment.yml` file which has all the required package dependencies:

    conda env create -f environment.yml

Here, our conda environment name `data_curator_env` is set from the `environment.yml` file .

Activate the `data_curator_env` environment:

    conda activate data_curator_env

-------

### Data Ingress Pipeline Setup (backend)

The next step is to install the latest release of the [Data Ingress Pipeline](https://github.com/Sage-Bionetworks/schematic) (backend) as a folder `schematic` inside the `NF_data_curator` folder and tie it together with this frontend. 

To do so carry out the following steps:

1. Inside the `HTAN_data_curator` folder, clone the repo from this [location](https://github.com/Sage-Bionetworks/schematic), by running the following command:

    `git clone --single-branch --branch production https://github.com/nf-osi/schematic.git`

This creates a folder named `schematic` inside the the `NF_data_curator folder`.

2. Navigate into the created `schematic` directory. Install the backend (`schematic` package) within the conda virtual environment by running:

    `pip install -e .`

To verify that the backend is installed, do this: `pip list`

If you can find the `schematic` package in the list of packages installed it was successful.

3. Obtain the `credentials.json` file in `schematic` to authenticate user access to Google API services which will be used to create the metadata templates. If you do not already have this file, make sure you are authorized (see _Notes_ below) and run the below command within `HTAN-data-pipeline` to download the HTAN credentials file `syn21088684` through the `synapseclient` (part of the backend):

    `synapse get syn21088684`

4. Obtain the `token.pickle` file in `schematic` which is also necessary for authentication. If you do not already have this file run the `metadata_usage` example as follows inside `schematic`:

    `python examples/metadata_model.py`

This will prompt you with a URL on your console to Google's authorization process. Follow that and upon completion the `token.pickle` file will automatically be downloaded to the required location.

_Notes:_

- You can install the package by changing from anywhere else by changing the `.` to whatever the path is to the to where you have downloaded the package (`pip install -e /path/to/package`).

- `syn21088684` is the Synapse ID of the HTAN specific `credentials.json` file on Synapse that corresponds to the Google service account that creates HTAN templates.

- You need to be authorized to download protected Synapse files such as credentials. Please contact milen.nikolov@sagebase.org for access to the HTAN credentials.

- If you want to test the backend you can run other things inside `schematic`, but in order to run the `examples/synapse_store.py` example, you need to configure your Synapse credentials in the `.synapseConfig` file (which can be found in the `schematic` directory), as described [here](https://github.com/Sage-Bionetworks/schematic/tree/develop#configure-synapse-credentials).
