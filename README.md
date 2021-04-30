
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

### Schematic Setup (backend)

The next step is to install the latest release of [schematic](https://github.com/Sage-Bionetworks/schematic). Please follow the instructions on the `schematic` [documentation site](https://sage-schematic.readthedocs.io/en/develop/README.html). 
