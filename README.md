# HTAN Data Curator App
## Development Environment Setup

Follow the steps below to make sure the _Data Curator App_ (frontend) is fully setup to work with the [Data Ingress Pipeline](https://github.com/Sage-Bionetworks/HTAN-data-pipeline/tree/organized-into-packages) (backend):

Navigate to the location where you want to setup the application (i.e., the _Shiny Server_). Clone the code on this github branch (_shiny-server-packaged-backend_):

    git clone --single-branch --branch shiny-server-packaged-backend https://github.com/Sage-Bionetworks/HTAN_data_curator.git

Create a conda environment in the cloned directory from the `environment.yml` file which has all the required package dependencies:

    conda env create -f environment.yml

Here, our conda environment name `data_curator_env` is set from the `environment.yml` file . You can change it anything you like, but please note that you will need to make changes accordingly in the `app.R` file.

Activate the `data_curator_env` environment:

    conda activate data_curator_env

======

The next step is to install the latest release of the [Data Ingress Pipeline](https://github.com/Sage-Bionetworks/HTAN-data-pipeline/tree/organized-into-packages) (backend) and tie it together with this frontend. To do so carry out the following steps:

1. Clone the repo from this [location](https://github.com/Sage-Bionetworks/HTAN-data-pipeline/tree/organized-into-packages), by running the following command:

    git clone --single-branch --branch organized-into-packages https://github.com/Sage-Bionetworks/HTAN-data-pipeline.git

2. Navigate into the created `HTAN-data-pipeline` directory. Install the backend (`ingresspipe` package) within the conda virtual environment by running:

    pip install -e .

_Notes:_

- You need to be within the `HTAN-data-pipeline` directory in order to run the command as above. Else you can install the package by changing from anywhere else by changing the `.` to whatever the path is to the to where you have downloaded the package (`pip install -e /path/to/package`).

- To verify that the backend do this:

    `pip list`

See if you can find the `ingresspipe` package in the list of packages installed.

3. Then, you need to download the `credentials.json` file, which is the credentials file that is used in order to authenticate user access to Google API services (such as gDrive, gSheets, etc.). After the step above you will have installed the `synapseclient` package via pip which has a powerful command line utility that lets you access files on Synapse. To download the `credentials.json` file, run the below command (within `HTAN-data-pipeline`):

    synapse get syn21088684

_Note: `syn21088684` is the synapse ID of the `credentials.json` file/entity on Synapse._

4. Next, we need to make sure we have the `token.pickle` file which is also necessary for authentication. To acquire that, run the `metadata_usage` example as follows:

    python ingresspipe/models/examples/metadata_usage.py

This will prompt you with a URL on your console. Copy and paste the URL in your browser. Use your email ID to go through the authentication process and allow the application/script (called _Quickstart_) to access Drive/Sheets/etc. You will generate an authentication/authorization code at the end. Copy and paste the code in the console. The `token.pickle` file will automatically get downloaded to the required location after that.

_Note:_

- In order to run the `synapse.store_usage` example, you need to configure your Synapse credentials in the `.synapseConfig` file (which can be found in the `HTAN-data-pipeline` directory), as described [here](https://github.com/Sage-Bionetworks/HTAN-data-pipeline/tree/organized-into-packages#configure-synapse-credentials).

======