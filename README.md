# HTAN Data Curator App
## Development Environment Setup

Follow the steps below to make sure the _Data Curator App_ (frontend) is fully setup with the [Data Ingress Pipeline](https://github.com/Sage-Bionetworks/HTAN-data-pipeline/tree/organized-into-packages) (backend):

Navigate to the location where you want to setup the application (i.e., the Shiny Server). Clone the code on this github branch (_shiny-server-packaged-backend_):

    git clone --single-branch --branch shiny-server-packaged-backend https://github.com/Sage-Bionetworks/HTAN_data_curator.git

Notes:

1. Make sure you have the `credentials.json` file (currently stored on Synapse) in the same directory (created after the cloning step above).
2. Once you run the app it will create a `token.pickle` file within the same directory, and that file will be used for subsequent authorizations/verifications to the Google services.
3. Configure the `config.py` file with your Syanpse credentials (_username/password_) found in `ingresspipe/config/config.py`.

Create a conda environment in the cloned directory:

    conda create --name data_curator_env

Here we are calling our conda environment `data_curator_env`. You can change it anything you like, but please note that you will need to make changes accordingly in the `app.R` file.

Activate the `data_curator_env` environment:

    conda activate data_curator_env

Install the backend (`ingresspipe` package) within the conda virtual environment:

    pip install -e .

Note: _For more instructions see, [Data Ingress Pipeline Docs](https://github.com/Sage-Bionetworks/HTAN-data-pipeline/tree/organized-into-packages#readme)_.

To verify that the backend is installed do (either of) the following:

    pip list

See if you can find the `ingresspipe` package in the list of packages installed using `pip`.

Run any example(s) from within the `ingresspipe` package as follows:

    python ingresspipe/models/metadata.py
