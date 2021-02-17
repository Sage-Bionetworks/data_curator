# HTAN Data Curator App
## Development Environment Setup

### Data Curator App Setup (frontend)
Follow the steps below to make sure the _Data Curator App_ (frontend) is fully setup to work with the [schematic](https://github.com/Sage-Bionetworks/schematic/tree/main) (backend):


Navigate to the location where you want to setup the application (i.e., the _Shiny Server_). Clone the code on this github branch (_shiny-server-1.2.1.rc1_):

    git clone --single-branch --branch shiny-server-1.2.1.rc1 https://github.com/ncihtan/HTAN_data_curator.git

Create a conda environment in the cloned directory from the `environment.yml` file which has all the required package dependencies:

    conda env create -f environment.yml

Here, our conda environment name `data_curator_env` is set from the `environment.yml` file .

Activate the `data_curator_env` environment:

    conda activate data_curator_env
    
_Note_:
- You can change the name of your conda environment inside `environment.yml` or even use another environment, but please note that you will need to make changes accordingly in the `app.R` file line 16.

-------

### Schematic Setup (backend)

The next step is to install the latest release of the [schematic](https://github.com/Sage-Bionetworks/schematic/tree/main) (backend) as a folder `schematic` inside the `HTAN_data_curator` folder and tie it together with this frontend. 

To do so carry out the following steps:

1. Inside the `HTAN_data_curator` folder, clone the repo from this [location](https://github.com/Sage-Bionetworks/schematic/tree/main), by running the following command:

    `git clone --single-branch --branch main https://github.com/Sage-Bionetworks/schematic.git`

This creates a folder named `schematic` inside the the `HTAN_data_curator folder`.

2. Navigate into the created `schematic` directory. Install the backend (`schematic` package) within the conda virtual environment by running:

    `pip install -e .`

To verify that the backend is installed, do this: `pip list`

If you can find the `schematic` package in the list of packages installed it was successful.

3. Obtain the `credentials.json` file in `schematic` to authenticate user access to Google API services which will be used to create the metadata templates. If you do not already have this file, make sure you are authorized (see _Notes_ below) and run the below command within `schematic` to download the HTAN credentials file `syn21088684` through the `synapseclient` (part of the backend):

    `synapse get syn21088684`

4. Obtain the `token.pickle` file in `schematic` which is also necessary for authentication. If you do not already have this file run the `metadata_usage` example as follows inside `schematic`:

    `python examples/metadata_model.py`

This will prompt you with a URL on your console to Google's authorization process. Follow that and upon completion the `token.pickle` file will automatically be downloaded to the required location.

_Notes:_

- You can install the package by changing from anywhere else by changing the `.` to whatever the path is to the to where you have downloaded the package (`pip install -e /path/to/package`).

- `syn21088684` is the Synapse ID of the HTAN specific `credentials.json` file on Synapse that corresponds to the Google service account that creates HTAN templates.

- You need to be authorized to download protected Synapse files such as credentials. Please contact milen.nikolov@sagebase.org for access to the HTAN credentials.

- If you want to test the backend you can run other things inside `schematic`, but in order to run the `examples/synapse_store.py` example, you need to configure your Synapse credentials in the `.synapseConfig` file (which can be found in the `schematic` directory), as described [here](https://github.com/Sage-Bionetworks/schematic/tree/main#configure-synapse-credentials).


### App Configuration file

Use the app configuration file `www/config.json` to adapt this app to your DCC. 

* `manifest schemas`: defines the list of schemas displayed under the "Choose a Metadata Template Type:" dropdown in the application.
    * `display_name` : The display name for the dropdown. (e.g. "Genomics Assay")
    * `schema_name`: The name of the manifest in the JSON-LD schema (e.g. "GenomicsAssay")  
    * `type`: The type of manifest. As currently configured in `app.R`, will only display manifests of type "assay".

* `main_fileview` : The Synapse ID of a fileview that is scoped to all files, folders, & projects in your community.  (e.g. "syn20446927")
* `community` : the abbreviated name of the community or project. (e.g. "HTAN")
}
