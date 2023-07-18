# Data Curator App

## Introduction

The *Data Curator App* is an R Shiny app that serves as the *frontend* to the [schematic Python package](github.com/sage-Bionetworks/schematic/). It allows data contributors to easily annotate, validate and submit their metadata.

## Sage's Multitenant DCA 

Sage Bionetworks hosts a version of Data Curator App for its collaborators.\
To configure your project for this version, fork [data_curator_config](https://github.com/Sage-Bionetworks/data_curator_config/) and follow the instructions in the [README](https://github.com/Sage-Bionetworks/data_curator_config/#readme).

Other things you will need:\
- A Synapse asset view for you project\
- A [data model](#datamodel)

## Setup a local instance of DCA

### 1. Clone this repo and install required R packages.

```         
git clone https://github.com/Sage-Bionetworks/data_curator.git
cd data_curator
R -e "renv::restore()"
```

### 2. Set up [schematic](github.com/sage-Bionetworks/schematic/)

DCA can use Schematic through [reticulate](https://rstudio.github.io/reticulate/) or a REST API.

Using Schematic with reticulate requires python 3.9 or greater. Create a python virtual environment named `.venv` and install schematicpy through [pypi](https://pypi.org/project/schematicpy/) or from [GitHub](github.com/sage-Bionetworks/schematic/) using [poetry](https://python-poetry.org/docs/). Follow the links to Schematic for more details on installation.

```         
# python virtual env must be named .venv
python3 -m venv .venv

# For pypi release of schematic, run this line
pip3 install schematicpy 

# Or for development schematic, run the following. Note you'll need to install poetry.
git clone https://github.com/Sage-Bionetworks/schematic.git
cd schematic
poetry shell
poetry install

# At this point you can also run the REST API service locally
# This will be accessible at http://0.0.0.0:3001
poetry run python3 run_api.py
```

To use Schematic through its REST API, run the service locally using the commands above. Or access Schematic hosted by Sage Bionetwork.

### 3. Configure App

Many app and schematic configurations are set in [dcc_config.csv](https://github.com/Sage-Bionetworks/data_curator_config/blob/main/dcc_config.csv). The following are stored as environment variables. Add these to `.Renviron`.

Schematic configurations

```         
DCA_SCHEMATIC_API_TYPE: "rest", "reticulate", or "offline"  
DCA_API_HOST: "" (blank string) if not using the REST API, otherwise URL to schematic service  
DCA_DCC_CONFIG: URL or filepath to dcc_config.csv file
DCA_SYNAPSE_PROJECT_API: TRUE or FALSE whether to use the Synapse API instead of Schematic for some fileview queries
```

OAuth-related variables

```         
DCA_CLIENT_ID: OAuth client ID  
DCA_CLIENT_SECRET: OAuth client secret  
DCA_APP_URL: OAuth redirect URL
```

------------------------------------------------------------------------

### Data Model Configuration

The app configuration file `www/config.json` will be used to adapt the schema dropdown menu in the app. The `config.json` file will be automatically created in the deployment workflow.

For local testing, run below snippet to generate `www/config.json` and check the [docs](docs/app_configuration.md#schema-configuration) how to modify it:

1.  Create a repo for your data model using this [template](https://github.com/Sage-Bionetworks/data-models)

2.  Clone your data model repo, i.e:

```         
git clone https://github.com/Sage-Bionetworks/data-models
```

3.  Create `config.json` and placed it in the `www` folder. For this script to work, your data model needs at least one record with a non-empty `dependsOn Component`. This could be a new record with a mock component that depends on a template component. For example,

```         
Attribute,Description,Valid Values,DependsOn,Properties,Required,Parent,DependsOn Component,Source,Validation Rules
Bulk RNA-seq Level 1,Bulk RNA-seq [EFO_0003738],,"Component, Filename, File Format, HTAN Parent Biospecimen ID, HTAN Data File ID, Library Layout, Read Indicator, Nucleic Acid Source, Sequencing Platform, Sequencing Batch ID, Read Length, Library Selection Method, Library Preparation Kit Name, Library Preparation Kit Vendor, Library Preparation Kit Version, Library Preparation Days from Index, Spike In, Adapter Name, Adapter Sequence, Base Caller Name, Base Caller Version, Flow, Cell Barcode, Fragment Maximum Length, Fragment Mean Length, Fragment Minimum Length, Fragment Standard Deviation Length, Lane Number, Library Strand, Multiplex Barcode, Size Selection Range, Target Depth, To Trim Adapter Sequence, Transcript Integrity Number, RIN, DV200, Adapter Content, Basic Statistics, Encoding, Kmer Content, Overrepresented Sequences, Per Base N Content, Per Base Sequence Content, Per Base Sequence Quality, Per Sequence GC Content, Per Sequence Quality Score, Per Tile Sequence Quality, Percent GC Content, Sequence Duplication Levels, Sequence Length Distribution, Total Reads, QC Workflow Type, QC Workflow Version, QC Workflow Link, Test Attribute",,FALSE,Sequencing,,http://www.ebi.ac.uk/efo/EFO_0003738,
mockComponent,mockComponent to enable config_schema.py,,,,FALSE,mock,Bulk RNA-seq Level 1,,
```

```         
python3 .github/config_schema.py \
  -jd data-models/example.model.jsonld \
  -schema 'Sage-Bionetworks/data-models' \
  -service Sage-Bionetworks/schematic' \
  --overwrite
```

If necesary, delete the mock component record from `config.json`.

## Authentication

This utilizes a Synapse Authentication (OAuth) client (code motivated by [ShinyOAuthExample](https://github.com/brucehoff/ShinyOAuthExample) and [app.R](https://gist.github.com/jcheng5/44bd750764713b5a1df7d9daf5538aea). Each application is required to have its own OAuth client as these clients cannot be shared between one another. View instructions [here](https://help.synapse.org/docs/Using-Synapse-as-an-OAuth-Server.2048327904.html) to learn how to request a client. Once you obtain the client, make sure to add the corresponding [environment variables](#configureapp)

## Deployment

To deploy the app to shinyapps.io, please follow the instructions in the [shinyapps_deploy.md](docs/shinyapps_deploy.md).

## Contributors

Main contributors and developers:

-   [Rongrong Chai](https://github.com/rrchai)
-   [Anthony Williams](https://github.com/afwillia)
-   [Milen Nikolov](https://github.com/milen-sage)
-   [Loren Wolfe](https://github.com/lakikowolfe)
-   [Robert Allaway](https://github.com/allaway)
-   [Bruno Grande](https://github.com/BrunoGrandePhD)
-   [Xengie Doan](https://github.com/xdoan)
-   [Sujay Patil](https://github.com/sujaypatil96)

<!-- Links -->
