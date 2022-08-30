## Schema Configuration

In the app, the required schemas is configured in the file `www/config.json`, which is used to adapt the dropdown data types list in the app. Below are the steps to generate `config.json` locally using an example data model. Please remember to replace the example repo with your own data model's repo. 

1.  Create a repo for your data model using this [template](https://github.com/Sage-Bionetworks/data-models)

2.  Clone your data model repo, i.e:

    git clone https://github.com/Sage-Bionetworks/data-models

3.  Create `config.json` and placed it in the `www` folder. We recommend that you generate the `config.json` via this command:

        python3 .github/generate_config_json.py \
          -jd data-models/example.model.jsonld \
          -schema 'Sage-Bionetworks/data-models' \
          -service Sage-Bionetworks/schematic'

    - `service` and `schema` are GitHub repos (`<repo-owner/repo-name>`, i.e `Sage-Bionetworks/schematics`) and are optional flags. If no values provided, empty string will be set for versions.

4.  All properties the in the `config.json` are listed below:

    - `manifest schemas`: defines the list of schemas displayed under the "Choose a Metadata Template Type:" dropdown in the application.
      - `display_name` : The display name for the dropdown. (e.g. _scRNA-seq Level 1_)
      - `schema_name`: The name of the manifest in the JSON-LD schema (e.g. _ScRNA-seqLevel1_)
      - `type`: The type of manifest, either _file_ or _record_.
    - `service_version`: The version of schematic service (Default is empty string if no value provided)
    - `schema_version`: The version of data model (Default is empty string if no value provided)
