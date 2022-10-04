## Schema Configuration

In the app, the required schemas is configured in the file `www/config.json`, which is used to adapt the dropdown data types list in the app.

All properties in the `config.json` are listed below:

- `manifest schemas`: defines the list of schemas displayed under the "Choose a Metadata Template Type:" dropdown in the application.
  - `display_name` : The display name for the dropdown. (e.g. _scRNA-seq Level 1_)
  - `schema_name`: The name of the manifest in the JSON-LD schema (e.g. _ScRNA-seqLevel1_)
  - `type`: The type of manifest, either _file_ or _record_.
- `service_version`: The version of schematic service (Default is empty string if no value provided)
- `schema_version`: The version of data model (if no specific version or branch provided in the `schematic_config.yml`, default is to query the latest release of the schema repo)

### Workflow Deployment

During the app deployment, the workflow will perform following steps to automatically generate the `config.json` file:

1. Clone the data model repo defined in the `repo` configuration in the `schematic_config.yml` file (if you would like to continue using `download_url`, please remove or comment out the `repo` key).

2. Run `create_schema.py` script to automatically generate the `config.json` file (filled `DependsOn Component` column of your data model is required)

## Local Development

For the local developers, the `config.json` will be automatically generated after the app launches via below command in `global.R`. You can always manually modify the `config.json` or replace with your own existing `config.json` in the `www` folder.

```python
python3 .github/config_schema.py \
  -c schematic_config.yml \
  --service_repo 'Sage-Bionetworks/schematic' \
  --overwrite
```

- `-c` is the path to the schematic configuration file
- `--service_repo` is the repo path for schematic, with the format as `<repo-owner>/<repo-name>`
- `--overwrite` is to set whether to overwrite the existing `config.json` file (please remove this flag if you would like to use your own `config.json`)
