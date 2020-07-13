## Usage of methods in `ingresspipe.models.metadata` module

_Note: Make sure to look at the config file in `ingresspipe.config.config` to configure the `model` parameters used here._

Create an instance of the `MetadataModel` class:
```python
MM_LOC = model["input"]["model_location"]   # location of HTAN data model (JSON-LD)
MM_TYPE = model["input"]["model_file_type"] # type of file -- "local"

metadata_model_htan = MetadataModel(MM_LOC, MM_TYPE)
```

You need to make sure you have a copy of the `credentials.json` file that is required by our API when accessing google services
Execute the following snippet to download a copy of the credentials file and store in your root directory:

_Note: Make sure to fill out values for your Synapse username and password credentials in the config file before executing the below snippet._

```python
API_CREDS = storage["Synapse"]["api_creds"]

SYN_USERNAME = storage["Synapse"]["username"]
SYN_PASSWORD = storage["Synapse"]["password"]

# try downloading 'credentials.json' file (if not present already)
if not os.path.exists("./credentials.json"):
    
    print("Retrieving Google API credentials from Synapse..")
    import synapseclient

    syn = synapseclient.Synapse()
    syn.login(SYN_USERNAME, SYN_PASSWORD, rememberMe=False)
    syn.get(API_CREDS, downloadLocation = "./")
    print("Stored Google API credentials.")

print("Google API credentials successfully located..")
```

Generate a manifest based on a specified schema.org schema (`HTAN.jsonld`) but no optionally provided JSON validation schema:

```python
print("Testing manifest generation based on a provided schema.org schema..")
manifest_url = metadata_model_htan.getModelManifest(title="Test_" + TEST_COMP, rootNode=TEST_COMP, filenames=["1.txt", "2.txt", "3.txt"])
print(manifest_url)
```

Click on the manifest url that is generated (link to google sheet/csv file). You can fill in the google sheet and download the file as a CSV to use with the _Data Curator App_.

Generate a manifest based on a schema.org schema, which in this case is specified in `HTAPP.jsonld` (a schema from biothings), and an additionally provided JSON validation schema:

```python
print("Testing manifest generation based on an additionally provided JSON schema..")
HTAPP_VALIDATION_SCHEMA = model["demo"]["htapp_validation_file_location"]

with open(HTAPP_VALIDATION_SCHEMA, "r") as f:
    json_schema = json.load(f)

HTAPP_SCHEMA = model["demo"]["htapp_location"]
HTAPP_SCHEMA_TYPE = model["demo"]["htapp_file_type"]

metadata_model_htapp = MetadataModel(HTAPP_SCHEMA, HTAPP_SCHEMA_TYPE)
manifest_url = metadata_model_htapp.getModelManifest(title="HTAPP Manifest", rootNode="", jsonSchema=json_schema, filenames=["1.txt", "2.txt", "3.txt"])
print(manifest_url)
```

Validate the content (_annotations_) filled out in the manifest file (_and downloaded as csv_) to check if it complies with the underlying data model/schema (which in this case is the HTAPP schema):

Case 1: Without additionally provided JSON validation schema:

```python
print("Testing metadata model-based validation..")

MANIFEST_PATH = model["demo"]["valid_manifest"]
print("Testing validation with jsonSchema generation from schema.org schema..")
annotation_errors = metadata_model_htan.validateModelManifest(MANIFEST_PATH, TEST_COMP)
print(annotation_errors)
```

Case 2: With additionally provided JSON validation schema:

```python
print("Testing validation with provided JSON schema..")
annotation_errors = metadata_model_htapp.validateModelManifest(MANIFEST_PATH, TEST_COMP, json_schema)
print(annotation_errors)
```

Getting all nodes that are dependent (based on the `requiresDependency` relationship) on a particular/given node:

```python
print("Testing metadata model-based object dependency generation..")
print("Generating dependency graph and ordering dependencies..")
dependencies = metadata_model_htan.getOrderedModelNodes(TEST_COMP, "requiresDependency")
print(dependencies)

with open(TEST_COMP + "_dependencies.json", "w") as f:
    json.dump(dependencies, f, indent = 3)

print("Dependencies stored in: " + TEST_COMP + "_dependencies.json")
```

Getting all nodes that are dependent (based on the `requiresComponent` relationship) on a particular/given component:

```python
print("Testing metadata model-based component dependency generation..")
print("Generating dependency graph and ordering dependencies..")

dependencies = metadata_model_htan.getOrderedModelNodes(TEST_COMP, "requiresComponent")
print(dependencies)

with open(TEST_COMP + "component_dependencies.json", "w") as f:
    json.dump(dependencies, f, indent = 3)

print("component dependencies stored: " + TEST_COMP + "component_dependencies.json")
```