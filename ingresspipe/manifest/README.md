## Usage of method(s) in `ingresspipe.manifest.generator` module

An important method in the `manifest.generator` module is the `get_manifest()` method which takes care of generating the manifest link, based on the underlying `JSON-LD schema` (in this case, `HTAN.jsonld`) and an optionally provided `JSON schema`.

_Note: Make sure to fill out the `"username"` and `"password"` key/value pairs in the config file._

First, we need to make sure the google API credentials file (which is required to interact with google services, in this case google docs), is present in the root folder:

```python
try:
    download_creds_file()
except synapseclient.core.exceptions.SynapseHTTPError:
    print("Make sure the credentials set in the config file are correct.")
```

Create an object of `ManifestGenerator`, and feed the path to the master schema (JSON-LD). In addition, also change the name of the root node (component) based on the custom template type of your choice:

```python
PATH_TO_JSONLD = "./data/schema_org_schemas/HTAN.jsonld"

# create an instance of ManifestGenerator class
TEST_NODE = "FollowUp"
manifest_generator = ManifestGenerator(title="Demo Manifest", path_to_json_ld=PATH_TO_JSONLD, root=TEST_NODE)
```

Get the link to a manifest/csv (google spreadsheet) based on the `manifest_generator` object created above. You can optionally also provide a JSON schema as parameter to the `get_manifest` method:

```python
print(manifest_generator.get_manifest())
```

_Note: Not providing any value for the `root` argument will produce a general manifest file (not specific to any component)._