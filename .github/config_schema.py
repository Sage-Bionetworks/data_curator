import argparse
import json
import os
import re
import requests
import yaml
from schematic.schemas.generator import SchemaGenerator


def get_args():
    """Set up command-line interface and get arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument('-c', '--config_path', metavar="",
                        required=True, help='path to schematic config ymal file')
    parser.add_argument('-sr', '--service_repo', metavar="",
                        help='repo path to service')
    parser.add_argument('-o', '--out_dir', default='www', metavar="",
                        help='directory to save result')
    parser.add_argument('--overwrite', action='store_true', default=False,
                        help='whether to overwrite the existing config.json')
    return parser.parse_args()


def _get_version(repo_name):
    """Get the latest release version of github repo, otherwise return empty string"""
    response = requests.get(
        f'https://api.github.com/repos/{repo_name}/releases/latest')
    if response.status_code == 200:
        return response.json()["tag_name"]
    else:
        return ''


def _is_valid(value, type):
    if type not in ["repo", "location"]:
        raise ValueError('type must be "repo" or "location"')
    pattern = "^([-_.A-z0-9]+\\/){1,2}[-_.A-z0-9]+$" if type == "repo" else "^[-_.A-z0-9]+\\/.*.jsonld$"
    return bool(re.match(pattern, value))


def _parse_schema(config_path):
    """Parse schematic_config.yml file"""
    with open(config_path, "r") as stream:
        try:
            config = yaml.safe_load(stream)["model"]["input"]
            return config
        except yaml.YAMLError as exc:
            print(exc)


def download_schema(config_path):
    """Download data model"""
    config = _parse_schema(config_path)

    # get the location of data model
    if config.get("location") and _is_valid(config.get("location"), "location"):
        location = config["location"]
    else:
        raise ValueError(
            f'No valid "location" value found in "{config_path}" \u274C')

    repo = ''
    version = ''
    # get the repo/url info of data model and download the jsonld
    if config.get("repo") and _is_valid(config.get("repo"), "repo"):
        repo_config = config["repo"].split("/")
        if (len(repo_config) > 2):  # aka version/branch provided
            repo = os.path.join(repo_config[0], repo_config[1])
            version = repo_config[2]
            os.system(
                f'git clone https://github.com/{repo}.git -b {version}  -c advice.detachedHead=false --depth 1')
        else:
            repo = config["repo"]
            os.system(
                f'git clone https://github.com/{repo}.git --depth 1')
    elif config.get('download_url'):  # to let users keep using 'download_url' for now
        url = config.get("download_url")
        os.system(f'mkdir -p {os.path.dirname(location)}')
        os.system(f'wget {url} -O {location}')
    else:
        raise ValueError(
            f'No valid "repo" value found in "{config_path}" \u274C')

    return repo, version, location


def generate_schema_config(schema_path):
    """Generate schema config with corresponding display name"""
    schemas = []
    # get all required data types from data model jsonld
    sg = SchemaGenerator(path_to_json_ld=schema_path)
    component_digraph = sg.se.get_digraph_by_edge_type('requiresComponent')
    components = component_digraph.nodes()
    # get display names for required data types
    mm_graph = sg.se.get_nx_schema()
    display_names = sg.get_nodes_display_names(components, mm_graph)
    # save display_name, schema_name, assay type to list
    for index, component in enumerate(components):
        # get component's dependencies
        deps = sg.get_node_dependencies(component)
        schema_type = 'file' if 'Filename' in deps else 'record'
        schemas.append({
            'display_name': display_names[index],
            'schema_name': component,
            'type': schema_type
        })
    return schemas


def main():
    args = get_args()
    # download schema and retrieve schema info
    s_repo, s_vesion, s_path = download_schema(args.config_path)

    if args.overwrite:
        # get versions for both service and schema
        service_version = _get_version(args.service_repo)
        schema_version = s_vesion if s_vesion else _get_version(s_repo)
        # generate schema configuration based on *.model.jsonld
        schemas_config = generate_schema_config(s_path)
        # write out the config.json including versions
        config = {'manifest_schemas': schemas_config,
                  'service_version': service_version,
                  'schema_version': schema_version
                  }
        with open(f'{args.out_dir}/config.json', 'w') as o:
            o.write(json.dumps(config, indent=2, separators=(',', ': ')))


if __name__ == '__main__':
    main()
