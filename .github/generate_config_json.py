import argparse
import json
import re
import requests
from schematic.schemas.generator import SchemaGenerator


def get_args():
    """Set up command-line interface and get arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument('-jd', '--jsonld_path',
                        required=True, help='path to model jsonld file')
    parser.add_argument('-service','--service_repo',
                        default='', help='GH repo of schematic')
    parser.add_argument('-schema','--schema_repo',
                        default='', help='GH repo of data model')
    parser.add_argument('--out_dir',
                        default='www', help='directory to save result')
    return parser.parse_args()


def _get_version(repo_name):
    """Get the latest release version of github repo, otherwise return empty string"""
    response = requests.get(
        f'https://api.github.com/repos/{repo_name}/releases/latest')
    if response.status_code == 200:
        return response.json()["tag_name"]
    else:
        return ''


def main():
    args = get_args()
    schemas = []

    # get all required data types from data model jsonld
    sg = SchemaGenerator(path_to_json_ld=args.jsonld_path)
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

    service_version = _get_version(args.service_repo)
    schema_version = _get_version(args.schema_repo)

    # write out the config.json including some versions
    config = {'manifest_schemas': schemas,
              'service_version': service_version,
              'schema_version': schema_version
              }
    with open(f'{args.out_dir}/config.json', 'w') as o:
        o.write(json.dumps(config, indent=2, separators=(',', ': ')))


if __name__ == '__main__':
    main()
