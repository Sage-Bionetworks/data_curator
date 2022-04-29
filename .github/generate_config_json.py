
import argparse
import json
import re
from functools import reduce
from schematic.schemas.generator import SchemaGenerator


def get_args():
    """Set up command-line interface and get arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--jsonld_path',
                        required=True, help='path to model jsonld file')
    parser.add_argument('-v1', '--service_version',
                        default='', help='version of schematic')
    parser.add_argument('-v2', '--schema_version',
                        default='', help='version of data model')
    parser.add_argument('-o', '--out_dir',
                        default='.', help='directory to save result')
    return parser.parse_args()


def unCamel(x):
    """Split camel-case string by ' ' delimiter."""
    new = reduce(lambda a, b: a + (
        (b.upper() == b and (len(a) and a[-1].upper() != a[-1]))
        and (' ' + b) or b), x, '')
    return new


def main():
    args = get_args()
    schemas = []

    # get all required data types from data model jsonld
    sg = SchemaGenerator(path_to_json_ld=args.jsonld_path)
    component_digraph = sg.se.get_digraph_by_edge_type('requiresComponent')
    components = component_digraph.nodes()

    # save display_name, schema_name, assay type to list
    for schema in components:
        cp = any(re.findall(r'clinical|biospecimen', schema, re.IGNORECASE))
        assay_type = 'record' if cp else 'file'
        schemas.append({
            'display_name': unCamel(schema),
            'schema_name': schema,
            'type': assay_type
        })

    # write out the config.json including some versions
    config = {'manifest_schemas': schemas,
              'service_version': args.service_version,
              'schema_version': args.schema_version
              }
    with open(f'{args.out_dir}/config.json', 'w') as o:
        o.write(json.dumps(config, indent=2, separators=(',', ': ')))


if __name__ == '__main__':
    main()
