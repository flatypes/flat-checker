#!/usr/bin/env python
import ast
import json
from ast2json import ast2json
import argparse
import os

counter = 0

def deserialize(source: str) -> None:
    with open(source, 'r') as f:
        code = f.read()
        tree = ast.parse(code)
        obj = ast2json(tree)
        obj['_source'] = os.path.abspath(source)
        pretty = json.dumps(obj, indent=2)
        global counter
        counter += 1
        with open(f'out/{counter}.py.json', 'w') as fw:
            fw.write(pretty)

parser = argparse.ArgumentParser(description='Deserialize Python sources to JSON')  
parser.add_argument('inputs', metavar='file_or_dir', nargs='+', type=str)

args = parser.parse_args()
for path in args.inputs:
    if os.path.isdir(path):
        for filename in os.listdir(path):
            if filename.endswith('.py'):
                deserialize(os.path.join(path, filename))
    elif os.path.isfile(path):
        filename = os.path.basename(path)
        if filename.endswith('.py'):
            deserialize(path)
    else:
        raise FileNotFoundError(path)