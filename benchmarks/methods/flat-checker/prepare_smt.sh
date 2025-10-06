#!/bin/bash

# NOTE: this is intended to be run from within the flat-checker docker container
# see README for more information

subjects_dir="/benchmark/subjects"

for file in "$subjects_dir"/*.py; do
  filename=$(basename -- "$file")
  subject="${filename%.*}"
  echo $subject
  ./flat-checker --extract-only "${subjects_dir}" "$file"
done
