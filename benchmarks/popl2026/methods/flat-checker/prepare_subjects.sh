#!/bin/bash

# NOTE: this is intended to be run from within the Docker container

SUBJECT_ORIG_DIR="/benchmark/subjects_raw"
SUBJECT_DIR="/benchmark/subjects"

mkdir -p "$SUBJECT_DIR"

find "$SUBJECT_ORIG_DIR" -type f -name "*.regex" | sort | while read -r regex_file; do
  regex_filename=$(basename "$regex_file")                  # e.g., XXX.pos.1.regex
  base_name="${regex_filename%.regex}"                      # e.g., XXX.pos.1
  subject_name="${base_name%%.*}"                           # e.g., XXX
  variant="${base_name#"$subject_name."}"                   # e.g., pos.1
  py_file="$SUBJECT_ORIG_DIR/$subject_name.py"
  output_file="$SUBJECT_DIR/$subject_name.$variant.py"

  regex_content=$(<"$regex_file")

  {
    echo "type Input = lang(r'$regex_content')"
    cat "$py_file"
  } > "$output_file"

  ./flat-checker --extract-only "$SUBJECT_DIR" "$output_file"
done
