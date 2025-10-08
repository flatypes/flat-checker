#!/bin/bash

# NOTE: this is intended to be run from within the flat-checker docker container
# see README for more information

subjects_dir="/benchmarks/$1/py"

start_time=$(date +%s%3N)
./flat-checker $subjects_dir
exit_code=$?
end_time=$(date +%s%3N)
elapsed_time=$((end_time - start_time))

echo "return code: $exit_code"
echo "$elapsed_time ms"
