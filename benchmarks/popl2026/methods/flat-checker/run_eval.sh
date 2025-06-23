#!/bin/bash

# NOTE: this is intended to be run from within the flat-checker docker container
# see README for more information

subjects_dir="/benchmark/subjects"
results_dir="/benchmark/methods/flat-checker/results"
results_table="$results_dir/results.csv"

mkdir -p $results_dir

echo "subject,time_ms,status" | tee $results_table

for file in "$subjects_dir"/*.py; do
  filename=$(basename -- "$file")
  subject="${filename%.*}"
  outfile="$results_dir/$subject.out"  

  echo -n "$subject," | tee -a $results_table
  
  start_time=$(date +%s%3N)
  ./flat-checker "$file" > $outfile 2>&1
  exit_code=$?
  end_time=$(date +%s%3N)
  elapsed_time=$((end_time - start_time))

  echo "$elapsed_time,$exit_code" | tee -a $results_table
done