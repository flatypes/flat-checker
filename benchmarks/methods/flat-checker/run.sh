#!/bin/bash

# NOTE: this is intended to be run from within the flat-checker docker container
# see README for more information

subjects_dir="/benchmarks/$1/py"
results_dir="/benchmarks/methods/flat-checker/results-$1"
results_table="$results_dir/results-$1.csv"

mkdir -p $results_dir

echo "subject,goal,time_ms,status,result" | tee $results_table

for file in "$subjects_dir"/*.py; do
  filename=$(basename -- "$file")
  subject="${filename%.*}"
  outfile="$results_dir/$subject.out"

  echo -n "$subject,0," | tee -a $results_table
  
  start_time=$(date +%s%3N)
  ./flat-checker $file > $outfile 2>&1
  exit_code=$?
  end_time=$(date +%s%3N)
  elapsed_time=$((end_time - start_time))

  if grep -q "Type CHECKED$" "$outfile"; then
    result="valid"
  elif grep -q "Type ERROR$" "$outfile"; then
    result="invalid"
  else
    result="error"
  fi

  echo "$elapsed_time,$exit_code,$result" | tee -a $results_table

  jsonfile="$results_dir/$subject.json"
  ./flat-checker --metrics $jsonfile $file
done
