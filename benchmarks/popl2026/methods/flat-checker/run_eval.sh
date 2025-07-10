#!/bin/bash

# NOTE: this is intended to be run from within the flat-checker docker container
# see README for more information

subjects_dir="/benchmark/subjects"
results_dir="/benchmark/methods/flat-checker/results"
results_table="$results_dir/results.csv"
results_table_vc="$results_dir/results_vc.csv"

mkdir -p $results_dir

echo "subject,time_ms,status,success" | tee $results_table
echo "subject,goal,time_ms,success" > $results_table_vc

for file in "$subjects_dir"/*.py; do
  filename=$(basename -- "$file")
  subject="${filename%.*}"
  outfile="$results_dir/$subject.out"
  statfile="$results_dir/$subject.json"

  echo -n "$subject," | tee -a $results_table
  
  start_time=$(date +%s%3N)
  ./flat-checker "$file" --stat "$statfile" > $outfile 2>&1
  exit_code=$?
  end_time=$(date +%s%3N)
  elapsed_time=$((end_time - start_time))

  if [ -f "$statfile" ]; then
    success=$(jq -r '.[0].success' "$statfile")    
    jq --arg subject $subject -r '(.[0].goals // [])[] | [$subject] + [."goal", ."time (ms)", ."success"] | "\(.[0]),\(.[1:] | @csv)"' "$statfile" >> "$results_table_vc"  
  else
    success="false"
  fi

  echo "$elapsed_time,$exit_code,$success" | tee -a $results_table

done
