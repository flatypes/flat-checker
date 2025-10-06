#!/bin/bash

# NOTE: this is intended to be run from within the z3-noodler-pos docker container
# see README for more information

TIMEOUT_SECONDS=60

subjects_dir="/benchmarks/$1/smt"
results_dir="/benchmarks/methods/z3-noodler-pos/results-$1"
results_table="$results_dir/results-$1.csv"

mkdir -p $results_dir

echo "subject,goal,time_ms,status,result" | tee $results_table

find "$subjects_dir" -mindepth 2 -maxdepth 2 -type f -name "*.smt2" | sort -n | while read -r filepath; do
  filename=$(basename "$filepath")
  subject=$(basename "$(dirname "$filepath")")  
  goal="${filename%%.smt2}"
  outfile="$results_dir/$subject.$goal.out"

  echo -n "$subject,$goal," | tee -a $results_table

  start_time=$(date +%s%3N)
  /pldi-artifact/smt-bench/bin/z3-noodler/notcont-build/z3 smt.string_solver="noodler" smt.str.ca_constr=true -smt2 -T:$TIMEOUT_SECONDS "$filepath" > $outfile 2>&1
  exit_code=$?
  end_time=$(date +%s%3N)
  elapsed_time=$((end_time - start_time))

  echo -n "$elapsed_time,$exit_code," | tee -a $results_table

  if grep -q "^sat$" "$outfile"; then
    result="invalid"
  elif grep -q "^unsat$" "$outfile"; then
    result="valid"
  elif grep -q "^timeout$" "$outfile"; then
    result="timeout"
  else
    result="error"
  fi

  echo "$result" | tee -a $results_table

done