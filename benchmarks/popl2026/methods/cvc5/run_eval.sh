#!/bin/bash

# NOTE: this is intended to be run from within the cvc5 docker container
# see README for more information

TIMEOUT_MILLISECONDS=60000

subjects_dir="/benchmark/subjects"
results_dir="/benchmark/methods/cvc5/results"
results_table="$results_dir/results.csv"

mkdir -p $results_dir

echo "subject,goal,time_ms,status,result" | tee $results_table

exec 2> error.log

find "$subjects_dir" -mindepth 2 -maxdepth 2 -type f -name "*.smt2" | sort -n | while read -r filepath; do
  filename=$(basename "$filepath")
  subject=$(basename "$(dirname "$filepath")")  
  goal="${filename%%.smt2}"
  outfile="$results_dir/$subject.$goal.out"

  echo -n "$subject,$goal," | tee -a $results_table

  start_time=$(date +%s%3N)
  (cvc5 --tlimit=$TIMEOUT_MILLISECONDS "$filepath" &> $outfile) 2>/dev/null
  exit_code=$?
  end_time=$(date +%s%3N)
  elapsed_time=$((end_time - start_time))

  echo -n "$elapsed_time,$exit_code," | tee -a $results_table

  if grep -q "^sat$" "$outfile"; then
    result="sat"
  elif grep -q "^unsat$" "$outfile"; then
    result="unsat"
  elif grep -q "^cvc5 interrupted by timeout.$" "$outfile"; then
    result="timeout"
  else
    result="error"
  fi

  echo "$result" | tee -a $results_table

done