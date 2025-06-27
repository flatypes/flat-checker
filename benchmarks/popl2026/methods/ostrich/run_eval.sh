#!/bin/bash

subjects_dir="../..//subjects"
results_dir="results"
results_table="$results_dir/results.csv"

mkdir -p $results_dir

echo "subject,goal,time_sec,status,result" | tee $results_table

find "$subjects_dir" -mindepth 2 -maxdepth 2 -type f -name "*.smt2" | sort -n | while read -r filepath; do
  filename=$(basename "$filepath")
  subject=$(basename "$(dirname "$filepath")")  
  goal="${filename%%.smt2}"
  outfile="$results_dir/$subject.$goal.out"

  echo -n "$subject,$goal," | tee -a $results_table

  tmp=$(mktemp)
  sed -e 's/str\.to_re/str.to.re/g' -e 's/str\.in_re/str.in.re/g' "$filepath" > "$tmp"

  #start_time=$(date +%s%3N)
  start_time=$(date +%s)
  java -jar ostrich-popl19-artifact/ostrich-popl2019.jar "$tmp" > $outfile 2>&1
  exit_code=$?
  #end_time=$(date +%s%3N)
  end_time=$(date +%s)
  elapsed_time=$((end_time - start_time))

  rm "$tmp"

  echo -n "$elapsed_time,$exit_code," | tee -a $results_table

  if grep -q "^sat$" "$outfile"; then
    result="sat"
  elif grep -q "^unsat$" "$outfile"; then
    result="unsat"
  elif grep -q "^timeout$" "$outfile"; then
    result="timeout"
  else
    result="error"
  fi

  echo "$result" | tee -a $results_table

done