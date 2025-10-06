#!/bin/bash

for benchmark in "panini" "panini-neg"; do
  echo "---> Benchmark: $benchmark"
  for method in "flat-checker" "ostrich2" "z3-noodler-pos" "cvc5" "z3"; do
    echo "--> Method: $method"
    rm -rf "methods/$method/results-$benchmark"
    docker run -v .:/benchmarks --entrypoint "/benchmarks/methods/$method/run.sh" "$method:eval" $benchmark
  done
done
