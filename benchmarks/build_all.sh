#!/bin/bash

for method in "ostrich2" "z3-noodler-pos" "cvc5" "z3"; do
  echo "--> Building: $method"
  cd "methods/$method" && ./build.sh && cd ..
done