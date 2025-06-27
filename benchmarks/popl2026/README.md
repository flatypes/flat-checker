# POPL 2026 Evaluation

This directory contains all materials necessary to reproduce the evaluation from our POPL'26 paper.

## Evaluation

1. Go into the subdirectory of each method and follow the instructions in the respective `README.md`. You should end up with a `methods/*/results/` folder for each method.

> The `methods/*/results_paper/` folders contain archived versions of the results as they appear in our paper.

## Subjects

### Panini benchmark dataset

The `subjects` folder contains 205 ad hoc parser programs written in Python. taken from the Panini benchmark dataset (Schröder and Cito, 2025). The versions contained here have been slightly modified to add an input type annotation and, where necessary, additional invariant annotations.

For each Python subject, there is a correspondingly named subdirectory containing a number of `.smt2` files, which are SMT constraints extracted from the Python programs whose compound validity entails a successfully type-checked program. The constraints where extracted using `flat-checker`'s `--extract-only` option (see `methods/flat-checker/prepare_smt.sh`).

## References

* Michael Schröder and Jürgen Cito. 2025. Static Inference of Regular Grammars for Ad Hoc Parsers. OOPSLA 2025.
