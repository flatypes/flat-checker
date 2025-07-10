# POPL 2026 Evaluation

This directory contains all materials necessary to reproduce the evaluation from our POPL'26 paper.

## Evaluation

1. Go into the subdirectory of each method and follow the instructions in the respective `README.md`. You should end up with a `methods/*/results/` folder for each method.

2. Run `python3 aggregate_results.py` to produce `results/results_by_method.csv` (the basis for Table 3 in the paper).

3. Run `python3 compare_results.py` to produce `results/comparison.csv`. You can use this table to easily find differences in solver output. For example, to find the 8 cases where Z3-NOODLER-POS exhibits unsoundness, look for entries reporting `sat` (the expected outcome for all VCs is `unsat`).

> The `results_paper` and `methods/*/results_paper/` folders contain archived versions of the results as they appear in our paper.

## Subjects

### Panini benchmark dataset

The `subjects` folder contains 204 ad hoc parser programs written in Python, taken from the Panini benchmark dataset (Schröder and Cito, 2025). The versions contained here have been slightly modified to add an input type annotation and, where necessary, additional invariant annotations.

For each Python subject, there is a correspondingly named subdirectory containing a number of `.smt2` goal files, which are SMT constraints extracted from the Python programs whose compound validity entails a successfully type-checked program. The constraints where extracted using `flat-checker`'s `--extract-only` option (see `methods/flat-checker/prepare_smt.sh`; you do not need to run this script, its output is already part of this archive).

## References

* Michael Schröder and Jürgen Cito. 2025. Static Inference of Regular Grammars for Ad Hoc Parsers. OOPSLA 2025.
