# Evaluation of FLATChecker

This directory contains all materials necessary to reproduce the evaluation from our OOPSLA'26 paper.

All evaluations are conducted in the Docker containers. So make sure you have the latest `docker` installed first.

## Building FLATChecker

In the project root folder:

```shell
docker build -t flat-checker:eval .
```

## Building SMT Solvers

In the `benchmarks/` folder:

```shell
./build_all.sh
```

## Running Benchmarks

Run FLAT-Checker and SMT solvers, one subject (program/SMT query) at a time:

```shell
./run_all.sh
```

Collect the metrics:

```shell
python3 metrics.py
python3 smt_comparison.py
```

Results are saved as CSV files in `results/`:

- `metrics-panini.csv`: Table 2 in paper.
- `smt-comparison-summary-*.csv`: Table 3 in paper.
- `smt-comparison-validity-panini.csv`: The full comparison of the baseline results. Table 4 is selected from this
  table.

The folder `results_paper` contains the results of the paper run.

Run FLAT-Checker in **batch mode**:

```shell
docker run -v .:/benchmarks --entrypoint /benchmarks/methods/flat-checker/run_batch.sh flat-checker:eval panini
```

## References

* Michael Schröder and Jürgen Cito. 2025. Static Inference of Regular Grammars for Ad Hoc Parsers. OOPSLA 2025.
