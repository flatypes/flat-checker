# Evaluation

## Building FLAT-Checker

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
python3 metrics.py
python3 smt_comparison.py
```

Run FLAT-Checker in **batch mode**:

```shell
docker run -v .:/benchmarks --entrypoint /benchmarks/methods/flat-checker/run_batch.sh flat-checker:eval panini
```