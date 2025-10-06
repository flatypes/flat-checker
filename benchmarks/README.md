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

```shell
./run_all.sh
python3 metrics.py
python3 smt_comparison.py
```