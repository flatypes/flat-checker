# Evaluation of Z3

## 1. Build the Docker container

This will take about 10 minutes.

```shell
docker build . -t z3:4.15.1
```

## 2. Run the evaluation

```shell
docker run --entrypoint /benchmark/methods/z3/run_eval.sh -v $(realpath ../../):/benchmark z3:4.15.1
```
