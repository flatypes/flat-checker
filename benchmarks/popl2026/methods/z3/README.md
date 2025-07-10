# Evaluation of Z3

## 1. Build the Docker container

This will take about 10 minutes.

```shell
docker build . -t z3:4.15.1
```

## 2. Run the evaluation

This takes about 2 hours. For a quicker run, you can change the `TIMEOUT_SECONDS` variable in `run_eval.sh` from 60 seconds to 3 seconds, in which case this should take less than 5 minutes (but might show more timeouts).

```shell
docker run --entrypoint /benchmark/methods/z3/run_eval.sh -v $(realpath ../../):/benchmark z3:4.15.1
```
