# Evaluation of CVC5

## 1. Build Docker image

This will take about 10 seconds.

```shell
docker build . -t cvc5:1.3.0
```

## 2. Run evaluation inside Docker container

This takes about 1 hour. For a quicker run, you can change the `TIMEOUT_MILLISECONDS` variable in `run_eval.sh` from 60000 milliseconds to 3000 milliseconds, in which case this should take less than 5 minutes (but might show more timeouts).

```shell
docker run --entrypoint /benchmark/methods/cvc5/run_eval.sh -v $(realpath ../../):/benchmark cvc5:1.3.0
```
