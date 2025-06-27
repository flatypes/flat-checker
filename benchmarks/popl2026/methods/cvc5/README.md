# Evaluation of CVC5

## 1. Build Docker image

This will take about 10 seconds.

```shell
docker build . -t cvc5:1.3.0
```

## 2. Run evaluation inside Docker container

This should take less than 5 minutes.

```shell
docker run --entrypoint /benchmark/methods/cvc5/run_eval.sh -v $(realpath ../../):/benchmark cvc5:1.3.0
```