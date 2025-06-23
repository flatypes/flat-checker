# Evaluation of FLAT checker

## 1. Build docker image

First, if you have not already done so, you need to build the Docker container for the FLAT Checker. This should take about 10 minutes.

```shell
docker build -f ../../../../Dockerfile -t flat-checker:dev ../../../../
```

## 2. Run evaluation inside Docker container

Now run the evaluation script inside the Docker container. This should take 5-10 minutes. Afterwards, this directory (outside the Docker container) will contain a `results` folder with the output of the benchmark run.

```shell
docker run -v $(realpath ../../):/benchmark --entrypoint /benchmark/methods/flat-checker/run_eval.sh flat-checker:dev
```
