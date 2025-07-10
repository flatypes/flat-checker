# Evaluation of FLAT-Checker

## 1. Build docker image

First, if you have not already done so, you need to build the Docker container for FLAT-Checker. This should take about 10 minutes.

```shell
docker build -f ../../../../Dockerfile -t flat-checker:dev ../../../../
```

## 2. Run evaluation inside Docker container

Now run the evaluation script inside the Docker container. This should take 5-10 minutes. Afterwards, this directory (outside the Docker container) will contain a `results` folder with the output of the benchmark run.

```shell
docker run -v $(realpath ../../):/benchmark --entrypoint /benchmark/methods/flat-checker/run_eval.sh flat-checker:dev
```

## 3. Compute aggregated results

You can now compute results aggregated by program category. This brings together the following information:

- type checking results and performance from `results/results.csv`
- goal/VC discharge information from each subject's `results/*.json` statistics file
- program categories from the original Panini benchmark dataset (found in `subjects/categories.csv`)
- information about invariant annotations (from `invariants.csv`, pre-computed using `count_inv.sh`)

```shell
python3 compute_tables.py
```

You should now have a `results/results_by_category.csv`, which is the basis for Table 2 in the paper.

> The `results_paper/` folder contains an archived version of the results as they appear in our paper.
