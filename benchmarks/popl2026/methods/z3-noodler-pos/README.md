# Evaluation of Z3-NOODLER-POS

## 1. Download the artifact

Download the artifact from <https://doi.org/10.5281/zenodo.15230216> (about 2.2 GB).

```shell
curl -o pldi-artifact.zip https://zenodo.org/records/15230216/files/pldi-artifact.zip
```

To ensure the integrity of the download you can compare its MD5 checksum with the reference `4961b33ce02accfe1ce01e14ec1cf0e5`.

## 2. Unzip the artifact and load the pre-built Docker container

```shell
unzip pldi-artifact.zip -d artifact
cd artifact
docker load -i position-constraints-artifact.tar
```

## 3. Run the evaluation

This should take about 2 minutes.

```shell
docker run --entrypoint /benchmark/methods/z3-noodler-pos/run_eval.sh -v $(realpath ../../../):/benchmark pldi-artifact:latest
```
