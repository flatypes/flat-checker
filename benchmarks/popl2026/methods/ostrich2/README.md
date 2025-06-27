# Evaluation of OSTRICH2

## 1. Download the artifact

Download the artifact from <https://zenodo.org/records/15379336> (about 500 MB).

```shell
curl -o ostrich.zip https://zenodo.org/records/15379336/files/ostrich.zip
```

To ensure the integrity of the download you can compare its MD5 checksum with the reference `fe26232516845a0cc5a757e9f5beba3a`.

## 2. Unzip the artifact and build the Docker image

This will take about 5 minutes.

```shell
unzip ostrich.zip
cd ostrich
docker build -t ostrich2-artifact .
```

## 3. Run the evaluation

This might take about half an hour.

```shell
docker run --entrypoint /benchmark/methods/ostrich2/run_eval.sh -v $(realpath ../../../):/benchmark ostrich2-artifact
```
