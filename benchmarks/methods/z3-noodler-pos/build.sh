#!/bin/bash

# Download the Z3-NOODLER-POS artifact from Zenodo.
if [ -e artifact.zip ]
then
  echo "Downloaded: artifact.zip"
else
  echo "Downloading https://zenodo.org/records/15230216/files/pldi-artifact.zip ..."
  curl -s -o artifact.zip https://zenodo.org/records/15230216/files/pldi-artifact.zip
fi

# Unzip and build Docker image.
if [ -e artifact/ ]
then
  echo "Unarchived: artifact"
else
  echo "Unarchiving: artifact.zip ..."
  unzip -q artifact.zip -d artifact
fi
cd artifact && docker build -t z3-noodler-pos:eval -f artifact.dockerfile .