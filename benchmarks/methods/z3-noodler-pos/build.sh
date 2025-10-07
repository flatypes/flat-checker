#!/bin/bash

# Download the Z3-NOODLER-POS artifact from Zenodo.
if [ -e pldi-artifact.zip ]
then
  echo "Downloaded: pldi-artifact.zip"
else
  echo "Downloading https://zenodo.org/records/15230216/files/pldi-artifact.zip ..."
  curl -o pldi-artifact.zip https://zenodo.org/records/15230216/files/pldi-artifact.zip
fi

# Unzip and build Docker image.
if [ -e pldi-artifact/ ]
then
  echo "Unarchived: pldi-artifact"
else
  echo "Unarchiving: pldi-artifact.zip ..."
  unzip -q pldi-artifact.zip -d pldi-artifact
fi
cd pldi-artifact && docker build -t z3-noodler-pos:eval -f artifact.dockerfile .