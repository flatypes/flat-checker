#!/bin/bash

# Download the OSTRICH2 artifact from Zenodo.
if [ -e ostrich.zip ]
then
  echo "Downloaded: ostrich.zip"
else
  echo "Downloading https://zenodo.org/records/15379336/files/ostrich.zip ..."
  curl -s -o ostrich.zip https://zenodo.org/records/15379336/files/ostrich.zip
fi

# Unzip and build Docker image.
if [ -e ostrich/ ]
then
  echo "Unarchived: ostrich"
else
  echo "Unarchiving: ostrich.zip ..."
  unzip -q ostrich.zip
fi
cd ostrich && docker build -t ostrich2:eval .