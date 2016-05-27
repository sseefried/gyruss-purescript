#!/bin/bash

echo "Bundling...this may take a few minutes"
time psc-bundle output/*/{index,foreign}.js --module Gyruss.Main \
  -o html/gyruss.js