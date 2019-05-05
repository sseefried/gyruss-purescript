#!/bin/bash

[ "$SEANSEEFRIED_DOT_ORG" != "" ] || { echo "Set SEANSEEFRIED_DOT_ORG SSH path to remote directory"; exit 1; } 

rsync -avz html/* "$SEANSEEFRIED_DOT_ORG/games/gyruss-ps"