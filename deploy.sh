#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

[ "$SEANSEEFRIED_DOT_ORG_MACHINE" != "" ] || { echo "Set SEANSEEFRIED_DOT_ORG SSH path to remote directory"; exit 1; }
[ "$SEANSEEFRIED_DOT_ORG_PATH" != "" ] || { echo "Set SEANSEEFRIED_DOT_ORG SSH path to remote directory"; exit 1; }

SEANSEEFRIED_DOT_ORG="$SEANSEEFRIED_DOT_ORG_MACHINE:$SEANSEEFRIED_DOT_ORG_PATH"

ssh "$SEANSEEFRIED_DOT_ORG_MACHINE" mkdir -p $SEANSEEFRIED_DOT_ORG_PATH/games/gyruss-ps

rsync -avz html/* "$SEANSEEFRIED_DOT_ORG/games/gyruss-ps"