#!/usr/bin/env bash

# TELEMAC home directory
export HOMETEL=$SRC_DIR/telemac-mascaret
# Copy sources

#6 the documentation folder
export DOCS=$PREFIX/documentation/
if [ -d "$DOCS" -a ! -h "$DOCS" ]
then
   echo "$DOCS already exists"
else
   mkdir $DOCS
fi

cp -r $HOMETEL/documentation/* $PREFIX/documentation     #6