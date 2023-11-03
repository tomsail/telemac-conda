#!/usr/bin/env bash

# TELEMAC home directory
export HOMETEL=$SRC_DIR/opentelemac
# Copy sources

#5 the examples and notebooks folder
export EXAMPLES=$PREFIX/examples/
if [ -d "$EXAMPLES" -a ! -h "$EXAMPLES" ]
then
   echo "$EXAMPLES already exists"
else
   mkdir $EXAMPLES
fi

export NOTEBOOKS=$PREFIX/notebooks/
if [ -d "$NOTEBOOKS" -a ! -h "$NOTEBOOKS" ]
then
   echo "$NOTEBOOKS already exists"
else
   mkdir $NOTEBOOKS
fi

cp -r $HOMETEL/examples/* $PREFIX/examples     #5
cp -r $HOMETEL/notebooks/* $PREFIX/notebooks   #5