#!/usr/bin/env bash

set -xeuo pipefail

export CC=mpicc
export CXX=mpicxx
export FC=mpif90
export F77=mpif77
export F90=mpif90

source ./configs/pysource.template.sh
# 
compile_telemac.py

#1: the config folder 
export CONFIGS=$PREFIX/configs/
if [ -d "$CONFIGS" -a ! -h "$CONFIGS" ]
then
   echo "$CONFIGS already exists"
else
   mkdir $CONFIGS
fi 

#2: the builds folder 
export BUILDS=$PREFIX/builds/
if [ -d "$BUILDS" -a ! -h "$BUILDS" ]
then
   echo "$BUILDS already exists"
else
   mkdir $BUILDS
fi 

#3 the scripts folder 
export SCRIPTS=$PREFIX/scripts/
if [ -d "$SCRIPTS" -a ! -h "$SCRIPTS" ]
then
   echo "$SCRIPTS already exists"
else
   mkdir $SCRIPTS
fi 

#4 the source folder 
export SOURCES=$PREFIX/sources/
if [ -d "$SOURCES" -a ! -h "$SOURCES" ]
then
   echo "$SOURCES already exists"
else
   mkdir $SOURCES
fi 

cp -r $HOMETEL/configs/* $PREFIX/configs     #1
cp -r $HOMETEL/builds/* $PREFIX/builds       #2
cp -r $HOMETEL/scripts/* $PREFIX/scripts     #3
cp -r $HOMETEL/sources/* $PREFIX/sources     #4

# AUTO activate /deactivate environments variables for TELEMAC
cd $PREFIX
export ACTIVATE=$PREFIX/etc/conda/activate.d
if [ -d "$ACTIVATE" -a ! -h "$ACTIVATE" ]
then
   echo "$ACTIVATE already exists"
else
   mkdir $ACTIVATE
fi 

export DEACTIVATE=$PREFIX/etc/conda/deactivate.d
if [ -d "$DEACTIVATE" -a ! -h "$DEACTIVATE" ]
then
   echo "$DEACTIVATE already exists"
else
   mkdir $DEACTIVATE
fi 
cp $SRC_DIR/conda.recipe/env_var_telemac_activate.sh $PREFIX/etc/conda/activate.d/env_var_telemac_activate.sh
cp $SRC_DIR/conda.recipe/env_var_telemac_deactivate.sh $PREFIX/etc/conda/deactivate.d/env_var_telemac_deactivate.sh

cd $BUILD_PREFIX