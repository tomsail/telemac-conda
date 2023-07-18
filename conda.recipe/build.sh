#!/usr/bin/env bash

set -xeuo pipefail

# build and install telemac
source ./configs/pysource.template.sh
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
cp -r $HOMETEL/scripts/python3/* $PREFIX/bin #3
cp -r $HOMETEL/scripts/* $PREFIX/scripts #3
cp -r $HOMETEL/sources/* $PREFIX/sources #4

cd $BUILD_PREFIX