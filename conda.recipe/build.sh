#!/usr/bin/env bash

set -xeuo pipefail

#export mpi=mpich
export CC=mpicc
export CXX=mpicxx
export FC=mpif90
export F77=mpif77
export F90=mpif90

# build and install schism
ls
ls ..
source ./configs/pysource.template.sh
compile_telemac.py

cp -r $HOMETEL/builds/$USETELCFG/lib/* $PREFIX/bin
mkdir $PREFIX/$USETELCFG/
mkdir $PREFIX/$USETELCFG/wrap_api/
mkdir $PREFIX/$USETELCFG/wrap_api/lib/
cp -r $HOMETEL/builds/$USETELCFG/wrap_api/lib/* $PREFIX/$USETELCFG/wrap_api/lib/

cd $BUILD_PREFIX