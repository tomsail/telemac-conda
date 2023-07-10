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
