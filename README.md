# TELEMAC-MASCARET Conda Package

![TELEMAC](http://www.opentelemac.org/media/kunena/attachments/75/openTELEMAC_hr.png)

[![Current version](https://anaconda.org/nicogodet/telemac-mascaret/badges/version.svg)](https://anaconda.org/nicogodet/telemac-mascaret/badges/version.svg)
[![Platforms](https://anaconda.org/nicogodet/telemac-mascaret/badges/platforms.svg)](https://anaconda.org/nicogodet/telemac-mascaret/badges/platforms.svg)

## Introduction

The [TELEMAC-MASCARET solver suite](http://www.opentelemac.org) is a high-performance Fortran based application for a range
of free-surface flow problems.

This software is a very powerful toolkit for numeric simulation that can be slightly cumbersome to install depending on the
host operating system. The runtime might also need some tweaks to the configuration provided in the upstream repository to
reflect Fortran compiler version and library locations.

Currently, only win-64 package is provided.

## How to install?

On Windows, it is recommended to use [Miniforge3](https://github.com/conda-forge/miniforge).

1. [Download](https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Windows-x86_64.exe) and install miniforge3

2. In Miniforge Prompt:

   ```console
   mamba create -n telemac-mascaret -c nicogodet telemac-mascaret
   mamba activate telemac-mascaret
   ```

3. Depending on your `.py` file association settings, it is recommended to run TELEMAC module using:

   ```console
   python -m module argument
   ```

   Exemple:

   ```console
   python -m telemac2d --ncsize=4 -s cas_file.cas
   ```

## Current implemented features

- Static build
- Parallel computing using `msmpi` (from [conda-forge](https://anaconda.org/conda-forge/msmpi)) and `msmpi-compilers` for `mpif90` command (from [nicogodet anaconda repo](https://anaconda.org/nicogodet/msmpi-compilers))
- Command switch to use debug build `telemac-debug [on/off]`

## TO-DO

- Dynamic build
- HDF5 and MED support
- MUMPS (openblas, scalapack) support
- aed2 support
- gotm support
- Provide `telemac-mascaret-examples` package
- Provide `telemac-mascaret-documentation` package
