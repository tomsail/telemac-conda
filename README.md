openTELEMAC conda package 
=========================
![CI](https://github.com/tomsail/telemac-conda/actions/workflows/test_examples.yml/badge.svg) 
![TELEMAC](http://www.opentelemac.org/media/kunena/attachments/75/openTELEMAC_hr.png)
This is the repository for the conda package of the openTELEMAC system

## Current build status

<table>
    
  <tr>
    <td>Anaconda.org</td>
    <td>
      <details>
        <summary>
          <a href="https://anaconda.org/nicogodet/opentelemac">
            <img src="https://anaconda.org/nicogodet/opentelemac/badges/version.svg">
          </a>
        </summary>
        <table>
          <thead><tr><th>Dependencies</th><th>Version</th><th>Last update</th></tr></thead>
          <tbody><tr>
              <td>m2w64-msmpi</td>
              <td>
                <a href="https://anaconda.org/nicogodet/m2w64-msmpi">
                  <img src="https://anaconda.org/nicogodet/m2w64-msmpi/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/nicogodet/m2w64-msmpi">
                  <img src="https://anaconda.org/nicogodet/m2w64-msmpi/badges/latest_release_date.svg">
                </a>
              </td>
            </tr><tr>
              <td>m2w64-scalapack</td>
              <td>
                <a href="https://anaconda.org/nicogodet/m2w64-scalapack">
                  <img src="https://anaconda.org/nicogodet/m2w64-scalapack/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/nicogodet/m2w64-scalapack">
                  <img src="https://anaconda.org/nicogodet/m2w64-scalapack/badges/latest_release_date.svg">
                </a>
              </td>
            </tr><tr>
              <td>m2w64-mumps</td>
              <td>
                <a href="https://anaconda.org/nicogodet/m2w64-mumps">
                  <img src="https://anaconda.org/nicogodet/m2w64-mumps/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/nicogodet/m2w64-mumps">
                  <img src="https://anaconda.org/nicogodet/m2w64-mumps/badges/latest_release_date.svg">
                </a>
              </td>
            </tr><tr>
              <td>telemac-mascaret-examples</td>
              <td>
                <a href="https://anaconda.org/nicogodet/telemac-mascaret-examples">
                  <img src="https://anaconda.org/nicogodet/telemac-mascaret-examples/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/nicogodet/telemac-mascaret-examples">
                  <img src="https://anaconda.org/nicogodet/telemac-mascaret-examples/badges/latest_release_date.svg">
                </a>
              </td>
            </tr><tr>
              <td>telemac-mascaret-documentation</td>
              <td>
                <a href="https://anaconda.org/nicogodet/telemac-mascaret-documentation">
                  <img src="https://anaconda.org/nicogodet/telemac-mascaret-documentation/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/nicogodet/telemac-mascaret-documentation">
                  <img src="https://anaconda.org/nicogodet/telemac-mascaret-documentation/badges/latest_release_date.svg">
                </a>
              </td>
            </tr>
          </tbody>
        </table>
      </details>
    </td>
  </tr>
</table>

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
   mamba create -n opentelemac -c nicogodet opentelemac
   mamba activate opentelemac
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

### Build

- Static build (not currently packaged)
- Dynamic build with API

### Lib

- Parallel computing using `m2w64-msmpi` (from [nicogodet anaconda repo](https://anaconda.org/nicogodet/m2w64-msmpi))
- MUMPS (scalapack) support using `m2w64-scalapack` (from [nicogodet anaconda repo](https://anaconda.org/nicogodet/m2w64-scalapack)) and `m2w64-mumps` (from [nicogodet anaconda repo](https://anaconda.org/nicogodet/m2w64-mumps))

### Custom commands

- Command switch to use debug build `telemac-debug [on/off]`

### Extra

- Provide `telemac-mascaret-examples` package

   ```console
   mamba install -c nicogodet telemac-mascaret-examples
   ```

- Provide `telemac-mascaret-documentation` package

   ```console
   mamba install -c nicogodet telemac-mascaret-documentation
   ```

## TO-DO

- HDF5 and MED support
- aed2 support
- gotm support

To check that the environment is correctly installed, first activate your conda environment :

    mamba activate telemac
then try to compile the binaries from the sources: 

    compile_telemac.py

### Run an example 
For the sake of space, this package does **not** comport the examples, notebooks and documentation.

We suggest to download the latest tag **v8p4** on the [openTELEMAC repository](https://gitlab.pam-retd.fr/otm/telemac-mascaret/-/tree/v8p4r0?ref_type=tags): 

    git clone https://gitlab.pam-retd.fr/otm/telemac-mascaret.git`
    git checkout -b v8p4r0`
    git pull origin v8p4r0`

go to any example and run it: 

    cd examples/tomawac/Manche
    tomawac.py tom_manches.cas

# Production Plan : 
First release (done):
* minimal version necessary for the latest openTELEMAC version **v8p4**

Second release (current): 
* all the optionals libraries needed for:
  * solver optimisation (MUMPS)
  * GOTM (General Ocean Turbulence Model)
  * BLAS / LAPACK libraries
  * AED2 

Third release: 
  * Salomé formats (MED) 

For more information check out the [wiki](https://github.com/tomsail/telemac-conda/wiki)