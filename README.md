openTELEMAC conda package 
=========================
![CI](https://github.com/tomsail/telemac-conda/actions/workflows/test_examples.yml/badge.svg) 
![TELEMAC](http://www.opentelemac.org/media/kunena/attachments/75/openTELEMAC_hr.png)
This is the repository for the conda package of the openTELEMAC system

# Current build status

<table>
    
  <tr>
    <td> Windows </td>
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
              <td>opentelemac</td>
              <td>
                <a href="https://anaconda.org/nicogodet/opentelemac">
                  <img src="https://anaconda.org/nicogodet/opentelemac/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/nicogodet/opentelemac">
                  <img src="https://anaconda.org/nicogodet/opentelemac/badges/latest_release_date.svg">
                </a>
              </td>
            </tr><tr>
              <td>opentelemac-examples</td>
              <td>
                <a href="https://anaconda.org/nicogodet/opentelemac-examples">
                  <img src="https://anaconda.org/nicogodet/opentelemac-examples/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/nicogodet/opentelemac-examples">
                  <img src="https://anaconda.org/nicogodet/opentelemac-examples/badges/latest_release_date.svg">
                </a>
              </td>
            </tr><tr>
              <td>opentelemac-documentation</td>
              <td>
                <a href="https://anaconda.org/nicogodet/opentelemac-documentation">
                  <img src="https://anaconda.org/nicogodet/opentelemac-documentation/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/nicogodet/opentelemac-documentation">
                  <img src="https://anaconda.org/nicogodet/opentelemac-documentation/badges/latest_release_date.svg">
                </a>
              </td>
            </tr>
          </tbody>
        </table>
      </details>
    </td>
  </tr><tr>
    <td> Ubuntu </td>
    <td>
      <details>
        <summary>
          <a href="https://anaconda.org/tomsail/opentelemac">
            <img src="https://anaconda.org/tomsail/opentelemac/badges/version.svg">
          </a>
        </summary>
        <table>
          <thead><tr><th>Dependencies</th><th>Version</th><th>Last update</th></tr></thead>
          <tbody><tr>
              <td>opentelemac</td>
              <td>
                <a href="https://anaconda.org/tomsail/opentelemac">
                  <img src="https://anaconda.org/tomsail/opentelemac/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/tomsail/opentelemac">
                  <img src="https://anaconda.org/tomsail/opentelemac/badges/latest_release_date.svg">
                </a>
              </td>
            </tr><tr>
              <td>opentelemac-examples</td>
              <td>
                <a href="https://anaconda.org/tomsail/opentelemac-examples">
                  <img src="https://anaconda.org/tomsail/opentelemac-examples/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/tomsail/opentelemac-examples">
                  <img src="https://anaconda.org/tomsail/opentelemac-examples/badges/latest_release_date.svg">
                </a>
              </td>
            </tr><tr>
              <td>opentelemac-documentation</td>
              <td>
                <a href="https://anaconda.org/tomsail/opentelemac-documentation">
                  <img src="https://anaconda.org/tomsail/opentelemac-documentation/badges/version.svg">
                </a>
              </td>
              <td>
                <a href="https://anaconda.org/tomsail/opentelemac-documentation">
                  <img src="https://anaconda.org/tomsail/opentelemac-documentation/badges/latest_release_date.svg">
                </a>
              </td>
            </tr>
          </tbody>
        </table>
      </details>
    </td>
  </tr>
</table>

# Introduction

The [opentelemac solver suite](http://www.opentelemac.org) is a high-performance Fortran based application for a range
of free-surface flow problems.

This software is a very powerful toolkit for numeric simulation that can be slightly cumbersome to install depending on the
host operating system. The runtime might also need some tweaks to the configuration provided in the upstream repository to
reflect Fortran compiler version and library locations.

Currently, only win-64 package is provided.

# How to install?
it is recommended to use [Miniforge3](https://github.com/conda-forge/miniforge).

## On Windows
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

## On Ubuntu
1. [See wiki](https://github.com/tomsail/telemac-conda/wiki/Install-conda-..-or-rather-mamba-!) to Download and install mamba

2. In terminal:

   ```console
   mamba create -n opentelemac -c tomsail opentelemac
   mamba activate opentelemac
   ```

3. All telemac scripts have been added to PATH & PYTHONPATH. So you can just use TELEMAC with: 

   ```console
   module.py argument
   ```

   Exemple:

   ```console
   telemac2d.py --ncsize=4 -s cas_file.cas
   ```

# Current implemented features

## Build

- Static build (not currently packaged)
- Dynamic build with API

## Lib
### Windows
- Parallel computing using `m2w64-msmpi` (from [nicogodet anaconda repo](https://anaconda.org/nicogodet/m2w64-msmpi))
- MUMPS (scalapack) support using `m2w64-scalapack` (from [nicogodet anaconda repo](https://anaconda.org/nicogodet/m2w64-scalapack)) and `m2w64-mumps` (from [nicogodet anaconda repo](https://anaconda.org/nicogodet/m2w64-mumps))

### Ubuntu
- Parallel computing using openmpi
- MUMPS for ARTEMIS parallel computing
- GOTM, AED2 and MED (MED not currently working) using [med-otm](https://anaconda.org/tomsail/), [gotm-otm](https://anaconda.org/tomsail/gotm-otm) and [aed2-otm](https://anaconda.org/tomsail/aed2-otm). 

## Custom commands

- Command switch to use debug build `telemac-debug [on/off]`

## Extra

- Provide `opentelemac-examples` package

  Windows: 
   ```console
   mamba install -c nicogodet opentelemac-examples
   ```
  Linux: 
   ```console
   mamba install -c tomsail opentelemac-examples
   ```
- Provide `opentelemac-documentation` package

  Windows: 
   ```console
   mamba install -c nicogodet opentelemac-documentation
   ```
  Linux: 
   ```console
   mamba install -c tomsail opentelemac-documentation
   ```

To check that the environment is correctly installed, first activate your conda environment :

    mamba activate telemac
then try to compile the binaries from the sources: 

    compile_telemac.py

### Run an example 
For the sake of space, this package does **not** comport the examples, notebooks and documentation.

We suggest either : 

1. download the latest tag **v8p4** on the [openTELEMAC repository](https://gitlab.pam-retd.fr/otm/telemac-mascaret/-/tree/v8p4r0?ref_type=tags): 

    git clone https://gitlab.pam-retd.fr/otm/telemac-mascaret.git`
    git checkout -b v8p4r0`
    git pull origin v8p4r0`
2. use the `opentelemac-examples` package cited above

then go to any example and run it: 

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