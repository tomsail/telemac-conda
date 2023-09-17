# TELEMAC-MASCARET Conda Package

![TELEMAC](http://www.opentelemac.org/media/kunena/attachments/75/openTELEMAC_hr.png)

## Current build status

<table>
    
  <tr>
    <td>Anaconda.org</td>
    <td>
      <details>
        <summary>
          <a href="https://anaconda.org/nicogodet/telemac-mascaret">
            <img src="https://anaconda.org/nicogodet/telemac-mascaret/badges/version.svg">
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
