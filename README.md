# TELEMAC-Conda 

This is the repository for the conda package of the openTELEMAC system
![TELEMAC](http://www.opentelemac.org/media/kunena/attachments/75/openTELEMAC_hr.png)

This package provides an operational working environment of openTELEMAC including: 
* all the python scripts included in the path
* all the TELEMAC binaries precompiled dynamically (with working API)

## Install the package 
to install the package, we recommend to use mamba (see link) and choose the channel tomsail where the package has been uploaded

    mamba install -c tomsail telemac

## Create an environment with TELEMAC 

    mamba create -n telemac -c tomsail telemac

## Run TELEMAC
### Check that the package works 
This package has been built with the same architecture as the [openTELEMAC repository](https://gitlab.pam-retd.fr/otm/telemac-mascaret).

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
First release (current):
* minimal version necessary for the latest openTELEMAC version **v8p4**

Second release: 
* all the optionals libraries needed for:
  * solver optimisation (MUMPS)
  * Salomé formats (MED) binaries
  * GOTM (General Ocean Turbulence Model)
  * BLAS / LAPACK libraries

For more information check out the [wiki](https://github.com/tomsail/telemac-conda/wiki)
