# TELEMAC-Conda 

This is the repository for the conda package of the openTELEMAC system
![TELEMAC](http://www.opentelemac.org/media/kunena/attachments/75/openTELEMAC_hr.png)

To install the package: 

`conda install -c tomsail telemac`


This package provides an operational working environment of openTELEMAC including: 
* all the python scripts included in the path
* all the TELEMAC binaries precompiled dynamically (with working API)

First release (current):
* minimal version necessary for the latest openTELEMAC version **v8p4**

Second release: 
* all the optionals libraries needed for:
  * solver optimisation (MUMPS)
  * Salomé formats (MED) binaries
  * GOTM (General Ocean Turbulence Model)
  * BLAS / LAPACK libraries

For more information check out the [wiki](https://github.com/tomsail/telemac-conda/wiki)
