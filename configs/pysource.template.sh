# This file is a template for a Linux environment file
# running "source pysource.template.sh" will position all
# the necessary environment variables for telemac
# To adapt to your installation replace word <word> by their local value
###
### TELEMAC settings -----------------------------------------------------------
###
# Path to telemac root dir
export HOMETEL=$SRC_DIR
# Adding python scripts to PATH
export PATH=$HOMETEL/scripts/python3:.:$PATH
# Configuration file
export SYSTELCFG=$HOMETEL/configs/systel.cfg
# Name of the configuration to use
export USETELCFG=S10.gfortran.dyn
# Path to this file
export SOURCEFILE=$HOMETEL/configs/pysource.template.sh
### Python
# To force python to flush its output
export PYTHONUNBUFFERED='true'                  
### API
export PYTHONPATH=$HOMETEL/scripts/python3
export LD_LIBRARY_PATH=$HOMETEL/builds/$USETELCFG/wrap_api/lib:$HOMETEL/builds/$USETELCFG/lib
export PYTHONPATH=$HOMETEL/builds/$USETELCFG/wrap_api/lib:$PYTHONPATH
###
### COMPILERS -----------------------------------------------------------
###
# Here are a few examples for external libraries
export SYSTEL=$HOMETEL

### MPI -----------------------------------------------------------
export MPIHOME=$HOMETEL/lib
export PATH=$HOMETEL:$PREFIX:$PATH
export LD_LIBRARY_PATH=$PATH/lib:$LD_LIBRARY_PATH
###
### EXTERNAL LIBRARIES -----------------------------------------------------------
###
### HDF5 -----------------------------------------------------------
export HDF5HOME=$SYSTEL/lib
export LD_RUN_PATH=$HDF5HOME
### MED  -----------------------------------------------------------
export MEDHOME=$SYSTEL/optionals/med-4.0.0
export LD_LIBRARY_PATH=$MEDHOME/lib:$LD_LIBRARY_PATH
export PATH=$MEDHOME/bin:$PATH
### MUMPS -------------------------------------------------------------
export MUMPSHOME=$SYSTEL/lib
export SCALAPACKHOME=$SYSTEL/LIBRARY/lic
export BLACSHOME=$SYSTEL/lib
### METIS -------------------------------------------------------------
export METISHOME=/usr/local
export LD_LIBRARY_PATH=$METISHOME/lib:$LD_LIBRARY_PATH
