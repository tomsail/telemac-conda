###
### TELEMAC settings -----------------------------------------------------------
###
# TELEMAC version number
#export TELVER=v8p4r0
# Path to telemac root dir
export HOMETEL=$SRC_DIR
# Adding python scripts to PATH
export PATH=$HOMETEL/scripts/python3:.:$PATH
# Configuration file
export SYSTELCFG=$HOMETEL/configs/systel.macos.cfg
# Name of the configuration to use
export USETELCFG=gfort-mpich
# Path to this file
export SOURCEFILE=$HOMETEL/configs/pysource.macos.sh
### Python
# To force python to flush its output
export PYTHONUNBUFFERED='true'
### API
export PYTHONPATH=$HOMETEL/scripts/python3 #:$PYTHONPATH
export LD_LIBRARY_PATH=$HOMETEL/builds/$USETELCFG/lib #:$LD_LIBRARY_PATH
export PYTHONPATH=$HOMETEL/builds/$USETELCFG/lib:$PYTHONPATH
###
### MPI -----------------------------------------------------------
export MPIHOME=$CONDA_PREFIX
export PATH=$MPIHOME/bin:$PATH
export LD_LIBRARY_PATH=$MPIHOME/lib:$LD_LIBRARY_PATH
###
### EXTERNAL LIBRARIES -----------------------------------------------------------
###
### METIS -------------------------------------------------------------
export METISHOME=$CONDA_PREFIX
export LD_LIBRARY_PATH=$METISHOME/lib:$LD_LIBRARY_PATH
