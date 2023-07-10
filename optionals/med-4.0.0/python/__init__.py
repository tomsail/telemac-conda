#                                               -*- Python -*-
#
#  __init__.py
#

"""
    MED --- _____ 
    ===================================================

    Documentation is available online at http://www.

    Contents
    --------
      Module MED imports all the functions from the MED modules.

    Available subpackages
    ---------------------

      medenum
      medequivalence
      medfamily
      medfield
      medfile
      medfilter
      medinterp
      medlibrary
      medlink
      medlocalization
      medmesh
      medparameter
      medprofile

    Utility tools
    -------------

    Environment variables
    ---------------------
    
"""
from __future__ import absolute_import

import os
import sys
import platform
import warnings
if platform.system() == "Windows":
    # the following line will be modified by the windows installer
    MED_PYTHON_PATH = ""
    if MED_PYTHON_PATH != "":
        # set Path to MED module
        os.environ['PATH'] = MED_PYTHON_PATH + ';' + os.environ['PATH']


# check if interactive mode
if not hasattr(sys, 'ps1'):
    try:
        # ipython does not define ps1
        __IPYTHON__
    except:
        # Reset the default Crtl-C behavior
        import signal
        try:
            signal.signal(signal.SIGINT, signal.SIG_DFL)
        except ValueError:
            pass

from .medenum import *
from .medequivalence import *
from .medfamily import *
from .medfield import *
from .medfile import *
from .medfilter import *
from .medinterp import *
from .medlibrary import *
from .medlink import *
from .medlocalization import *
from .medmesh import *
from .medparameter import *
from .medprofile import *


# define the version
#__version__ = PlatformInfo.GetVersion()
