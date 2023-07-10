#! /usr/bin/env python
# -*- coding:utf-8 -*-
# /*  This file is part of MED.
#  *
#  *  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
#  *  MED is free software: you can redistribute it and/or modify
#  *  it under the terms of the GNU Lesser General Public License as published by
#  *  the Free Software Foundation, either version 3 of the License, or
#  *  (at your option) any later version.
#  *
#  *  MED is distributed in the hope that it will be useful,
#  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  *  GNU Lesser General Public License for more details.
#  *
#  *  You should have received a copy of the GNU Lesser General Public License
#  *  along with MED.  If not, see <http://www.gnu.org/licenses/>.
#  */


# /*
#  * Interp use case 1 : write an interpolation family
#  * In this example, the interpolation family can be used with
#  * the TEMPERATURE field of UsesCase_MEDfield_10 use case
#  */

from med.medfile import *
from med.medinterp import *

interpname = "MED_TRIA3 interpolation family"
nvariable          = 2
maxdegree          = 1
nmaxcoefficient    = 3
ncoefficient1_1    = 3
power1_1           = MEDINT([0,0,1,0,0,1])
coefficient1_1     = MEDFLOAT([1.0,-1.0,-1.0])

ncoefficient1_2    = 1
power1_2           = MEDINT([1,0])
coefficient1_2     = MEDFLOAT([1.0])

ncoefficient1_3 = 1;
power1_3           = MEDINT([0,1])
coefficient1_3     = MEDFLOAT([1.0])

# /* file creation */
fid = MEDfileOpen("UsesCase_MEDinterp_1.med",MED_ACC_CREAT)

# /* Family interpolation creation :
#    - reference element = MED_TRIA3 
#    - integration points of UsesCase_MEDfield_10 use case 
#    are used to build the interpolation                 
#    - basis functions are P1(X)= 1-X1-X2, P2(X)= X1, P3(X)= X2 
#  */
MEDinterpCr(fid, interpname, MED_TRIA3, MED_FALSE, nvariable, maxdegree, nmaxcoefficient)

#/* Basis functions creation */
MEDinterpBaseFunctionWr(fid,interpname,1,ncoefficient1_1,power1_1,coefficient1_1)
MEDinterpBaseFunctionWr(fid,interpname,2,ncoefficient1_2,power1_2,coefficient1_2)
MEDinterpBaseFunctionWr(fid,interpname,3,ncoefficient1_3,power1_3,coefficient1_3)

MEDfileClose(fid)


