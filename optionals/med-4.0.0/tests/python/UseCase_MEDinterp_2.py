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
#  * Interp use case 2 : read an interpolation family with a direct access by name
#  * In this example, the interpolation family can be used with
#  * the TEMPERATURE field of UsesCase_MEDfield_10 use case
#  */

from med.medfile import *
from med.medinterp import *

interpname = "MED_TRIA3 interpolation family"

#/* file creation */
fid = MEDfileOpen("UsesCase_MEDinterp_1.med",MED_ACC_RDONLY)
 
#/* with direct access by the family name */
geotype, cellnodes, nbasisfunc, nvariable, maxdegree, nmaxcoefficient = MEDinterpInfoByName(fid,interpname)

#/* read each basis function */
for  basisfuncit in range(1,nbasisfunc+1):

    ncoefficient = MEDinterpBaseFunctionCoefSize(fid,interpname,basisfuncit)
    print("\t Nombre de coefficients :",ncoefficient)
   
    coefficient = MEDFLOAT(ncoefficient)
    power       = MEDINT(nvariable*ncoefficient)
    
    MEDinterpBaseFunctionRd(fid,interpname,basisfuncit,power,coefficient)
    print("Function de base n°%d de l'interpolation |%s| : "%(basisfuncit,interpname))
    print("\t Nombre de coefficients :",ncoefficient)
    print("\t Nombre de variables :",nvariable)
    print("\t coefficient :",coefficient)
    print("\t power       :",power)
    
MEDfileClose(fid)


