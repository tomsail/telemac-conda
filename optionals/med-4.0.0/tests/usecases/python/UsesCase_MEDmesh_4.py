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

# /******************************************************************************
#  *  How to create an structured mesh
#  *
#  *  Use case 4 :  write a 2D structured mesh (5x3 cartesian grid)
#  *
#  *****************************************************************************/

import sys

import med

def doCleanup(fid, status) :
	try :
		med.MEDfileClose(fid)
	except RuntimeError as ex :
		print("ERROR : close file ...\n%s" % ex)
		status = -1
	finally :
		if status != 0 :
			sys.exit(status)

#  max size of meshname : med.MED_NAME_SIZE+1
meshname = "2D structured mesh"
spacedim = 2
meshdim = 2
#  max size of axisname and unitname : 2*med.MED_SNAME_SIZE+1
#           12345678901234561234567890123456
axisname = "x               y               "
unitname = "cm              cm              "
cooXaxis = med.MEDFLOAT([1.,2.,3.,4.,5.])
cooYaxis = med.MEDFLOAT([1.,2.,3.])
nquad4 = 8
#  max size of cellsname : 8*med.MED_SNAME_SIZE+1
#                         12345678901234561234567890123456123456789012345612345678901234561234567890123456123456789012345612345678901234561234567890123456
cellsnames = med.MEDCHAR("CELL_1          CELL_2          CELL_3          CELL_4          CELL_5          CELL_6          CELL_7          CELL_8          ")
#  max size of familyname : med.MED_NAME_SIZE+1
familyname = "CART_GRID_QUAD_FAMILY"
#  max size of groupname : med.MED_LNAME_SIZE+1
groupname = med.MEDCHAR("CART_GRID_GROUP")
familynumbers = med.MEDINT([-1,-1,-1,-1,-1,-1,-1,-1])

#  MED file creation
try :
	fid = med.MEDfileOpen("UsesCase_MEDmesh_4.med", med.MED_ACC_CREAT)
except RuntimeError as ex :
	print("ERROR : file creation ...\n%s" % ex)
	sys.exit(-1)

#  create the structured mesh in the MED file
try :
	med.MEDmeshCr(fid, meshname, spacedim, meshdim, med.MED_STRUCTURED_MESH,
				"A 2D structured mesh", "", med.MED_SORT_DTIT,
				med.MED_CARTESIAN, axisname, unitname)
except RuntimeError as ex :
	print("ERROR : mesh creation ...\n%s" % ex)
	doCleanup(fid, -1)

#  specify the grid type : MED_CARTESIAN_GRID
try :
	med.MEDmeshGridTypeWr(fid, meshname, med.MED_CARTESIAN_GRID)
except RuntimeError as ex :
	print("ERROR : write grid type ...\n%s" % ex)
	doCleanup(fid, -1)

#  write axis "X" and "Y" coordinates
axis = 1
size = 5
try :
	med.MEDmeshGridIndexCoordinateWr(fid, meshname, med.MED_NO_DT,med.MED_NO_IT,
									0.0, axis, size, cooXaxis)
except RuntimeError as ex :
	print("ERROR : write of axis X coordinates ...\n%s" % ex)
	doCleanup(fid, -1)

axis = 2
size = 3
try :
	med.MEDmeshGridIndexCoordinateWr(fid, meshname, med.MED_NO_DT,med.MED_NO_IT,
									0.0, axis, size, cooYaxis)
except RuntimeError as ex :
	print("ERROR : write of axis Y coordinates ...\n%s" % ex)
	doCleanup(fid, -1)

#  optionnal : names for nodes or elements
#  In this case, a name is given to the cells of the mesh
try :
	med.MEDmeshEntityNameWr(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
				med.MED_CELL, med.MED_QUAD4, nquad4, cellsnames)
except RuntimeError as ex :
	print("ERROR : cells names ...\n%s" % ex)
	doCleanup(fid, -1)

#  We have to create family 0 : by default, all mesh entities family number is 0
#TODO : Etudier la pertinence de définir MED_NO_GROUP comme un MEDCHAR('')
#TODO : A confronter aux types des paramètres axisname, axisunit
try :
	med.MEDfamilyCr(fid, meshname, med.MED_NO_NAME, 0, 0,
				med.MEDCHAR(med.MED_NO_GROUP))
except RuntimeError as ex :
	print("ERROR : family 0 creation ...\n%s" % ex)
	doCleanup(fid, -1)

#  We decide to create a family for boundary quad4 :
#  by convention a nodes family number is > 0 and an element family number is < 0
try :
	med.MEDfamilyCr(fid, meshname, familyname, 1, -1, groupname)
except RuntimeError as ex :
	print("ERROR : family creation ...\n%s" % ex)
	doCleanup(fid, -1)

#  Then we write family number for quad4
try :
	med.MEDmeshEntityFamilyNumberWr(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
				med.MED_CELL, med.MED_QUAD4, nquad4, familynumbers)
except RuntimeError as ex :
	print("ERROR : nodes family numbers ...\n%s" % ex)
	doCleanup(fid, -1)

#  close MED file
doCleanup(fid, 0)