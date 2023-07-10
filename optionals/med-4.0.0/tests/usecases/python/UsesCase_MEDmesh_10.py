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
#  *  How to create an unstructured mesh
#  *
#  *  Use case 10 : a 2D unstructured mesh with 15 nodes, 8 triangular cells, 4 triangular cells
#  *  and families
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
meshname = "2D unstructured mesh"
spacedim = 2
meshdim = 2
#  max size of axisname and unitname : 2*med.MED_SNAME_SIZE+1
#           12345678901234561234567890123456
axisname = "x               y               "
unitname = "cm              cm              "
coordinates = med.MEDFLOAT([2.,1.,  7.,1.,  12.,1.,  17.,1.,  22.,1.,
							2.,6.,  7.,6.,  12.,6.,  17.,6.,  22.,6.,
							2.,11., 7.,11., 12.,11., 17.,11., 22.,11. ])
nnodes = 15
triaconnectivity = med.MEDINT([ 1,7,6,   2,7,1,  3,7,2,   8,7,3,
								13,7,8, 12,7,13, 11,7,12, 6,7,11 ])
ntria3 = 8
quadconnectivity = med.MEDINT([ 3,4,9,8,    4,5,10,9,
								15,14,9,10, 13,8,9,14 ])
nquad4 = 4
#  max size of familyname : med.MED_NAME_SIZE+1
familyname = "BOUNDARY_VERTICES"
#  max size of groupename : med.MED_LNAME_SIZE+1
groupname = med.MEDCHAR("MESH_BOUNDARY_VERTICES")
familynumbers = med.MEDINT([1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1])

#  MED file creation
try :
	fid = med.MEDfileOpen("UsesCase_MEDmesh_10.med", med.MED_ACC_CREAT)
except RuntimeError as ex :
	print("ERROR : file creation ...\n%s" % ex)
	sys.exit(-1)

#  write a comment in the file
try :
	med.MEDfileCommentWr(fid, "A 2D unstructured mesh : 15 nodes, 12 cells")
except RuntimeError as ex :
	print("ERROR : write file description ...\n%s" % ex)
	doCleanup(fid, -1)

#  mesh creation : a 2D unstructured mesh
try :
	med.MEDmeshCr(fid, meshname, spacedim, meshdim, med.MED_UNSTRUCTURED_MESH,
				"A 2D unstructured mesh", "", med.MED_SORT_DTIT,
				med.MED_CARTESIAN, axisname, unitname)
except RuntimeError as ex :
	print("ERROR : mesh creation ...\n%s" % ex)
	doCleanup(fid, -1)

#  nodes coordinates in a cartesian axis in full interlace mode
#  (X1,Y1, X2,Y2, X3,Y3, ...) with no iteration and computation step
try :
	med.MEDmeshNodeCoordinateWr(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
							0.0, med.MED_FULL_INTERLACE, nnodes, coordinates)
except RuntimeError as ex :
	print("ERROR : nodes coordinates ...\n%s" % ex)
	doCleanup(fid, -1)

#  cells connectiviy is defined in nodal mode
try :
	med.MEDmeshElementConnectivityWr(fid, meshname, med.MED_NO_DT,med.MED_NO_IT,
							0.0, med.MED_CELL, med.MED_TRIA3, med.MED_NODAL,
							med.MED_FULL_INTERLACE, ntria3, triaconnectivity)
except RuntimeError as ex :
	print("ERROR : triangular cells connectivity ...\n%s" % ex)
	doCleanup(fid, -1)

try :
	med.MEDmeshElementConnectivityWr(fid, meshname, med.MED_NO_DT,med.MED_NO_IT,
							0.0, med.MED_CELL, med.MED_QUAD4, med.MED_NODAL,
							med.MED_FULL_INTERLACE, nquad4, quadconnectivity)
except RuntimeError as ex :
	print("ERROR : quadrangular cells connectivity ...\n%s" % ex)
	doCleanup(fid, -1)

#  create family 0 : by default, all mesh entities family number is 0
#TODO : Etudier la pertinence de définir MED_NO_GROUP comme un MEDCHAR('')
#TODO : A confronter aux types des paramètres axisname, axisunit
try :
	med.MEDfamilyCr(fid, meshname, med.MED_NO_NAME, 0, 0,
				med.MEDCHAR(med.MED_NO_GROUP))
except RuntimeError as ex :
	print("ERROR : family 0 creation ...\n%s" % ex)
	doCleanup(fid, -1)

#  create a family for boundary vertices : by convention a nodes family number is > 0,
#  and an element family number is < 0
try :
	med.MEDfamilyCr(fid, meshname, familyname, 1, 1, groupname)
except RuntimeError as ex :
	print("ERROR : family %s creation ...\n%s" % (familyname, ex))
	doCleanup(fid, -1)

#  write family number for nodes
try :
	med.MEDmeshEntityFamilyNumberWr(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
						med.MED_NODE, med.MED_NONE, nnodes, familynumbers)
except RuntimeError as ex :
	print("ERROR : nodes family numbers ...\n%s" % ex)
	doCleanup(fid, -1)

#  close MED file
doCleanup(fid, 0)