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
#  *
#  *  Use case 13 : a 2D unstructured mesh with 10 nodes and 2 polygons
#  *  poly1 : 1,4,7,9,6,3
#  *  poly2 : 2,5,8,10,7,4
#  *      9   10
#  *
#  *   6    7    8
#  *
#  *   3    4    5
#  *
#  *      1     2
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
meshname = "2D unstructured mesh"
spacedim = 2
meshdim = 2
#  max size of axisname and unitname : 2*med.MED_SNAME_SIZE+1
#           12345678901234561234567890123456
axisname = "x               y               "
unitname = "cm              cm              "
#  ten nodes with two of them shared by the two polygons
coordinates = med.MEDFLOAT([0.5, 0.,
							1.5, 0.,
							0.,  0.5,
							1.,  0.5,
							2.,  0.5,
							0.,  1.,
							1.,  1.,
							2.,  1.,
							0.5, 2.,
							1.5, 2. ])
nnodes = 10
indexsize = 3
index = med.MEDINT([1, 7, 13])
#  connectivity : 2 hexagons
connectivity = med.MEDINT([ 1,  4,  7,  9,  6,  3,
						    2,  5,  8,  10, 7,  4])

#  MED file creation
try :
	fid = med.MEDfileOpen("UsesCase_MEDmesh_13.med", med.MED_ACC_CREAT)
except RuntimeError as ex :
	print("ERROR : file creation ...\n%s" % ex)
	sys.exit(-1)

#  write a comment in the file
try :
	med.MEDfileCommentWr(fid, "A 2D unstructured mesh : 12, 12 polygons")
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
								med.MED_UNDEF_DT, med.MED_FULL_INTERLACE,
								nnodes, coordinates)
except RuntimeError as ex :
	print("ERROR : nodes coordinates ...\n%s" % ex)
	doCleanup(fid, -1)

#  cells connectiviy is defined in nodal mode
#  2 polygons
try :
	med.MEDmeshPolygonWr(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
						med.MED_UNDEF_DT, med.MED_CELL, med.MED_NODAL,
						indexsize, index, connectivity)
except RuntimeError as ex :
	print("ERROR : polygon connectivity ...\n%s" % ex)
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

#  close MED file
doCleanup(fid, 0)
