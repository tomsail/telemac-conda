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
#  *  How to create an unstructured mesh with polyhedrons
#  *
#  *  Use case 15 : a 3D unstructured mesh with 2 polyhedrons
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
meshname = "3D Unstructured Mesh With 2 polyhedrons"
spacedim = 3
meshdim = 3
#  max size of axisname and unitname : 3*med.MED_SNAME_SIZE+1
#           123456789012345612345678901234561234567890123456
axisname = "x               y               z               "
unitname = "cm              cm              cm              "
coordinates = med.MEDFLOAT([
	-10.0,  -10.0,  +0.0, # [ 1]
	-10.0,  -10.0,  +10., # [ 2]
	-10.0,  +10.0,  +10., # [ 3]
	-10.0,  +10.0,  +0.0, # [ 4]
	+10.0,  -10.0,  +0.0, # [ 5]
	+10.0,  -10.0,  +10., # [ 6]
	+10.0,  +10.0,  +10., # [ 7]
	+10.0,  +10.0,  +0.0, # [ 8]
	-10.0,  +0.0 , +10.0, # [ 9]
	-10.0,  +0.0 , +0.0 , # [10]
	+0.0 , -10.0 , +10.0, # [11]
	+0.0 , -10.0 , +0.0 , # [12]
	+0.0 , +10.0 , +10.0, # [13]
	+10.0,  +0.0 , +10.0, # [14]
	+0.0 , +10.0 , +0.0 , # [15]
	+10.0,  +0.0 , +0.0   # [16]
])
nnodes = 16
#  58 node numbers in the connectivity array
connectivity = med.MEDINT([
                               # - Poly 1
	1,  2  ,  9 , 3  , 10,     # - Face 1
	1,  12 ,  5 , 6  , 11,  2, # - Face 2
	2,  11 ,  6 , 3  , 9 ,     # - Face 3
	3,  6  ,  5 ,              # - Face 4
	3,  5  , 10 ,              # - Face 5
	1,  10 ,  5 , 12,          # - Face 6
                               # - Poly 2
	3,  13 ,  7 , 8  , 15,  4, # - Face 1
	3,  4  , 10 ,              # - Face 2
	4,  15 ,  8 , 16 ,  5,  10,# - Face 3
	3,  6  , 14 , 7  , 13,     # - Face 4
	5,  16 ,  8 , 7  , 14,  6, # - Face 5
	3,  10 ,  5 ,              # - Face 6
	3,  5  ,  6                # - Face 7
])

#  there are two polyhedrons, the first has 6 faces, the second 7 faces
#  face 7 of Poly 2 uses 59-56=3 nodes
nodeindex = med.MEDINT([1,  6,  12, 17, 20, 23,
				    	27, 33, 36, 42, 47, 53, 56, 59])
#  there is a total of 13 faces
#  nodeindexsize == nbr of faces + 1
nodeindexSize = 6+7+1

#  there are two polyhedrons, the first has 6 faces, the second 7 faces
#  Pn+1 == FaceIndex[n+1]== NodeIndexSize== nbr of faces + 1
faceindex = med.MEDINT([1, 7, 14])
faceindexSize = 3

#  MED file creation
try :
	fid = med.MEDfileOpen("UsesCase_MEDmesh_15.med", med.MED_ACC_CREAT)
except RuntimeError as ex :
	print("ERROR : file creation ...\n%s" % ex)
	sys.exit(-1)

#  write a comment in the file
try :
	med.MEDfileCommentWr(fid, "A 3D unstructured mesh : 2 polyhedrons")
except RuntimeError as ex :
	print("ERROR : write file description ...\n%s" % ex)
	doCleanup(fid, -1)

#  mesh creation : a 3D unstructured mesh
try :
	med.MEDmeshCr(fid, meshname, spacedim, meshdim, med.MED_UNSTRUCTURED_MESH,
				"A 3D mesh with 2 polyhedron", "", med.MED_SORT_DTIT,
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

#  cells connectiviy is defined in nodal mode with no iteration and computation step
try :
	med.MEDmeshPolyhedronWr(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
			med.MED_UNDEF_DT, med.MED_CELL, med.MED_NODAL, faceindexSize,
			faceindex, nodeindexSize, nodeindex, connectivity)
except RuntimeError as ex :
	print("ERROR : polyhedron connectivity ...\n%s" % ex)
	doCleanup(fid, -1)

#  create family 0 : by default, all mesh entities family number is 0
#TODO : Etudier la pertinence de définir MED_NO_GROUP comme un MEDCHAR('')
#TODO : A confronter aux types des paramètres axisname, axisunit
try :
	med.MEDfamilyCr(fid, meshname, "", 0, 0, med.MEDCHAR(""))
except RuntimeError as ex :
	print("ERROR : family 0 creation ...\n%s" % ex)
	doCleanup(fid, -1)

#  close MED file
doCleanup(fid, 0)