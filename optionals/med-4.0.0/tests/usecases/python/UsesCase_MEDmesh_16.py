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
#  *  Use case 16 : read a 3D unstructured mesh with 2 polyhedrons
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

#  open MED file with READ ONLY access mode
try :
	fid = med.MEDfileOpen("UsesCase_MEDmesh_15.med", med.MED_ACC_RDONLY)
except RuntimeError as ex :
	print("ERROR : open file in READ ONLY ACCESS mode ...\n%s" % ex)
	sys.exit(-1)

#  ... we know that this MED file has only one mesh,
#  a real code would check ...

#  read mesh informations : mesh dimension, space dimension ...
try :
	meshinfo = med.MEDmeshInfoByName(fid, meshname)
except RuntimeError as ex :
	print("ERROR : mesh info ...\n%s" % ex)
	doCleanup(fid, -1)

if (not meshinfo) or (len(meshinfo) != 10) :
	print("ERROR : no mesh info created from MEDmeshInfoByName")
	doCleanup(fid, -1)
else :
	spacedim,meshdim,meshtype,description,dtunit,sortingtype,nstep,axistype,axisname,axisunit=meshinfo

#  read how many nodes in the mesh
try :
	nnodes, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
						med.MED_NO_IT, med.MED_NODE, med.MED_POINT1,
						med.MED_COORDINATE, med.MED_NO_CMODE)
except RuntimeError as ex :
	print("ERROR : number of nodes ...\n%s" % ex)
	doCleanup(fid, -1)

#  ... we know that we only have MED_POLYHEDRON cells in the mesh,
#  a real code would check all MED geometry cell types ...

#  We read how many polyhedrons are in the mesh (using nodal connectivity mode)
#  We get the size of the polyhedrons/face index array.
#  As an index of the face index array give the location of the first face   and so the
#  number of polyhedrons
try :
	faceindexsize, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
							med.MED_NO_IT, med.MED_CELL, med.MED_POLYHEDRON,
							med.MED_INDEX_FACE, med.MED_NODAL)
except RuntimeError as ex :
	print("ERROR : read number of polyhedrons faceindex ...\n%s" % ex)
	doCleanup(fid, -1)
npoly = faceindexsize - 1

try :
	nodeindexsize, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,med.MED_CELL,
					med.MED_POLYHEDRON, med.MED_INDEX_NODE, med.MED_NODAL)
except RuntimeError as ex :
	print("ERROR : read number of polyhedrons nodeindex ...\n%s" % ex)
	doCleanup(fid, -1)

#  how many nodes for the polyhedron connectivity ?
try :
	connectivitysize, chgt, trsf = med.MEDmeshnEntity(fid, meshname,
						med.MED_NO_DT, med.MED_NO_IT, med.MED_CELL,
						med.MED_POLYHEDRON, med.MED_CONNECTIVITY, med.MED_NODAL)
except RuntimeError as ex :
	print("ERROR : read connectivity size ...\n%s" % ex)
	doCleanup(fid, -1)

#  read mesh nodes coordinates
coordinates = med.MEDFLOAT([0.] * nnodes * spacedim)
try :
	med.MEDmeshNodeCoordinateRd(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
								med.MED_FULL_INTERLACE, coordinates)
except RuntimeError as ex :
	print("ERROR : nodes coordinates ...\n%s" % ex)
	doCleanup(fid, -1)

for i in range(0, nnodes * spacedim) :
	sys.stdout.write(str(coordinates[i]))
	if (i + 1 != nnodes * spacedim) :
		sys.stdout.write(" - ")
	else :
		print("")

#  read polyhedron connectivity
faceindex = med.MEDINT([0] * faceindexsize)
nodeindex = med.MEDINT([0] * nodeindexsize)
connectivity = med.MEDINT([0] * connectivitysize)

try :
	med.MEDmeshPolyhedronRd(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
							med.MED_CELL, med.MED_NODAL, faceindex,
							nodeindex, connectivity)
except RuntimeError as ex :
	print("ERROR : read polyhedron connectivity ...\n%s" % ex)
	doCleanup(fid, -1)

for i in range(0, npoly) :
	print(">> MED_POLYHEDRON %s :" % str(i+1))
	print("---- Face Index         ----- : [")
	ind1 = faceindex[i]-1
	ind2 = faceindex[i+1]-1
	for k in range(ind1, ind2) :
		sys.stdout.write(str(nodeindex[k]) + " ")
	print(" ] ")
	print("---- Connectivity       ----- : [")
	for k in range(ind1, ind2) :
		jind1 = nodeindex[k]-1
		jind2 = nodeindex[k+1]-1
		for j in range(jind1, jind2) :
			sys.stdout.write(str(connectivity[j]) + " ")
		print("")
	print(" ]")

#  ... we know that the family number of nodes and elements is 0, a real code would check ...

#  close MED file
doCleanup(fid, 0)