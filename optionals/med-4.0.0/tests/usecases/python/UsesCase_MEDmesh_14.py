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
#  *  Use case 14 : read a 2D unstructured mesh with 2 polygons
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

#  open MED file with READ ONLY access mode
try :
	fid = med.MEDfileOpen("UsesCase_MEDmesh_13.med", med.MED_ACC_RDONLY)
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

#  ... we know that we only have MED_POLYGON cells in the mesh,
#  a real code would check all MED geometry cell types ...

#  How many polygon in the mesh in nodal connectivity mode
#  For the polygons, we get the size of array index
try :
	indexsize, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
								med.MED_NO_IT, med.MED_CELL, med.MED_POLYGON,
								med.MED_INDEX_NODE, med.MED_NODAL)
except RuntimeError as ex :
	print("ERROR : read number of polygon ...\n%s" % ex)
	doCleanup(fid, -1)
npoly = indexsize - 1

#  how many nodes for the polygon connectivity ?
try :
	connectivitysize, chgt, trsf = med.MEDmeshnEntity(fid, meshname,
						med.MED_NO_DT, med.MED_NO_IT, med.MED_CELL,
						med.MED_POLYGON, med.MED_CONNECTIVITY, med.MED_NODAL)
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

#  read polygons connectivity
index = med.MEDINT([0] * indexsize)
connectivity = med.MEDINT([0] * connectivitysize)

try :
	med.MEDmeshPolygonRd(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
						med.MED_CELL, med.MED_NODAL, index, connectivity)
except RuntimeError as ex :
	print("ERROR : read polygon connectivity ...\n%s" % ex)
	doCleanup(fid, -1)

#  ... we know that the family number of nodes and elements is 0, a real code would check ...

#  close MED file
doCleanup(fid, 0)