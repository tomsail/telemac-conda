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
#  *  Use case 7 : read a 2D unstructured mesh with nodes coordinates modifications
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
	fid = med.MEDfileOpen("UsesCase_MEDmesh_6.med", med.MED_ACC_RDONLY)
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
						med.MED_NO_IT, med.MED_NODE, med.MED_NO_GEOTYPE,
						med.MED_COORDINATE, med.MED_NO_CMODE)
except RuntimeError as ex :
	print("ERROR : number of nodes ...\n%s" % ex)
	doCleanup(fid, -1)

#  ... we know that we only have MED_TRIA3 and MED_QUAD4 in the mesh,
#  a real code would check all MED geometry cell types ...

#  read how many triangular cells in the mesh
try :
	ntria3, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
						med.MED_NO_IT, med.MED_CELL, med.MED_TRIA3,
						med.MED_CONNECTIVITY, med.MED_NODAL)
except RuntimeError as ex :
	print("ERROR : number of MED_TRIA3 ...\n%s" % ex)
	doCleanup(fid, -1)

#  read how many quadrangular cells in the mesh
try :
	nquad4, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
						med.MED_NO_IT, med.MED_CELL, med.MED_QUAD4,
			    		med.MED_CONNECTIVITY, med.MED_NODAL)
except RuntimeError as ex :
	print("ERROR : number of MED_QUAD4 ...\n%s" % ex)
	doCleanup(fid, -1)

#  read mesh nodes coordinates in the initial mesh
coordinates = med.MEDFLOAT([0.] * nnodes * spacedim)
try :
	med.MEDmeshNodeCoordinateRd(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
								med.MED_FULL_INTERLACE, coordinates)
except RuntimeError as ex :
	print("ERROR : initial nodes coordinates ...\n%s" % ex)
	doCleanup(fid, -1)

#  read cells connectivity in the mesh in the initial mesh
triaconnectivity = med.MEDINT([0] * ntria3 * 3)
try :
	med.MEDmeshElementConnectivityRd(fid, meshname, med.MED_NO_DT,
						med.MED_NO_IT, med.MED_CELL, med.MED_TRIA3,
						med.MED_NODAL, med.MED_FULL_INTERLACE, triaconnectivity)
except RuntimeError as ex :
	print("ERROR : MED_TRIA3 connectivity ...\n%s" % ex)
	doCleanup(fid, -1)

quadconnectivity = med.MEDINT([0] * nquad4 * 4)
try :
	med.MEDmeshElementConnectivityRd(fid, meshname, med.MED_NO_DT,
					med.MED_NO_IT, med.MED_CELL, med.MED_QUAD4,
					med.MED_NODAL, med.MED_FULL_INTERLACE, quadconnectivity)
except RuntimeError as ex :
	print("ERROR : MED_QUAD4 connectivity ...\n%s" % ex)
	doCleanup(fid, -1)

#  ... we know that the family number of nodes and elements is 0, a real code would check ...

#  read nodes coordinates changements step by step

for it in range(1, nstep) :

	try :
		numdt, numit, dt = med.MEDmeshComputationStepInfo(fid, meshname, it + 1)
	except RuntimeError as ex :
		print("ERROR : Computing step info ...\n%s" % ex)
		doCleanup(fid, -1)

	#  test changement : for nodes coordinates
	try :
		entityinfo = med.MEDmeshnEntityWithProfile(fid, meshname, numdt,
					numit, med.MED_NODE, med.MED_NO_GEOTYPE,
					med.MED_COORDINATE, med.MED_NO_CMODE, med.MED_GLOBAL_STMODE)
	except RuntimeError as ex :
		print("ERROR : nodes coordinates test changement ...\n%s" % ex)
		doCleanup(fid, -1)

	if (not entityinfo) or (len(entityinfo) != 5) :
		print("ERROR : nodes coordinates test changement return ...")
		doCleanup(fid, -1)
	else :
		nnodes,profilename,profilesize,chgt,trsf = entityinfo

	#  if coordinates have changed, then read the new coordinates
	if (chgt) :
		try :
			med.MEDmeshNodeCoordinateWithProfileRd(fid, meshname, numdt, numit,
					med.MED_GLOBAL_STMODE, profilename, med.MED_FULL_INTERLACE,
					med.MED_ALL_CONSTITUENT, coordinates)
		except RuntimeError as ex :
			print("ERROR : nodes coordinates changement ...\n%s" % ex)
			doCleanup(fid, -1)

#  close MED file
doCleanup(fid, 0)