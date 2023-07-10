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
#  *  Use case 3 : read an unstructured mesh : generic approach
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

#  open MED file with READ ONLY access mode
try :
	fid = med.MEDfileOpen("UsesCase_MEDmesh_1.med", med.MED_ACC_RDONLY)
except RuntimeError as ex :
	print("ERROR : open file in READ ONLY ACCESS mode ...\n%s" % ex)
	sys.exit(-1)

#  read how many mesh in the file
try :
	nmesh = med.MEDnMesh(fid)
except RuntimeError as ex :
	print("ERROR : read how many mesh ...\n%s" % ex)
	doCleanup(fid, -1)

for i in range(0, nmesh) :

	#  read computation space dimension
	try :
		spacedim = med.MEDmeshnAxis(fid, i+1)
	except RuntimeError as ex :
		print("ERROR : read computation space dimension ...\n%s" % ex)
		doCleanup(fid, -1)

	#  read mesh informations : meshname, mesh dimension, mesh type ...
	try :
		meshinfo = med.MEDmeshInfo(fid, i+1)
	except RuntimeError as ex :
		print("ERROR : mesh info ...\n%s" % ex)
		doCleanup(fid, -1)

	if (not meshinfo) or (len(meshinfo) != 11) :
		print("ERROR : no mesh info created from MEDmeshInfo")
		doCleanup(fid, -1)
	else :
		meshname,spacedim,meshdim,meshtype,description,dtunit,sortingtype,nstep,axistype,axisname,axisunit=meshinfo

	#  read how many nodes in the mesh
	try :
		nnodes, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
							med.MED_NO_IT, med.MED_NODE, med.MED_NO_GEOTYPE,
							med.MED_COORDINATE, med.MED_NO_CMODE)
	except RuntimeError as ex :
		print("ERROR : number of nodes ...\n%s" % ex)
		doCleanup(fid, -1)

	#  read mesh nodes coordinates
	coordinates = med.MEDFLOAT([0.] * nnodes * spacedim)
	try :
		med.MEDmeshNodeCoordinateRd(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
									med.MED_FULL_INTERLACE, coordinates)
	except RuntimeError as ex :
		print("ERROR : nodes coordinates ...\n%s" % ex)
		doCleanup(fid, -1)

	#  read number of geometrical types for cells
	try:
		ngeo, chgt2, trsf2 = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
							med.MED_NO_IT, med.MED_CELL, med.MED_GEO_ALL,
							med.MED_CONNECTIVITY, med.MED_NODAL)
	except RuntimeError as ex :
		print("ERROR : number of geo types ...\n%s" % ex)
		doCleanup(fid, -1)

	for it in range(1, ngeo + 1) :

		#  get geometry type
		try :
			geotypename,geotype = med.MEDmeshEntityInfo(fid, meshname, med.MED_NO_DT,
								med.MED_NO_IT, med.MED_CELL, it )
		except RuntimeError as ex :
			print("ERROR : get geo type ...\n%s" % ex)
			doCleanup(fid, -1)

		#  how many cells of type geotype ?
		try :
			nelt, chgt3, trsf3 = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
								med.MED_NO_IT, med.MED_CELL, geotype,
								med.MED_CONNECTIVITY, med.MED_NODAL)
		except RuntimeError as ex :
			print("ERROR : number of cell of type geotype ...\n%s" % ex)
			doCleanup(fid, -1)

		#  read cells connectivity in the mesh
		connectivity = med.MEDINT([0] * nelt * (geotype % 100))
		try :
			med.MEDmeshElementConnectivityRd(fid, meshname, med.MED_NO_DT,
							med.MED_NO_IT, med.MED_CELL, geotype,
							med.MED_NODAL, med.MED_FULL_INTERLACE, connectivity)
		except RuntimeError as ex :
			print("ERROR : cell connectivity ...\n%s" % ex)
			doCleanup(fid, -1)

#  ... we know that the family number of nodes and elements is 0, a real code would check ...

#  close MED file
doCleanup(fid, 0)