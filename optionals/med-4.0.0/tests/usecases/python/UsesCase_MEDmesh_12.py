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
#  *  Use case 12 : read a 2D unstructured mesh with moving grid (generic approach)
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

geotypes = med.MED_GET_CELL_GEOMETRY_TYPE
matrix = med.MEDFLOAT([0.] * 7)

#  open MED file with READ ONLY access mode
try :
	fid = med.MEDfileOpen("UsesCase_MEDmesh_9.med", med.MED_ACC_RDONLY)
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
							med.MED_NO_IT, med.MED_NODE, med.MED_NONE,
							med.MED_COORDINATE, med.MED_NO_CMODE)
	except RuntimeError as ex :
		print("ERROR : number of nodes initial ...\n%s" % ex)
		doCleanup(fid, -1)

	#  read mesh nodes coordinates
	coordinates = med.MEDFLOAT([0.] * nnodes * spacedim)
	try :
		med.MEDmeshNodeCoordinateRd(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
									med.MED_FULL_INTERLACE, coordinates)
	except RuntimeError as ex :
		print("ERROR : nodes coordinates ...\n%s" % ex)
		doCleanup(fid, -1)

	#  read all MED geometry cell types
	for it in range(1, med.MED_N_CELL_FIXED_GEO + 1) :

		geotype = geotypes[it - 1][0]

		#  how many cells
		try :
			ngeo, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
								med.MED_NO_IT, med.MED_CELL, geotype,
								med.MED_CONNECTIVITY, med.MED_NODAL)
		except RuntimeError as ex :
			print("ERROR : number of cell ...\n%s" % ex)
			doCleanup(fid, -1)

		if ngeo :
			#  read cells connectivity in the mesh
			connectivity = med.MEDINT([0] * ngeo * (geotype % 100))
			try :
				med.MEDmeshElementConnectivityRd(fid, meshname, med.MED_NO_DT,
							med.MED_NO_IT, med.MED_CELL, geotype,
							med.MED_NODAL, med.MED_FULL_INTERLACE, connectivity)
			except RuntimeError as ex :
				print("ERROR : cell connectivity ...\n%s" % ex)
				doCleanup(fid, -1)

	#  read nodes coordinates changements step by step
	for it in range(1, nstep) :

		try :
			numdt, numit, dt = med.MEDmeshComputationStepInfo(fid, meshname, it+1)
		except RuntimeError as ex :
			print("ERROR : Computing step info ...\n%s" % ex)
			doCleanup(fid, -1)

		#  test changement : for nodes coordinates
		try :
			entityinfo = med.MEDmeshnEntityWithProfile(fid, meshname, numdt,
						numit, med.MED_NODE, med.MED_NONE, med.MED_COORDINATE,
						med.MED_NO_CMODE, med.MED_GLOBAL_STMODE)
		except RuntimeError as ex :
			print("ERROR : nodes coordinates test changement ...\n%s" % ex)
			doCleanup(fid, -1)

		if (not entityinfo) or (len(entityinfo) != 5) :
			print("ERROR : nodes coordinates test changement return ...")
			doCleanup(fid, -1)
		else :
			nnodes,profilename,profilesize,chgt,trsf = entityinfo

		#  if only coordinates have changed, then read the new coordinates
		if chgt and trsf :
			try :
				med.MEDmeshNodeCoordinateWithProfileRd(fid, meshname, numdt, numit,
						med.MED_GLOBAL_STMODE, profilename, med.MED_FULL_INTERLACE,
						med.MED_ALL_CONSTITUENT, coordinates)
			except RuntimeError as ex :
				print("ERROR : nodes coordinates changement ...\n%s" % ex)
				doCleanup(fid, -1)

		if chgt and not trsf :
			try :
				matrixsize, chgt, trsf = med.MEDmeshnEntity(fid,meshname,numdt,
									numit, med.MED_NODE, med.MED_NONE,
									med.MED_COORDINATE_TRSF, med.MED_NODAL)
			except RuntimeError as ex :
				print("ERROR : read matrix size ...\n%s" % ex)
				doCleanup(fid, -1)
			if matrixsize < 0 :
				print("ERROR : matrix transformation ...")
				doCleanup(fid, -1)

			if trsf :
				try :
					med.MEDmeshNodeCoordinateTrsfRd(fid, meshname, numdt, numit, matrix)
				except RuntimeError as ex :
					print("ERROR : read transformation matrix ...\n%s" % ex)
					doCleanup(fid, -1)

#  close MED file
doCleanup(fid, 0)