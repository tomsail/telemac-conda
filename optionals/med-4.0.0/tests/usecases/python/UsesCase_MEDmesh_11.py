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
#  *  Use case 11 : read a 2D unstructured mesh with 15 nodes, 8 triangular cells, 4 quadragular cells with
#  *  nodes families
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
	fid = med.MEDfileOpen("UsesCase_MEDmesh_10.med", med.MED_ACC_RDONLY)
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

#  read mesh nodes coordinates
coordinates = med.MEDFLOAT([0.] * nnodes * spacedim)
try :
	med.MEDmeshNodeCoordinateRd(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
								med.MED_FULL_INTERLACE, coordinates)
except RuntimeError as ex :
	print("ERROR : nodes coordinates ...\n%s" % ex)
	doCleanup(fid, -1)

#  read cells connectivity in the mesh
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

#  read families of entities...
try :
	nfamily = med.MEDnFamily(fid, meshname)
except RuntimeError as ex :
	print("ERROR : read number of family ...\n%s" % ex)
	doCleanup(fid, -1)

for i in range(0, nfamily) :

	try :
		ngroup = med.MEDnFamilyGroup(fid, meshname, i+1)
	except RuntimeError as ex :
		print("ERROR : read number of group in a family ...\n%s" % ex)
		doCleanup(fid, -1)

	if ngroup > 0 :
		groupname = med.MEDCHAR("\0" * (med.MED_LNAME_SIZE*ngroup+1))
		try :
			familyname,familynumber,groupname = med.MEDfamilyInfo(fid,
										meshname, i+1, groupname)
		except RuntimeError as ex :
			print("ERROR : family info ...\n%s" % ex)
			doCleanup(fid, -1)

#  check family numbers for node
#  By convention, if there is no numbers in the file, it means that 0 is the family
#  number of all nodes
familynumbers = med.MEDINT([0] * nnodes)
try :
	nfamilynumbers, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
					med.MED_NO_IT, med.MED_NODE, med.MED_NONE,
					med.MED_FAMILY_NUMBER, med.MED_NO_CMODE)
except RuntimeError as ex :
	print("ERROR : check family numbers nodes ...\n%s" % ex)
	doCleanup(fid, -1)

#  read family numbers for nodes
if nfamilynumbers != 0 :
	try :
		med.MEDmeshEntityFamilyNumberRd(fid, meshname, med.MED_NO_DT,
										med.MED_NO_IT, med.MED_NODE,
										med.MED_NONE, familynumbers)
	except RuntimeError as ex :
		print("ERROR : read family numbers nodes ...\n%s" % ex)
		doCleanup(fid, -1)
else :
	for i in range(0, nnodes) :
		familynumbers[i] = 0

print(familynumbers)

#  read family numbers for cells
familynumbers = med.MEDINT([0] * ntria3)
try :
	nfamilynumbers, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
					med.MED_NO_IT, med.MED_CELL, med.MED_TRIA3,
					med.MED_FAMILY_NUMBER, med.MED_NODAL)
except RuntimeError as ex :
	print("ERROR : check family numbers tria3 ...\n%s" % ex)
	doCleanup(fid, -1)

if nfamilynumbers != 0 :
	try :
		med.MEDmeshEntityFamilyNumberRd(fid, meshname, med.MED_NO_DT,
										med.MED_NO_IT, med.MED_CELL,
										med.MED_TRIA3, familynumbers)
	except RuntimeError as ex :
		print("ERROR : read family numbers tria3 ...\n%s" % ex)
		doCleanup(fid, -1)
else :
	for i in range(0, ntria3) :
		familynumbers[i] = 0

familynumbers = med.MEDINT([0] * nquad4)
try :
	nfamilynumbers, chgt, trsf =  med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
					med.MED_NO_IT, med.MED_CELL, med.MED_QUAD4,
					med.MED_FAMILY_NUMBER, med.MED_NODAL)
except RuntimeError as ex :
	print("ERROR : check family numbers quad4 ...\n%s" % ex)
	doCleanup(fid, -1)

if nfamilynumbers != 0 :
	try :
		med.MEDmeshEntityFamilyNumberRd(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
	  					med.MED_CELL, med.MED_QUAD4, familynumbers)
	except RuntimeError as ex :
		print("ERROR : read family numbers quad4 ...\n%s" % ex)
		doCleanup(fid, -1)

#  close MED file
doCleanup(fid, 0)