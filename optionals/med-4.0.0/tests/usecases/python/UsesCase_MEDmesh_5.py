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
#  *  Mesh Use case 5 : read a 2D structured mesh
#  *                    5x3 cartesian grid
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

#  open MED file
try :
	fid = med.MEDfileOpen("UsesCase_MEDmesh_4.med", med.MED_ACC_RDONLY)
except RuntimeError as ex :
	print("ERROR : open file in READ ONLY ACCESS mode ...\n%s" % ex)
	sys.exit(-1)

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

#  read the grid type : MED_CARTESIAN_GRID or MED_CURVILINEAR_GRID
try :
	gridtype = med.MEDmeshGridTypeRd(fid, meshname)
except RuntimeError as ex :
	print("ERROR : read grid type ...\n%s" % ex)
	doCleanup(fid, -1)

#  ... we know that the mesh is a cartesian grid,
#  a real code would check ...

#  read the axis coordinates (MED_CARTESIAN coordinates system)
#  X
axis = 1
try :
	size, chgt, trsf = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
						med.MED_NO_IT, med.MED_NODE, med.MED_NONE,
						med.MED_COORDINATE_AXIS1, med.MED_NO_CMODE)
except RuntimeError as ex :
	print("ERROR : number of coordinates on X axis ...\n%s" % ex)
	doCleanup(fid, -1)
ncell = size - 1
cooXaxis = med.MEDFLOAT([0.] * size)

try :
	med.MEDmeshGridIndexCoordinateRd(fid, meshname,
						med.MED_NO_DT, med.MED_NO_IT, axis, cooXaxis)
except RuntimeError as ex :
	print("ERROR : read axis X coordinates ...\n%s" % ex)
	doCleanup(fid, -1)

#  Y
axis = 2
try :
	size2, chgt2, trsf2 = med.MEDmeshnEntity(fid, meshname, med.MED_NO_DT,
						med.MED_NO_IT, med.MED_NODE, med.MED_NONE,
						med.MED_COORDINATE_AXIS2, med.MED_NO_CMODE)
except RuntimeError as ex :
	print("ERROR : number of coordinates on Y axis ...\n%s" % ex)
	doCleanup(fid, -1)
ncell = ncell * (size2 - 1)
cooYaxis = med.MEDFLOAT([0.] * size2)

try :
	med.MEDmeshGridIndexCoordinateRd(fid, meshname,
						med.MED_NO_DT, med.MED_NO_IT, axis, cooYaxis)
except RuntimeError as ex :
	print("ERROR : read axis Y coordinates ...\n%s" % ex)
	doCleanup(fid, -1)

#  read cells name

cellsname = med.MEDCHAR(["\0"] * (ncell * med.MED_SNAME_SIZE + 1))
try :
	med.MEDmeshEntityNameRd(fid, meshname, med.MED_NO_DT, med.MED_NO_IT,
							med.MED_CELL, med.MED_QUAD4, cellsname)
except RuntimeError as ex :
	print("ERROR : read cells name ...\n%s" % ex)
	doCleanup(fid, -1)

#  close MED file
doCleanup(fid, 0)