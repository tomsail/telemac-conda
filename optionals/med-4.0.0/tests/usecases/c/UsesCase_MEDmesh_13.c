/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
 *  MED is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MED is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with MED.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 *  Use case 13 : a 2D unstructured mesh with 10 nodes and 2 polygons
 * poly1 : 1,4,7,9,6,3
 * poly2 : 2,5,8,10,7,4
 *      9   10
 *
 *   6    7    8
 *
 *   3    4    5
 *
 *      1     2
 *
*/

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  const char meshname[MED_NAME_SIZE+1] = "2D unstructured mesh";
  const med_int spacedim = 2;
  const med_int meshdim = 2;
  /*                                         12345678901234561234567890123456 */
  const char axisname[2*MED_SNAME_SIZE+1] = "x               y               ";
  const char unitname[2*MED_SNAME_SIZE+1] = "cm              cm              ";
  /* Dix noeuds dont deux communs aux deux polygones */
  const med_float coordinates[2*10] = { 0.5,   0.,
					1.5,   0.,
					0.,    0.5,
					1.,    0.5,
					2.,    0.5,
					0.,    1.,
					1.,    1.,
					2.,    1.,
					0.5,   2.,
					1.5,   2. };
  const med_int nnodes    = 10;
  const med_int indexsize = 3;
  const med_int index[3]  = {1,7,13};
  /* connectivity : 2 hexagons */
  const med_int connectivity[12] = {1,4,7,9,6,3,
				    2,5,8,10,7,4};
  int ret=-1;

  /* open MED file */
  fid = MEDfileOpen("UsesCase_MEDmesh_13.med", MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }

  /* write a comment in the file */
  if (MEDfileCommentWr(fid, "A 2D unstructured mesh : 12, 12 polygons") < 0)
    { MESSAGE("ERROR : write file description ...");
      goto ERROR;
    }

  /* mesh creation : a 2D unstructured mesh */
  if (MEDmeshCr(fid, meshname, spacedim, meshdim,
		MED_UNSTRUCTURED_MESH, "A 2D mesh with 2 polygons",
		"", MED_SORT_DTIT, MED_CARTESIAN, axisname, unitname) < 0)
    { MESSAGE("ERROR : mesh creation ...");
      goto ERROR;
    }

  /* nodes coordinates in a cartesian axis in full interlace mode
     (X1,Y1, X2,Y2, X3,Y3, ...) with no iteration and computation step
  */
  if (MEDmeshNodeCoordinateWr(fid, meshname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
			      MED_FULL_INTERLACE, nnodes, coordinates) < 0)
    { MESSAGE("ERROR : nodes coordinates ...");
      goto ERROR;
    }

  /* cells connectiviy is defined in nodal mode */
  /* 2 polygons */
  if (MEDmeshPolygonWr(fid, meshname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
		       MED_CELL, MED_NODAL, indexsize, index, connectivity) < 0)
    { MESSAGE("ERROR : polygon connectivity ...");
      goto ERROR;
    }


  /* create family 0 : by default, all mesh entities family number is 0 */
  if (MEDfamilyCr(fid, meshname, MED_NO_NAME, 0, 0, MED_NO_GROUP) < 0)
    { MESSAGE("ERROR : family 0 creation ...");
      goto ERROR;
    }

  ret=0;
 ERROR:

  /* close MED file */
  if (MEDfileClose(fid)  < 0) {
    MESSAGE("ERROR : close file ...");
    ret=-1;
  }

  return ret;
}
