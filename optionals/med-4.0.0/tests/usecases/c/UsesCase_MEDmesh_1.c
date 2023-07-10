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
 *  How to create an unstructured mesh
 *
 *  Use case 1 : a 2D unstructured mesh with 15 nodes, 8 triangular cells, 4 triangular cells
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
  const med_float coordinates[30] = { 2.,1.,  7.,1.,  12.,1.,  17.,1.,  22.,1.,
				      2.,6.,  7.,6.,  12.,6.,  17.,6.,  22.,6.,
				      2.,11., 7.,11., 12.,11., 17.,11., 22.,11.};
  const med_int nnodes = 15;
  const med_int triaconnectivity[24] = { 1,7,6,   2,7,1,  3,7,2,   8,7,3,
					 13,7,8, 12,7,13, 11,7,12, 6,7,11 };
  const med_int ntria3 = 8;
  const med_int quadconnectivity[16] = {3,4,9,8,    4,5,10,9,
					15,14,9,10, 13,8,9,14};
  const med_int nquad4 = 4;
  med_err ret=-1;

  /* open MED file */
  fid = MEDfileOpen("UsesCase_MEDmesh_1.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }

  /* write a comment in the file */
  if (MEDfileCommentWr(fid,"A 2D unstructured mesh : 15 nodes, 12 cells") < 0) {
    MESSAGE("ERROR : write file description ...");
    goto ERROR;
  }

  /* mesh creation : a 2D unstructured mesh */
  if (MEDmeshCr(fid, meshname, spacedim, meshdim, MED_UNSTRUCTURED_MESH,
		"A 2D unstructured mesh","",MED_SORT_DTIT,MED_CARTESIAN, axisname, unitname) < 0) {
    MESSAGE("ERROR : mesh creation ...");
    goto ERROR;
  }

  /* nodes coordinates in a cartesian axis in full interlace mode
     (X1,Y1, X2,Y2, X3,Y3, ...) with no iteration and computation step
  */
  if (MEDmeshNodeCoordinateWr(fid, meshname, MED_NO_DT, MED_NO_IT, 0.0,
			      MED_FULL_INTERLACE, nnodes, coordinates) < 0) {
    MESSAGE("ERROR : nodes coordinates ...");
    goto ERROR;
  }

  /* cells connectiviy is defined in nodal mode with no iteration and computation step */
  if (MEDmeshElementConnectivityWr(fid, meshname, MED_NO_DT, MED_NO_IT, 0.0, MED_CELL, MED_TRIA3,
				   MED_NODAL, MED_FULL_INTERLACE, ntria3, triaconnectivity) < 0) {
    MESSAGE("ERROR : triangular cells connectivity ...");
    goto ERROR;
  }
  if (MEDmeshElementConnectivityWr(fid, meshname, MED_NO_DT, MED_NO_IT, 0.0, MED_CELL, MED_QUAD4,
				   MED_NODAL, MED_FULL_INTERLACE, nquad4, quadconnectivity) < 0) {
    MESSAGE("ERROR : quadrangular cells connectivity ...");
    goto ERROR;
  }

  /* create family 0 : by default, all mesh entities family number is 0 */
  if (MEDfamilyCr(fid, meshname,MED_NO_NAME, 0, 0, MED_NO_GROUP) < 0) {
    MESSAGE("ERROR : family 0 creation ...");
    goto ERROR;
  }

  ret = 0;
 ERROR :

  /* close MED file */
  if (MEDfileClose(fid)  < 0) {
    MESSAGE("ERROR : close file ...");
    return -1;
  }

  return ret;
}
