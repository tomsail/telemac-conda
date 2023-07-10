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
 *  How to create an unstructured mesh with polyhedrons
 *
 *  Use case 15 : a 3D unstructured mesh with 2 polyhedrons
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  const char meshname[MED_NAME_SIZE+1] = "3D Unstructured Mesh With 2 polyhedrons";
  const med_int spacedim = 3;
  const med_int meshdim  = 3;
  /*                                         12345678901234561234567890123456 */
  const char axisname[3*MED_SNAME_SIZE+1] = "x               y               z               ";
  const char unitname[3*MED_SNAME_SIZE+1] = "cm              cm              cm              ";

  const med_int nnodes = 16;
  const med_float coordinates[3 * 16] = {
/* [     1 ] */  -10.0,  -10.0,  +0.0,
/* [     2 ] */  -10.0,  -10.0,  +10.,
/* [     3 ] */  -10.0,  +10.0,  +10.,
/* [     4 ] */  -10.0,  +10.0,  +0.0,
/* [     5 ] */  +10.0,  -10.0,  +0.0,
/* [     6 ] */  +10.0,  -10.0,  +10.,
/* [     7 ] */  +10.0,  +10.0,  +10.,
/* [     8 ] */  +10.0,  +10.0,  +0.0,
/* [     9 ] */  -10.0,  +0.0 , +10.0,
/* [    10 ] */  -10.0,  +0.0 , +0.0 ,
/* [    11 ] */  +0.0 , -10.0 , +10.0,
/* [    12 ] */  +0.0 , -10.0 , +0.0 ,
/* [    13 ] */  +0.0 , +10.0 , +10.0,
/* [    14 ] */  +10.0,  +0.0 , +10.0,
/* [    15 ] */  +0.0 , +10.0 , +0.0 ,
/* [    16 ] */  +10.0,  +0.0 , +0.0
  };

  /*Il y a 58 numéros de noeuds dans le tableau de connextivité*/
  const med_int connectivity[26+32] = {
/*  - Poly 1 : */
/*  - Face 1 : */  1,  2  ,  9 , 3  , 10,
/*  - Face 2 : */  1,  12 ,  5 , 6  , 11,  2,
/*  - Face 3 : */  2,  11 ,  6 , 3  , 9 ,
/*  - Face 4 : */  3,  6  ,  5 ,
/*  - Face 5 : */  3,  5  , 10 ,
/*  - Face 6 : */  1,  10 ,  5 , 12,
/*  - Poly 2 : */
/*  - Face 1 : */  3,  13 ,  7 , 8  , 15,  4,
/*  - Face 2 : */  3,  4  , 10 ,
/*  - Face 3 : */  4,  15 ,  8 , 16 ,  5,  10,
/*  - Face 4 : */  3,  6  , 14 , 7  , 13,
/*  - Face 5 : */  5,  16 ,  8 , 7  , 14,  6,
/*  - Face 6 : */  3,  10 ,  5 ,
/*  - Face 7 : */  3,  5  ,  6
  };

  /* Il y a deux polyèdres, le premier a 6 faces, le second 7 faces */
  /* La face 7 du Poly 2 utilise 59-56=3 noeuds */
  const med_int nodeindex[6+7+1] = { 1, 6, 12, 17, 20, 23,
				     27, 33, 36, 42, 47, 53, 56, 59};
  /* Il y a un total de 13 faces */
  /* NodeIndexSize == Nbre De Faces +1 */
  const med_int nodeindexSize = 6+7+1;

  /* Il y a deux polyèdres, le premier a 6 faces, le second 7 faces */
  /* Pn+1 == FaceIndex[n+1]== NodeIndexSize== Nbre De Faces +1 */
  const med_int faceindex[3] = {1,7,14};
  const med_int faceindexSize = 3;


  /* open MED file */
  fid = MEDfileOpen("UsesCase_MEDmesh_15.med",
        MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    return -1;
  }

  /* write a comment in the file */
  if (MEDfileCommentWr(fid,
           "A 3D unstructured mesh : 2 polyhedrons") < 0) {
    MESSAGE("ERROR : write file description ...");
    return -1;
  }

  /* mesh creation : a 3D unstructured mesh */
  if (MEDmeshCr(fid, meshname, spacedim, meshdim,
		MED_UNSTRUCTURED_MESH, "A 3D mesh with 2 polyhedron",
		"", MED_SORT_DTIT,
		MED_CARTESIAN, axisname, unitname) < 0) {
    MESSAGE("ERROR : mesh creation ...");
    return -1;
  }

  /* nodes coordinates in a cartesian axis in full interlace mode
     (X1,Y1, X2,Y2, X3,Y3, ...) with no iteration and computation step
  */
  if (MEDmeshNodeCoordinateWr(fid, meshname,
            MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
            MED_FULL_INTERLACE, nnodes, coordinates) < 0) {
    MESSAGE("ERROR : nodes coordinates ...");
    return -1;
  }

  /* cells connectiviy is defined in nodal mode */
  if (MEDmeshPolyhedronWr(fid, meshname,
			  MED_NO_DT, MED_NO_IT, MED_UNDEF_DT,
			  MED_CELL, MED_NODAL,
			  faceindexSize, faceindex,
			  nodeindexSize, nodeindex,
			  connectivity) < 0) {
    MESSAGE("ERROR : polyhedron connectivity ...");
    return -1;
  }

  /* create family 0 : by default, all mesh entities family number is 0 */
  if (MEDfamilyCr(fid, meshname, "", 0, 0, "") < 0) {
    MESSAGE("ERROR : family 0 creation ...");
    return -1;
  }

  /* close MED file */
  if (MEDfileClose(fid)  < 0) {
    MESSAGE("ERROR : close file ...");
    return -1;
  }

  return 0;
}
