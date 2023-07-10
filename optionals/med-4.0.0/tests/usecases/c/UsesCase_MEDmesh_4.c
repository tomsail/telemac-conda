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
 *  How to create a structured mesh
 *
 *  Use case 4 :  write a 2D structured mesh (5x3 cartesian grid)
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt         fid;
  const char      meshname[MED_NAME_SIZE+1] = "2D structured mesh";
  const med_int   spacedim = 2;
  const med_int   meshdim  = 2;
  const char      axisname[2*MED_SNAME_SIZE+1] = "x               y               ";
  const char      unitname[2*MED_SNAME_SIZE+1] = "cm              cm              ";
  med_int         axis, size ;
  const med_float cooXaxis[5] = {1.,2.,3.,4.,5.};
  const med_float cooYaxis[3] = {1.,2.,3.};
  const med_int   nquad4 = 8;
  /*                                           12345678901234561234567890123456123456789012345612345678901234561234567890123456123456789012345612345678901234561234567890123456 */
  const char cellsnames[8*MED_SNAME_SIZE+1] = "CELL_1          CELL_2          CELL_3          CELL_4          CELL_5          CELL_6          CELL_7          CELL_8          ";
  const char     familyname   [MED_NAME_SIZE+1]  = "CART_GRID_QUAD_FAMILY";
  const char     groupname    [MED_LNAME_SIZE+1] = "CART_GRID_GROUP";
  const med_int  familynumbers[8]           = { -1, -1, -1, -1, -1, -1, -1, -1 };
  int ret=-1;

  /* MED file creation */
  fid = MEDfileOpen("UsesCase_MEDmesh_4.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }

  /* create the structured mesh in the MED file */
  if (MEDmeshCr(fid, meshname, spacedim, meshdim, MED_STRUCTURED_MESH,
 		"A 2D structured mesh","",MED_SORT_DTIT,
		MED_CARTESIAN, axisname, unitname) < 0) {
    MESSAGE("ERROR : mesh creation ...");
    goto ERROR;
  }

  /* specify the grid type : MED_CARTESIAN_GRID */
   if (MEDmeshGridTypeWr(fid,meshname, MED_CARTESIAN_GRID) < 0) {
    MESSAGE("ERROR : write grid type ...");
    goto ERROR;
  }

   /* write axis "X" and "Y" coordinates */
   axis = 1;
   size = 5;
   if (MEDmeshGridIndexCoordinateWr(fid, meshname, MED_NO_DT, MED_NO_IT,0.0,
				    axis, size, cooXaxis) < 0) {
     MESSAGE("ERROR : write of axis X coordinates ...");
     goto ERROR;
   }
  axis++;
  size = 3;
  if (MEDmeshGridIndexCoordinateWr(fid, meshname, MED_NO_DT, MED_NO_IT,0.0,
				   axis, size, cooYaxis) < 0) {
    MESSAGE("ERROR : write of axis Y coordinates ...");
    goto ERROR;
  }

  /* optionnal : names for nodes or elements */
  /* In this case, a name is given to the cells of the mesh */
  if (MEDmeshEntityNameWr(fid, meshname, MED_NO_DT, MED_NO_IT,
			  MED_CELL, MED_QUAD4, nquad4, cellsnames) < 0) {
    MESSAGE("ERROR : cells names  ...");
    goto ERROR;
  }

  /* We have to create family 0 : by default, all mesh entities family number is 0 */
  if (MEDfamilyCr(fid, meshname, MED_NO_NAME, 0, 0, MED_NO_GROUP) < 0) {
    MESSAGE("ERROR : family 0 creation ...");
    goto ERROR;
  }

  /* We decide to create a family for boundary quad4 :
     by convention a nodes family number is > 0 and an element family number is < 0 */
  if (MEDfamilyCr(fid, meshname,familyname, 1, -1, groupname) < 0) {
    MESSAGE("ERROR : family creation ...");
    goto ERROR;
  }

  /* Then we write family number for quad4 */
  if (MEDmeshEntityFamilyNumberWr(fid, meshname, MED_NO_DT, MED_NO_IT,
				  MED_CELL, MED_QUAD4, nquad4, familynumbers) < 0) {
    MESSAGE("ERROR : nodes family numbers ...");
    goto ERROR;
  }

  ret = 0;
 ERROR:

  /* close MED file  */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file ...");
    ret = -1;
  }

  return ret;
}

