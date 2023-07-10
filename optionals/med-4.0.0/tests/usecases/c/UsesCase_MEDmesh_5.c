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
 * Mesh Use case 5 : read a 2D structured mesh
 *                   5x3 cartesian grid
 *
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  const char meshname[MED_NAME_SIZE+1]="2D structured mesh";
  med_int spacedim;
  med_int meshdim;
  char meshdescription[MED_COMMENT_SIZE+1];
  char axisname[2*MED_SNAME_SIZE+1];
  char unitname[2*MED_SNAME_SIZE+1];
  char dtunit[MED_SNAME_SIZE+1];
  med_mesh_type meshtype;
  med_axis_type axistype;
  med_grid_type gridtype;
  med_int axis, size ;
  med_float *cooXaxis = NULL;
  med_float *cooYaxis = NULL;
  med_bool coordinatechangement;
  med_bool geotransformation;
  med_int nstep;
  med_sorting_type sortingtype;
  int j;
  int ret=-1;
  int ncell=0;
  char *cellsname=NULL;

  /* open MED file */
  fid = MEDfileOpen("UsesCase_MEDmesh_4.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : open file ...");
    goto ERROR;
  }

  /* read mesh informations : meshname, mesh dimension, space dimension ... */
  if (MEDmeshInfoByName(fid, meshname, &spacedim, &meshdim, &meshtype, meshdescription,
			dtunit, &sortingtype, &nstep, &axistype, axisname, unitname) < 0) {
    MESSAGE("ERROR : mesh info ...");
    goto ERROR;
  }

  /* read the grid type : MED_CARTESIAN_GRID or MED_CURVILINEAR_GRID */
  if (MEDmeshGridTypeRd(fid, meshname, &gridtype) < 0) {
    MESSAGE("ERROR : read grid type ...");
  }

  /*
   * ... we know that the mesh is a cartesian grid,
   * a real code would check  ...
   */

  /* read the axis coordinates (MED_CARTESIAN coordinates system */
  /* X */
  axis = 1;
  if ((size = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT,
			     MED_NODE, MED_NONE, MED_COORDINATE_AXIS1, MED_NO_CMODE,
			     &coordinatechangement, &geotransformation)) < 0) {
    MESSAGE("ERROR : number of coordinates on X axis ...");
    goto ERROR;
  }
  ncell = size-1;

  if ((cooXaxis = (med_float *) malloc(sizeof(med_float)*size)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }
  if (MEDmeshGridIndexCoordinateRd(fid, meshname, MED_NO_DT, MED_NO_IT,
				   axis, cooXaxis) < 0) {
    MESSAGE("ERROR : read axis X coordinates ...");
    free(cooXaxis);
    goto ERROR;
  }

  free(cooXaxis);

  /* Y */
  axis = 2;
  if ((size = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT,
			     MED_NODE, MED_NONE, MED_COORDINATE_AXIS2, MED_NO_CMODE,
			     &coordinatechangement, &geotransformation)) < 0) {
    MESSAGE("ERROR : number of coordinates on Y axis ...");
    goto ERROR;
  }
  ncell = ncell * (size-1);

  if ((cooYaxis = (med_float *) malloc(sizeof(med_float)*size)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }
  if (MEDmeshGridIndexCoordinateRd(fid, meshname, MED_NO_DT, MED_NO_IT,
				   axis, cooYaxis) < 0) {
    MESSAGE("ERROR : read axis Y coordinates ...");
    free(cooYaxis);
    goto ERROR;
  }

  free(cooYaxis);

  /* read cells name */

  cellsname = (char *) malloc((sizeof(char))*ncell*MED_SNAME_SIZE+1);
  if (MEDmeshEntityNameRd(fid, meshname, MED_NO_DT, MED_NO_IT,
			  MED_CELL, MED_QUAD4, cellsname) < 0)  {
    MESSAGE("ERROR : read cells name ...");
    free(cellsname);
    goto ERROR;
  }
  free(cellsname);

  ret=0;
 ERROR:

  /* close MED file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file ...");
    ret = -1;
  }

  return ret;
}
