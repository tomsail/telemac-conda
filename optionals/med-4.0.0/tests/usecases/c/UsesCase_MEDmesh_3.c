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
 *  Use case 3 : read an unstructured mesh : generic approach
 */


#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  char meshname[MED_NAME_SIZE+1]="";
  char meshdescription[MED_COMMENT_SIZE+1]="";
  med_int meshdim=0;
  med_int spacedim=0;
  med_sorting_type sortingtype;
  med_int nstep;
  med_mesh_type meshtype;
  med_axis_type axistype;
  char *axisname="";
  char *unitname="";
  char dtunit[MED_SNAME_SIZE+1]="";
  med_float *coordinates = NULL;
  med_int nnodes = 0;
  med_int ngeo = 0;
  med_int nelt=0;
  med_int *connectivity = NULL;
  med_bool coordinatechangement;
  med_bool geotransformation;
  med_int i, j, it, nmesh;
  med_geometry_type geotype;
  med_geometry_type *geotypes = MED_GET_CELL_GEOMETRY_TYPE;
  char geotypename[MED_NAME_SIZE+1];
  int ret=-1;


  /* open MED file with READ ONLY access mode */
  fid = MEDfileOpen("UsesCase_MEDmesh_1.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : open file in READ ONLY ACCESS mode ...");
    goto ERROR;
  }

  /* read how many mesh in the file */
  if ((nmesh = MEDnMesh(fid)) < 0) {
    MESSAGE("ERROR : read how many mesh ...");
    goto ERROR;
  }

  for (i=0;i<nmesh;i++) {

    /* read computation space dimension */
    if ((spacedim = MEDmeshnAxis(fid, i+1)) < 0) {
      MESSAGE("ERROR : read computation space dimension ...");
      goto ERROR;
    }

    /* memory allocation */
    if ((axisname  = (char*) malloc(MED_SNAME_SIZE*spacedim+1)) == NULL) {
      MESSAGE("ERROR : memory allocation ...");
      goto ERROR;
    }
    if ((unitname  = (char*) malloc(MED_SNAME_SIZE*spacedim+1)) == NULL) {
      MESSAGE("ERROR : memory allocation ...");
      goto ERROR;
    }

    /* read mesh informations : meshname, mesh dimension, mesh type ... */
    if (MEDmeshInfo(fid, i+1, meshname, &spacedim, &meshdim, &meshtype, meshdescription,
		    dtunit, &sortingtype, &nstep, &axistype, axisname, unitname) < 0) {
      MESSAGE("ERROR : mesh info ...");
      free(axisname); free(unitname);
      goto ERROR;
    }

    free(axisname);
    free(unitname);

    /* read how many nodes in the mesh */
    if ((nnodes = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_NO_GEOTYPE,
				 MED_COORDINATE, MED_NO_CMODE,&coordinatechangement,
				 &geotransformation)) < 0) {
      MESSAGE("ERROR : number of nodes ...");
    goto ERROR;
    }

    /* read mesh nodes coordinates */
    if ((coordinates = (med_float*) malloc(sizeof(med_float)*nnodes*spacedim)) == NULL) {
      MESSAGE("ERROR : memory allocation ...");
      goto ERROR;
    }

    if (MEDmeshNodeCoordinateRd(fid, meshname, MED_NO_DT, MED_NO_IT, MED_FULL_INTERLACE,
				coordinates) < 0) {
      MESSAGE("ERROR : nodes coordinates ...");
      free(coordinates);
      goto ERROR;
    }

    if (coordinates)
      free(coordinates);

    /* read number of geometrical types for cells */
    if ((ngeo = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT, MED_CELL,MED_GEO_ALL,
			       MED_CONNECTIVITY, MED_NODAL,&coordinatechangement,
			       &geotransformation)) < 0) {
      MESSAGE("ERROR : number of geo type ...");
      ISCRUTE(geotype);
      goto ERROR;
    }

    for (it=1; it<=ngeo; it++) {

      /* get geometry type */
      if (MEDmeshEntityInfo(fid, meshname, MED_NO_DT, MED_NO_IT,MED_CELL,it,
				   geotypename,&geotype) < 0) {
	MESSAGE("ERROR : get geo type ...");
	ISCRUTE(it);
	goto ERROR;
      }

      /* how many cells of type geotype ? */
      if ((nelt = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT, MED_CELL,geotype,
				 MED_CONNECTIVITY, MED_NODAL,&coordinatechangement,
				   &geotransformation)) < 0) {
	MESSAGE("ERROR : number of cell ...");
	goto ERROR;
      }

      /* read cells connectivity in the mesh */
      if ((connectivity = (med_int *) malloc(sizeof(med_int)*nelt*(geotype%100))) == NULL) {
	MESSAGE("ERROR : memory allocation ...");
	goto ERROR;
      }

      if (MEDmeshElementConnectivityRd(fid, meshname, MED_NO_DT, MED_NO_IT, MED_CELL,
				       geotype, MED_NODAL, MED_FULL_INTERLACE, connectivity) < 0) {
	MESSAGE("ERROR : cell connectivity ...");
	ISCRUTE(geotype);
	free(connectivity);
	goto ERROR;
      }

      /* memory deallocation */
      if (connectivity) {
	free(connectivity);
	connectivity = NULL;
      }


    }

  }

  /*
   * ... we know that the family number of nodes and elements is 0, a real code would check ...
   */
  ret=0;
 ERROR:

  /* close MED file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file");
    ret=-1;
  }

  return ret;
}

