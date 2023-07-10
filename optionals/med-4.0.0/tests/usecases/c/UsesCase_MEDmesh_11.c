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
 *  Use case 11 : read a 2D unstructured mesh with 15 nodes, 8 triangular cells, 4 quadragular cells with
 *  nodes families
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  const char meshname[MED_NAME_SIZE+1] = "2D unstructured mesh";
  char meshdescription[MED_COMMENT_SIZE+1]="";
  med_int meshdim;
  med_int spacedim;
  med_sorting_type sortingtype;
  med_int nstep;
  med_mesh_type meshtype;
  med_axis_type axistype;
  char axisname[2*MED_SNAME_SIZE+1]="";
  char unitname[2*MED_SNAME_SIZE+1]="";
  char dtunit[MED_SNAME_SIZE+1];
  med_float *coordinates = NULL;
  med_int nnodes = 0;
  med_int *triaconnectivity = NULL;
  med_int ntria3 = 0;
  med_int *quadconnectivity = NULL;
  med_int nquad4 = 0;
  med_bool coordinatechangement;
  med_bool geotransformation;
  int i;
  med_int nfamily, ngroup;
  med_int familynumber;
  char *groupname=NULL;
  char familyname[MED_NAME_SIZE+1]="";
  med_int *familynumbers = NULL;
  med_int nfamilynumber = 0;
  int ret=-1;

  /* open MED file with READ ONLY access mode */
  fid = MEDfileOpen("UsesCase_MEDmesh_10.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : open file in READ ONLY ACCESS mode ...");
    goto ERROR;
  }

  /*
   * ... we know that the MED file has only one mesh,
   * a real code would check ...
   */

  /* read mesh informations : mesh dimension, space dimension ... */
  if (MEDmeshInfoByName(fid, meshname, &spacedim, &meshdim, &meshtype, meshdescription,
			dtunit, &sortingtype, &nstep, &axistype, axisname, unitname) < 0) {
    MESSAGE("ERROR : mesh info ...");
    goto ERROR;
  }

  /* read how many nodes in the mesh */
  if ((nnodes = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_NO_GEOTYPE,
			       MED_COORDINATE, MED_NO_CMODE,&coordinatechangement,
			       &geotransformation)) < 0) {
    MESSAGE("ERROR : number of nodes ...");
    goto ERROR;
  }

  /*
   * ... we know that we only have MED_TRIA3 and MED_QUAD4 in the mesh,
   * a real code would check all MED geometry cell types ...
   */

  /* read how many triangular cells in the mesh */
  if ((ntria3 = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT, MED_CELL,MED_TRIA3,
			       MED_CONNECTIVITY, MED_NODAL,&coordinatechangement,
			       &geotransformation)) < 0) {
    MESSAGE("ERROR : number of MED_TRIA3 ...");
    goto ERROR;
  }

  /* read how many quadrangular cells in the mesh */
  if ((nquad4 = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT, MED_CELL,MED_QUAD4,
			       MED_CONNECTIVITY, MED_NODAL, &coordinatechangement,
			       &geotransformation)) < 0) {
    MESSAGE("ERROR : number of MED_QUAD4 ...");
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

  free(coordinates);

  /* read cells connectivity in the mesh */
  if ((triaconnectivity = (med_int *) malloc(sizeof(med_int)*ntria3*3)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }
  if (MEDmeshElementConnectivityRd(fid, meshname, MED_NO_DT, MED_NO_IT, MED_CELL,
				   MED_TRIA3, MED_NODAL, MED_FULL_INTERLACE, triaconnectivity) < 0) {
    MESSAGE("ERROR : MED_TRIA3 connectivity ...");
    free(triaconnectivity);
    goto ERROR;
  }
  free(triaconnectivity);

  if ((quadconnectivity = (med_int *) malloc(sizeof(med_int)*nquad4*4)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }
  if (MEDmeshElementConnectivityRd(fid, meshname, MED_NO_DT, MED_NO_IT, MED_CELL,
				   MED_QUAD4, MED_NODAL, MED_FULL_INTERLACE, quadconnectivity) < 0) {
    MESSAGE("ERROR : MED_QUAD4 connectivity ...");
    free(quadconnectivity);
    goto ERROR;
  }
  free(quadconnectivity);

  /*
   * read families of entities...
   */
  if ((nfamily = MEDnFamily(fid,meshname)) < 0) {
    MESSAGE("ERROR : read number of family ...");
    goto ERROR;
  }

  for (i=0; i<nfamily ; i++) {

    if ((ngroup = MEDnFamilyGroup(fid, meshname, i+1)) < 0) {
      MESSAGE("ERROR : read number of group in a family ...");
      goto ERROR;
    }

    if (ngroup > 0) {
      if ((groupname = (char*) malloc(sizeof(char)*MED_LNAME_SIZE*ngroup+1)) == NULL) {
	MESSAGE("ERROR : memory allocation ...");
	goto ERROR;
      }

      if (MEDfamilyInfo(fid, meshname, i+1, familyname, &familynumber, groupname) < 0) {
	MESSAGE("ERROR : family info ...");
	free(groupname);
	goto ERROR;
      }
      free(groupname);
    }

  }

  /* check for family numbers */
  /* By convention, if there is no numbers in the file, it means that 0 is the family
      number of all nodes */
  if ((nfamilynumber = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT,
          MED_NODE, MED_NONE, MED_FAMILY_NUMBER, MED_NO_CMODE,
          &coordinatechangement, &geotransformation)) < 0) {
    MESSAGE("ERROR : check family numbers nodes ...");
    goto ERROR;
  }

  if ((familynumbers = (med_int *) malloc(sizeof(med_int)*nnodes)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }

  if (nfamilynumber > 0) {
    /* read family numbers for nodes */
    if (MEDmeshEntityFamilyNumberRd(fid, meshname, MED_NO_DT, MED_NO_IT,
  				  MED_NODE, MED_NONE, familynumbers) < 0) {
      MESSAGE("ERROR : read family numbers nodes ...");
      goto ERROR;
    }
  } else
    for (i=0; i<nnodes; i++) *(familynumbers+i) = 0;

  for (i=0; i<nnodes; i++) {
    printf("%d", *(familynumbers+i));
    if (i+1 != nnodes)
      printf(" - ");
    else
      printf("\n");
  }

  if (familynumbers)
    free(familynumbers);

  /* read family numbers for cells */

  if ((nfamilynumber = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT,
          MED_CELL, MED_TRIA3, MED_FAMILY_NUMBER, MED_NODAL,
          &coordinatechangement, &geotransformation)) < 0) {
    MESSAGE("ERROR : check family number tria3 ...")
    goto ERROR;
  }

  if ((familynumbers = (med_int *) malloc(sizeof(med_int)*ntria3)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }

  if (nfamilynumber > 0) {
    if (MEDmeshEntityFamilyNumberRd(fid, meshname, MED_NO_DT, MED_NO_IT,
  				  MED_CELL, MED_TRIA3, familynumbers) < 0) {
      MESSAGE("ERROR : read family numbers tria3 ...");
    }
  } else
    for (i=0; i<ntria3; i++) *(familynumbers+i) = 0;

  free (familynumbers);

  if ((nfamilynumber = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT,
            MED_CELL, MED_QUAD4, MED_FAMILY_NUMBER, MED_NODAL,
            &coordinatechangement, &geotransformation)) < 0) {
    MESSAGE("ERROR : check family number quad4 ...");
    goto ERROR;
  }

  if ((familynumbers = (med_int *) malloc(sizeof(med_int)*nquad4)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }

  if (nfamilynumber > 0) {
    if (MEDmeshEntityFamilyNumberRd(fid, meshname, MED_NO_DT, MED_NO_IT,
  				  MED_CELL, MED_QUAD4, familynumbers) < 0) {
      MESSAGE("ERROR : read family numbers quad4 ...");
      goto ERROR;
    }
  } else
    for (i=0; i<nquad4; i++) *(familynumbers+i) = 0;

  free (familynumbers);

  ret=0;
 ERROR:

  /* close MED file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file");
    ret= -1;
  }

  return ret;
}
