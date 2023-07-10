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
 *  Use case 16 : read a 3D unstructured mesh with 2 polyhedrons
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  const char meshname[MED_NAME_SIZE+1] = "3D Unstructured Mesh With 2 polyhedrons";
  char meshdescription[MED_COMMENT_SIZE+1];
  med_int meshdim;
  med_int spacedim;
  med_sorting_type sortingtype;
  med_int nstep;
  med_mesh_type meshtype;
  med_axis_type axistype;
  char axisname[3*MED_SNAME_SIZE+1];
  char unitname[3*MED_SNAME_SIZE+1];
  char dtunit[MED_SNAME_SIZE+1];
  med_float *coordinates = NULL;
  med_int nnodes = 0;
  med_int npoly = 0;
  med_int faceindexsize;
  med_int nodeindexsize;
  med_int *faceindex = NULL;
  med_int *nodeindex = NULL;
  med_int *connectivity = NULL;
  med_int connectivitysize;
  med_bool coordinatechangement;
  med_bool geotransformation;
  int i;
  int k,ind1,ind2;
  int j, jind1,jind2;

  /* open MED file with READ ONLY access mode */
  fid = MEDfileOpen("./UsesCase_MEDmesh_15.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : open file in READ ONLY ACCESS mode ...");
    return -1;
  }

  /*
   * ... we know that the MED file has only one mesh,
   * a real code would check ...
   */

  /* read mesh informations : mesh dimension, space dimension ... */
  if (MEDmeshInfoByName(fid, meshname, &spacedim, &meshdim, &meshtype, meshdescription,
      dtunit, &sortingtype, &nstep, &axistype, axisname, unitname) < 0) {
    MESSAGE("ERROR : mesh info ...");
    return -1;
  }

  /* read how many nodes are in the mesh */
  if ((nnodes = MEDmeshnEntity(fid, meshname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_POINT1,
             MED_COORDINATE, MED_NO_CMODE,&coordinatechangement, &geotransformation)) < 0)
    { MESSAGE("ERROR : number of nodes ...");
      return -1;
    }

  /*
   * ... we know that we only have MED_POLYHEDRON cells in the mesh,
   * a real code would check for MED geometry cell types ...
   */

  /* We read how many polyhedrons are in the mesh (using nodal connectivity mode) */
  /* We get the size of the polyhedrons/face index array.
   * As an index of the face index array give the location of the first face   and so the
   * number of polyhedrons
   */
  if ((faceindexsize = MEDmeshnEntity(fid,meshname,MED_NO_DT,MED_NO_IT,
          MED_CELL,MED_POLYHEDRON,MED_INDEX_FACE,MED_NODAL,
          &coordinatechangement, &geotransformation)) < 0)
    { MESSAGE("ERROR : read number of polyhedrons ...");
      return -1;
    }
  npoly = faceindexsize-1;
  ISCRUTE(npoly);

  if ((nodeindexsize = MEDmeshnEntity(fid,meshname,MED_NO_DT,MED_NO_IT,
          MED_CELL,MED_POLYHEDRON,MED_INDEX_NODE,MED_NODAL,
          &coordinatechangement, &geotransformation)) < 0)
    { MESSAGE("ERROR : read number of polyhedrons ...");
      return -1;
    }
  ISCRUTE(nodeindexsize);

  /* how many nodes for the polyhedron connectivity ? */
  if ((connectivitysize = MEDmeshnEntity(fid,meshname,MED_NO_DT,MED_NO_IT,
           MED_CELL,MED_POLYHEDRON,MED_CONNECTIVITY,MED_NODAL,
           &coordinatechangement, &geotransformation)) < 0)
    { MESSAGE("ERROR : read connectivity size ...");
      return -1;
    }
  ISCRUTE(connectivitysize);

  /* read mesh nodes coordinates */
  if ((coordinates = (med_float*) malloc(sizeof(med_float)*nnodes*spacedim)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    return -1;
  }

  if (MEDmeshNodeCoordinateRd(fid, meshname, MED_NO_DT, MED_NO_IT, MED_FULL_INTERLACE,
            coordinates) < 0) {
    MESSAGE("ERROR : nodes coordinates ...");
    return -1;
  }
  for (i=0;i<nnodes*spacedim;i++)
    printf("%f - ",*(coordinates+i));
  printf("\n");


  /* read polyhedron connectivity */
  faceindex = (med_int *) malloc(sizeof(med_int)*faceindexsize);
  nodeindex = (med_int *) malloc(sizeof(med_int)*nodeindexsize);
  connectivity = (med_int *) malloc(sizeof(med_int)*connectivitysize);

  if (MEDmeshPolyhedronRd(fid,meshname,MED_NO_DT,MED_NO_IT,MED_CELL,MED_NODAL,
			  faceindex,nodeindex,connectivity) < 0)
    { MESSAGE("ERROR : read polyhedron connectivity ...");
      return -1;
    }

  for (i=0;i<npoly;i++)
    {
    printf(">> MED_POLYHEDRON "IFORMAT" : \n",i+1);
    printf("---- Face Index         ----- : [\n");
    ind1 = *(faceindex+i)-1;
    ind2 = *(faceindex+i+1)-1;
    for (k=ind1;k<ind2;k++)
      printf(IFORMAT" ",*(nodeindex+k));
    printf(" ] \n");
    printf("---- Connectivity       ----- : [\n");
    for (k=ind1;k<ind2;k++)
      {
      jind1 = *(nodeindex+k)-1;
      jind2 = *(nodeindex+k+1)-1;
      for (j=jind1;j<jind2;j++)
        printf(IFORMAT" ",*(connectivity+j));
      printf(" \n");
      }
    printf(" ] \n");
    }

  /*
   * ... we know that the family number of nodes and elements is 0, a real code would check ...
   */

  /* close MED file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file");
    return -1;
  }

  /* memory deallocation */
  if (coordinates)
    free(coordinates);

  if (faceindex)
    free(faceindex);

  if (nodeindex)
    free(nodeindex);

  if (connectivity)
    free(connectivity);

  return 0;
}
