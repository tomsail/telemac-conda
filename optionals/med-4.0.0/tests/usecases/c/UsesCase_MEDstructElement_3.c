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

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

/* 
 * StructElement use case 3 : read struct element models in a file from a computation mesh 
 */

int main (int argc, char **argv) {
  med_idt fid;
  med_int nmodels, nsmesh;
  int i,j;
  char elementname[MED_NAME_SIZE+1]="";
  char supportmeshname[MED_NAME_SIZE+1]="";
  const char computmeshname[MED_NAME_SIZE+1]="COMPUT_MESH";
  med_geometry_type *geotype;
  med_entity_type entitype;
  med_int elementdim;
  med_int nnode,ncell;
  med_geometry_type geocelltype;
  med_bool anyprofile=0;
  med_int nconstatt, *nvaratt=NULL;
  char attname[MED_NAME_SIZE+1]="";
  char profilename[MED_NAME_SIZE+1]="";
  med_attribute_type atttype;
  med_int nattcomp;
  med_entity_type attentitype;
  med_int profilesize;
  med_float *value=NULL;
  med_int size=0;
  med_int meshdim, spacedim;
  char description[MED_COMMENT_SIZE+1]="";
  char axisname[3*MED_SNAME_SIZE+1]="";
  char axisunit[3*MED_SNAME_SIZE+1]="";
  med_axis_type axistype;
  med_float *coordinates=NULL;
  med_bool coordinatechangement;
  med_bool geotransformation;
  med_int nseg2;
  med_int *seg2connectivity=NULL;
  med_int nentities=0;
  med_sorting_type sortingtype;
  med_mesh_type meshtype;
  med_int nstep;
  char dtunit[MED_SNAME_SIZE+1]="";
  char unitname[2*MED_SNAME_SIZE+1]="";
  int ret=-1;

  /* open file */
  fid = MEDfileOpen("UsesCase_MEDstructElement_1.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }

  /* 
   * ... In this case, we know that the MED file has only one mesh, 
   * a real code would check ... 
   */
  /* read mesh informations : mesh dimension, space dimension ... */
  if (MEDmeshInfoByName(fid, computmeshname, &spacedim, &meshdim, &meshtype, description, 
			dtunit, &sortingtype, &nstep, &axistype, axisname, unitname) < 0) {
    MESSAGE("ERROR : mesh info ...");
    goto ERROR;
  }

  /* read how many struct element models in the mesh ? */
  if ((nmodels = MEDmeshnEntity(fid, computmeshname, MED_NO_DT, MED_NO_IT, MED_STRUCT_ELEMENT, MED_GEO_ALL,
			      MED_UNDEF_DATATYPE, MED_NO_CMODE,&coordinatechangement,
			      &geotransformation)) < 0) {
    MESSAGE("ERROR : number of nodes ...");
    goto ERROR;
  }

  geotype = (med_geometry_type *) malloc(sizeof(med_geometry_type)*nmodels);
  nvaratt = (med_int *) malloc(sizeof(med_int)*nmodels);

  /* Get the name and the geotype of each struct element model used in the computation mesh.
     For each struct element geotype read the connectivity and the attribute(s) 
     The mesh support may be read if needed.
  */
  for (i=0;i<nmodels;i++) {

    /* read the geotype */
    if (MEDmeshEntityInfo(fid, computmeshname, MED_NO_DT, MED_NO_IT, MED_STRUCT_ELEMENT,i+1,elementname,(geotype+i)) < 0) {
      MESSAGE("ERROR : name and type of MED_STRUCT_ELEMENT ...");
      goto ERROR;
    }

    /* read how many MED_STRUCT_ELEMENT of type *(geotype+i) there is in the mesh */
    if ((nentities = MEDmeshnEntity(fid, computmeshname, MED_NO_DT, MED_NO_IT, MED_STRUCT_ELEMENT,*(geotype+i),
				    MED_CONNECTIVITY, MED_NODAL, &coordinatechangement,
				    &geotransformation)) < 0) {
      MESSAGE("ERROR : number of MED_STRUCT_ELEMENT ...");
      goto ERROR;
    }

    /* read informations about the model */
    if (MEDstructElementInfoByName(fid, elementname, geotype+i, &elementdim,
				   supportmeshname, &entitype, &nnode, &ncell,
				   &geocelltype, &nconstatt, &anyprofile, nvaratt+i) < 0) {
      MESSAGE("ERROR : struct element models information ...");
      goto ERROR;
    }
    
    /* read support mesh  */
    /* TODO : Ceci est optionnel dans ce usescase */ 
    if (strcmp(supportmeshname,"")) {
      if ( MEDsupportMeshInfoByName(fid, supportmeshname, &spacedim, &meshdim, description,
				    &axistype, axisname, axisunit) < 0 ) {
	MESSAGE("ERROR : read information about mesh support ...");
	goto ERROR;
      }

      ISCRUTE(nnode);
      /* read how many nodes in the support mesh */
      /* Ceci est optionnel dans ce usescase */ 
      if ((nnode = MEDmeshnEntity(fid, supportmeshname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_NONE,
				  MED_COORDINATE, MED_NO_CMODE,&coordinatechangement,
				  &geotransformation)) < 0) {
	MESSAGE("ERROR : read number of nodes ...");
	goto ERROR;
      }
     ISCRUTE(nnode);

      /* read support mesh nodes coordinates */
      /* Ceci est optionnel dans ce usescase */ 
      coordinates = (med_float*) malloc(sizeof(med_float)*nnode*spacedim);

      if (MEDmeshNodeCoordinateRd(fid, supportmeshname, MED_NO_DT, MED_NO_IT, MED_FULL_INTERLACE,
				  coordinates) < 0) {
	MESSAGE("ERROR : read nodes coordinates ...");
	free(coordinates);
	goto ERROR;
      }

      /* free memory */
      free(coordinates);

      /* read how many MED_SEG2 cells in the support mesh */
      /* Ceci est optionnel dans ce usescase */ 
      if ((nseg2 = MEDmeshnEntity(fid, supportmeshname, MED_NO_DT, MED_NO_IT, MED_CELL,geocelltype,
				  MED_CONNECTIVITY, MED_NODAL, &coordinatechangement,
				  &geotransformation)) < 0) {
	MESSAGE("ERROR : number of MED_SEG2 ...");
	goto ERROR;
      }
     ISCRUTE(nseg2);
     ISCRUTE(ncell);
      /* read MED_SEG2 connectivity in the support mesh */
      if (nseg2 > 0) {
	seg2connectivity = (med_int *) malloc(sizeof(med_int)*nseg2*2);

	if (MEDmeshElementConnectivityRd(fid, supportmeshname, MED_NO_DT, MED_NO_IT, MED_CELL,
					 geocelltype, MED_NODAL, MED_FULL_INTERLACE, seg2connectivity) < 0) {
	  MESSAGE("ERROR : MED_SEG2 connectivity ...");
	  free(seg2connectivity);
	  goto ERROR;
	}
	free(seg2connectivity);
      }
    }

    /* read constant attribute(s) */
    /* Optionnel pour ce usescase (cf. usescase lecture modèle) */
    for (j=0; j<nconstatt; j++) {
      if ( MEDstructElementConstAttInfo(fid, elementname, j+1,
					attname, &atttype, &nattcomp, &attentitype,
					profilename, &profilesize) < 0) {
	MESSAGE("ERROR : const attribute information ...");
	goto ERROR;
      }

      /* if there is a profile => read the profile, see UsesCase_MEDprofile_2 */

      /* memory allocation */
      if (profilesize != 0)
	size = profilesize*nattcomp*MEDstructElementAttSizeof(atttype);
      else
	if (attentitype== MED_NODE)
	  size = nnode*nattcomp*MEDstructElementAttSizeof(atttype);
	else
	  size = ncell*nattcomp*MEDstructElementAttSizeof(atttype);
      if ( atttype == MED_ATT_NAME) ++size;
      ISCRUTE(size);
      value = (med_float *) malloc(size);


      /* read attribute(s) value(s) */
      if ( MEDstructElementConstAttRd(fid, elementname, attname, (unsigned char *)value ) < 0 ) {
	MESSAGE("ERROR : const attribute value ...");
	free(value);
	goto ERROR;
      }

      /* memory deallocation */
      free(value);
    }

    /* read variable attribute(s) */
    for (j=0; j<*(nvaratt+i); j++) {

      /* read informations about the attribute */
      if ( MEDstructElementVarAttInfo(fid, elementname, j+1, 
				      attname, &atttype, &nattcomp) < 0) {
	    MESSAGE("ERROR : var attribute information ...");
	    goto ERROR;
      }
      
      /* memory allocation */
      size = nentities*nattcomp*MEDstructElementAttSizeof(atttype);
      if ( atttype == MED_ATT_NAME) ++size;
      ISCRUTE(size);
      value = (med_float *) malloc((size)*sizeof(char));
      
      /* read attribute values */
      if (MEDmeshStructElementVarAttRd(fid, computmeshname, MED_NO_DT, MED_NO_IT,
				       *(geotype+i), attname, value ) < 0) {
	MESSAGE("ERROR : read variable attributes values ...");
	free(value);
	goto ERROR;
      }

      /* free memory */
      free(value);
      
    }
    
  }

  ret=0;
 ERROR:

  /* free memory */
  free(geotype);
  free(nvaratt);

  /* close file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : file closing ...");
    ret=-1;
  }

  return ret;
}

