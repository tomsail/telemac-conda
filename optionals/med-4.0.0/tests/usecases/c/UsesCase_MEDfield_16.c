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

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

/* 
 * Field use case 16 : read a field (generic approach) in a MED file with 
 * - values defined on struct elements
 */


int main (int argc, char **argv) {
  med_idt fid,mfid,sfid;
  med_idt nfield, i, j;
  char meshname[MED_NAME_SIZE+1]="";
  med_bool localmesh;
  char fieldname[MED_NAME_SIZE+1]="";
  med_field_type fieldtype;
  char *componentname = NULL;
  char *componentunit = NULL;
  char dtunit[MED_SNAME_SIZE+1]="";
  med_float *values = NULL;
  med_int nstep, nvalues;
  med_int ncomponent;
  med_int csit, numit, numdt, it;
  med_float dt;
  med_geometry_type *geotypes, geotype;
  med_int nmodels;
  med_int nprofile, pit, profilesize;
  char profilename[MED_NAME_SIZE+1]="";
  med_int nintegrationpoint;
  char localizationname[MED_NAME_SIZE+1]="";
  int k;
  char elementname[MED_NAME_SIZE+1];
  char supportmeshname[MED_NAME_SIZE+1];
  med_entity_type entitype;
  med_int elementdim;
  med_int nnode,ncell;
  med_geometry_type geocelltype;
  med_bool anyprofile=0;
  med_int nconstatt, *nvaratt;
  int ret=-1;

  /* open file */
  fid = MEDfileOpen("UsesCase_MEDfield_15.med",MED_ACC_RDWR);
  if (fid < 0) {
    MESSAGE("ERROR : open file ...");
    goto ERROR;
  }

  if (( mfid=MEDfileObjectsMount(fid,  "UsesCase_MEDstructElement_1.med",MED_MESH_SUPPORT)) < 0 ) {
    MESSAGE("ERROR : file mounting ...");
    goto ERROR;
  }

  if (( sfid=MEDfileObjectsMount(fid,  "UsesCase_MEDstructElement_1.med",MED_ELSTRUCT)) < 0 ) {
    MESSAGE("ERROR : file mounting ...");
    goto ERROR;
  }

  /* How many struct element models ? */
  if ((nmodels =  MEDnStructElement(mfid)) < 0) {
    MESSAGE("ERROR : read number of struct element models ...");
    goto ERROR;
  }
  geotypes = (med_geometry_type *) malloc(sizeof(med_geometry_type)*nmodels);
  nvaratt = (med_int *) malloc(sizeof(med_int)*nmodels);

  for (it=0; it<nmodels; it++) {
    if (MEDstructElementInfo(mfid, it+1, elementname, geotypes+it, &elementdim,
			     supportmeshname, &entitype, &nnode, &ncell,
			     &geocelltype, &nconstatt, &anyprofile, nvaratt+it) < 0) {
      MESSAGE("ERROR : struct element models information ...");
      free(nvaratt);
      goto ERROR;
    }
  }
  free(nvaratt);


  /*
   * generic approach : how many fields in the file and identification
   * of each field.
   */
  if ((nfield = MEDnField(fid)) < 0) {
    MESSAGE("ERROR : How many fields in the file ...");
    goto ERROR;
  }

  /*
   * read values for each field
   */
  for (i=0; i<nfield; i++) {

    if ((ncomponent = MEDfieldnComponent(fid,i+1)) < 0) {
      MESSAGE("ERROR : number of field component ...");
      goto ERROR;
    }
    
    if ((componentname = (char *) malloc(ncomponent*MED_SNAME_SIZE+1)) == NULL) {
      MESSAGE("ERROR : memory allocation ...");
      goto ERROR;
    }

    if ((componentunit = (char *) malloc(ncomponent*MED_SNAME_SIZE+1)) == NULL) {
      MESSAGE("ERROR : memory allocation ...");
      goto ERROR;
    }

    if (MEDfieldInfo(fid, i+1, fieldname, meshname, &localmesh, &fieldtype,
		     componentname, componentunit, dtunit, &nstep) < 0) {
      MESSAGE("ERROR : Field info ...");
      free(componentname);
      free(componentunit);
      goto ERROR;
    }

    free(componentname);
    free(componentunit);

    /*
     * Read field values for each computing step 
     */ 
    for (csit=0; csit<nstep; csit++) {

      if (MEDfieldComputingStepInfo(fid, fieldname, csit+1, &numdt, &numit, &dt) < 0) {
	MESSAGE("ERROR : Computing step info ...");
	goto ERROR;
      }

      /* 
       * ... In our case, we suppose that the field values are only defined on struct element ... 
       */
      for (it=0; it<nmodels; it++) {

	geotype = *(geotypes+it);
     
	/*
	 * How many profile for each geometry type ? 
	 */
	if ((nprofile = MEDfieldnProfile(fid, fieldname, numdt, numit, MED_STRUCT_ELEMENT, geotype,
					 profilename, localizationname)) < 0) {
	  MESSAGE("ERROR : read number of profile ");
	  goto ERROR;
	}
	
	/* 
	 * Read values for each profile
	 */
	for (pit=0; pit<nprofile; pit++) {
	  
	  if ((nvalues = MEDfieldnValueWithProfile(fid, fieldname, numdt, numit, MED_STRUCT_ELEMENT, geotype,
						   pit+1, MED_COMPACT_STMODE, profilename, &profilesize,
						   localizationname, &nintegrationpoint)) < 0) {
	    MESSAGE("ERROR : read number of values with a profile ...");
	    goto ERROR;
	  } 
	  
	  if (nvalues) {
	    if ((values = (med_float *) malloc(sizeof(med_float)*nvalues*ncomponent*nintegrationpoint)) == NULL) {
	      MESSAGE("ERROR : memory allocation ...");
	      goto ERROR;
	    }
	    if (MEDfieldValueWithProfileRd(fid, fieldname, numdt, numit, MED_STRUCT_ELEMENT, geotype,
					   MED_COMPACT_STMODE, profilename,
					   MED_FULL_INTERLACE, MED_ALL_CONSTITUENT, 
					   (unsigned char*) values) < 0) {
	      MESSAGE("ERROR : read fields values for cells ..."); 
	      goto ERROR; 
	      free(values);
	    }  
	    free(values);
	  }
	}
      }
    }
  }
  
  ret=0;
 ERROR :

  free(geotypes);

  if ( MEDfileObjectsUnmount(fid, sfid, MED_ELSTRUCT) < 0 ) {
    MESSAGE("ERROR : file unmounting ...");
    ret=-1;
  }

  if ( MEDfileObjectsUnmount(fid, mfid, MED_MESH_SUPPORT) < 0 ) {
    MESSAGE("ERROR : file unmounting ...");
    ret= -1;
  }

  /* close file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file ...");             
    ret= -1; 
  } 
  
  return ret;
}
