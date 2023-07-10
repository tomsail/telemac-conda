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
 * Field use case 12 : read a field (generic approach) in a MED file with computing steps,
 *                     profiles, integration points and interpolation families
 */


int main (int argc, char **argv) {
  med_idt fid;
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
  med_geometry_type geotype;
  med_geometry_type *geotypes = MED_GET_CELL_GEOMETRY_TYPE;
  med_int nprofile, pit, profilesize;
  char profilename[MED_NAME_SIZE+1]="";
  med_int nintegrationpoint;
  char localizationname[MED_NAME_SIZE+1]="";
  int k;
  med_int ninterp;
  char interpname[MED_NAME_SIZE+1];
  int ret=-1;


  /* open file */
  fid = MEDfileOpen("UsesCase_MEDfield_10.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : open file ...");
    return -1;
  }

  /*
   * generic approach : how many fields in the file and identification
   * of each field.
   */
  if ((nfield = MEDnField(fid)) < 0) {
    MESSAGE("ERROR : How many fields in the file ...");
    return -1;
  }

  /*
   * read values for each field
   */
  for (i=0; i<nfield; i++) {

    if ((ncomponent = MEDfieldnComponent(fid,i+1)) < 0) {
      MESSAGE("ERROR : number of field component ...");
      return -1;
    }

    if ((componentname = (char *) malloc(ncomponent*MED_SNAME_SIZE+1)) == NULL) {
      MESSAGE("ERROR : memory allocation ...");
      return -1;
    }

    if ((componentunit = (char *) malloc(ncomponent*MED_SNAME_SIZE+1)) == NULL) {
      MESSAGE("ERROR : memory allocation ...");
      return -1;
    }

    if (MEDfieldInfo(fid, i+1, fieldname, meshname, &localmesh, &fieldtype,
		     componentname, componentunit, dtunit, &nstep) < 0) {
      MESSAGE("ERROR : Field info ...");
      free(componentname);
      free(componentunit);
      return -1;
    }

    free(componentname);
    free(componentunit);

    /*
     * Read how many interpolation family name in the field ?
     */
    if ((ninterp = MEDfieldnInterp(fid, fieldname)) < 0) {
      MESSAGE("ERROR : Read how many interpolation functions for the field ...");
      return -1;
    }
    /* - Read each interlolation family name
     * - The way to read an interploation family and it's basis functions
     *  is described in UsesCase_MEDinterp_2 and UsesCase_MEDinterp_3 uses case
     */
    for (it=0; it<ninterp; it++) {
      if (MEDfieldInterpInfo(fid,fieldname,it+1,interpname) < 0) {
	MESSAGE("ERROR : read interpolation family name ...");
	return -1;
      }
    }

    /*
     * Read field values for each computing step
     */
    for (csit=0; csit<nstep; csit++) {

      if (MEDfieldComputingStepInfo(fid, fieldname, csit+1, &numdt, &numit, &dt) < 0) {
	MESSAGE("ERROR : Computing step info ...");
	return -1;
      }

      /*
       * ... In our case, we suppose that the field values are only defined on cells ...
       */
      for (it=1; it<=MED_N_CELL_FIXED_GEO; it++) {

	geotype = geotypes[it];

	/*
	 * How many profile for each geometry type ?
	 */
	if ((nprofile = MEDfieldnProfile(fid, fieldname, numdt, numit, MED_CELL, geotype,
					 profilename, localizationname)) < 0) {
	  MESSAGE("ERROR : read number of profile ");
	  return -1;
	}

	/*
	 * Read values for each profile
	 */
	for (pit=0; pit<nprofile; pit++) {

	  if ((nvalues = MEDfieldnValueWithProfile(fid, fieldname, numdt, numit, MED_CELL, geotype,
						   pit+1, MED_COMPACT_STMODE, profilename, &profilesize,
						   localizationname, &nintegrationpoint)) < 0) {
	    MESSAGE("ERROR : read number of values with a profile ...");
	    return -1;
	  }

	  if (nvalues) {
	    if ((values = (med_float *) malloc(sizeof(med_float)*nvalues*ncomponent*nintegrationpoint)) == NULL) {
	      MESSAGE("ERROR : memory allocation ...");
	      return -1;
	    }
	    if (MEDfieldValueWithProfileRd(fid, fieldname, numdt, numit, MED_CELL, geotype,
					   MED_COMPACT_STMODE, profilename,
					   MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
					   (unsigned char*) values) < 0) {
	      MESSAGE("ERROR : read fields values for cells ...");
	      free(values);
	      return -1;
	    }
	    free(values);
	  }
	}
      }
    }
  }

  ret=0;
 ERROR:

  /* close file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file ...");
    ret=-1;
  }

  return ret;
}
