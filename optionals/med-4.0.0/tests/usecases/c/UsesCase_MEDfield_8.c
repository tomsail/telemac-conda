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
 * Field use case 8 : read a field with following features computing steps and profiles
 */


#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  char meshname[MED_NAME_SIZE+1]="";
  med_bool localmesh;
  const char fieldname[MED_NAME_SIZE+1] = "TEMPERATURE_FIELD";
  med_field_type fieldtype;
  char componentname[MED_SNAME_SIZE+1]="";
  char componentunit[MED_SNAME_SIZE+1]="";
  char dtunit[MED_SNAME_SIZE+1]="";
  med_float *values = NULL;
  med_int nstep, nvalues;
  med_int ncomponent = 1;
  med_int csit, numit, numdt, it;
  med_float dt;
  med_geometry_type geotype;
  med_geometry_type *geotypes = MED_GET_CELL_GEOMETRY_TYPE;
  med_int pit, profilesize;
  med_int np;
  char profilename[MED_NAME_SIZE+1] = "";
  med_int nintegrationpoint;
  char localizationname[MED_NAME_SIZE+1] = "";
  int ret = -1;


  /* open file */
  fid = MEDfileOpen("UsesCase_MEDfield_7.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : open file ...");
    goto ERROR;
  }

  /*
   * ... we know that the MED file has only one field with one component ,
   * a real code would check ...
   */

  /*
   * if you know the field name, direct access to field informations
   */
  if (MEDfieldInfoByName(fid, fieldname, meshname, &localmesh, &fieldtype,
			 componentname, componentunit, dtunit, &nstep) < 0) {
    MESSAGE("ERROR : Field info by name ...");
    goto ERROR;
  }

  /*
   * Read field values for each computing step
   */
  for (csit=0; csit<nstep; csit++) {

    if (MEDfieldComputingStepInfo(fid, fieldname, csit+1, &numdt, &numit, &dt) < 0) {
      MESSAGE("ERROR : Computing step info ...");
      goto ERROR;
    }

  /*
   * ... In our case, we suppose that the field values are only defined on cells ...
   */
    for (it=1; it<=MED_N_CELL_FIXED_GEO; it++) {

      geotype = geotypes[it];
      /*
       * How many profile for each geometry type ?
       */
      if ((np = MEDfieldnProfile(fid, fieldname, numdt, numit, MED_CELL, geotype,
				 profilename, localizationname)) < 0) {
	MESSAGE("ERROR : read number of profile ");
	goto ERROR;
      }

      /*
       * Read values for each profile
       */
      for (pit=0; pit<np; pit++) {

	if ((nvalues = MEDfieldnValueWithProfile(fid, fieldname, numdt, numit, MED_CELL, geotype,
						 pit+1, MED_COMPACT_STMODE, profilename, &profilesize,
						 localizationname, &nintegrationpoint)) < 0) {
	  MESSAGE("ERROR : read number of values with a profile ...");
	  goto ERROR;
	}

	if (nvalues) {
	  if ((values = (med_float *) malloc(sizeof(med_float)*nvalues*ncomponent)) == NULL) {
	    MESSAGE("ERROR : memory allocation ...");
	    goto ERROR;
	  }
	  if (MEDfieldValueWithProfileRd(fid, fieldname, numdt, numit, MED_CELL, geotype,
					 MED_COMPACT_STMODE, profilename,
					 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
					 (unsigned char*) values) < 0) {
	    MESSAGE("ERROR : read fields values for cells ...");
	    free(values);
	    goto ERROR;
	  }

	  free(values);
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
