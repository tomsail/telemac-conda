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
 * Field use case 3 : read a field (generic approach)
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
  med_geometry_type geotype;
  med_geometry_type *geotypes = MED_GET_CELL_GEOMETRY_TYPE;
  int ret=-1;

  /* file creation */
  fid = MEDfileOpen("UsesCase_MEDfield_1.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : open file ...");
    goto ERROR;
  }

  /*
   * generic approach : how many fields in the file and identification
   * of each field.
   */
  if ((nfield = MEDnField(fid)) < 0) {
    MESSAGE("ERROR : How many fields in the file ...");
    goto ERROR;
  }

  for (i=0; i<nfield; i++) {

    /* field information
     * ... we know that the field has no computation step
     * and that the field values type is MED_FLOAT64, a real code would check ...
     */
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
      free(componentname); free(componentunit);
      goto ERROR;
    }
    free(componentname); free(componentunit);

    /* read field values for nodes and cells */

    /* MED_NODE */
    if ((nvalues = MEDfieldnValue(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_NONE)) < 0) {
      MESSAGE("ERROR : read number of values ...");
      goto ERROR;
    }

    if (nvalues) {
      if ((values = (med_float *) malloc(sizeof(med_float)*nvalues*ncomponent)) == NULL) {
	MESSAGE("ERROR : memory allocation ...");
	goto ERROR;
      }
      if (MEDfieldValueRd(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_NONE,
			  MED_FULL_INTERLACE, MED_ALL_CONSTITUENT, (unsigned char*) values) < 0) {
	MESSAGE("ERROR : read fields values defined on vertices ...");
	free(values);
	goto ERROR;
      }
      free(values);
    }

    /* MED_CELL */
    for (j=1; j<=MED_N_CELL_FIXED_GEO; j++) {
      geotype = geotypes[j];

      if ((nvalues = MEDfieldnValue(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_CELL,
				    geotype)) < 0) {
	MESSAGE("ERROR : read number of values ...");
	goto ERROR;
      }

      if (nvalues) {
	if ((values = (med_float *) malloc(sizeof(med_float)*nvalues*ncomponent)) == NULL) {
	  MESSAGE("ERROR : memory allocation ...");
	  goto ERROR;
	}
	if (MEDfieldValueRd(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_CELL, geotype,
			    MED_FULL_INTERLACE, MED_ALL_CONSTITUENT, (unsigned char*) values) < 0) {
	  MESSAGE("ERROR : read fields values for cells ...");
	  free(values);
	  goto ERROR;
	}
	free(values);
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
