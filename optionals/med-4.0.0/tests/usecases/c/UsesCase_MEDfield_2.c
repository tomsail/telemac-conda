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
 * Field use case 2 : read the field of use case 1
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
  med_float *verticesvalues = NULL;
  med_float *tria3values = NULL;
  med_float *quad4values = NULL;
  med_int nstep, nvalues;
  const med_int ncomponent = 1;
  int ret=-1;

  /* open MED file with READ ONLY access mode */
  fid = MEDfileOpen("UsesCase_MEDfield_1.med",MED_ACC_RDONLY);
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
   * ... we know that the field values are defined on vertices and MED_TRIA3
   * and MED_QUAD4 cells, a real code would check ...
   */

  /* MED_NODE */
  if ((nvalues = MEDfieldnValue(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_NONE)) < 0) {
    MESSAGE("ERROR : read number of values ...");
    goto ERROR;
  }

  if ((verticesvalues = (med_float *) malloc(sizeof(med_float)*nvalues*ncomponent)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }
  if (MEDfieldValueRd(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_NODE, MED_NONE,
		      MED_FULL_INTERLACE, MED_ALL_CONSTITUENT, (unsigned char*) verticesvalues) < 0) {
    MESSAGE("ERROR : read fields values on vertices ...");
    free(verticesvalues);
    goto ERROR;
  }
  free(verticesvalues);

  /* MED_TRIA3 */
  if ((nvalues = MEDfieldnValue(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_CELL,
				MED_TRIA3)) < 0) {
    MESSAGE("ERROR : read number of values ...");
    goto ERROR;
  }
  if ((tria3values = (med_float *) malloc(sizeof(med_float)*nvalues*ncomponent)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }
  if (MEDfieldValueRd(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_CELL, MED_TRIA3,
		      MED_FULL_INTERLACE, MED_ALL_CONSTITUENT, (unsigned char*) tria3values) < 0) {
    MESSAGE("ERROR : read fields values for MED_TRIA3 cells ...");
    free(tria3values);
    goto ERROR;
  }
  free(tria3values);

  /* MED_QUAD4 */
  if ((nvalues = MEDfieldnValue(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_CELL,
				MED_QUAD4)) < 0) {
    MESSAGE("ERROR : read number of values ...");
    goto ERROR;
  }
  if ((quad4values = (med_float *) malloc(sizeof(med_float)*nvalues*ncomponent)) == NULL) {
    MESSAGE("ERROR : memory allocation ...");
    goto ERROR;
  }
  if (MEDfieldValueRd(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_CELL, MED_QUAD4,
		      MED_FULL_INTERLACE, MED_ALL_CONSTITUENT, (unsigned char*) quad4values) < 0) {
    MESSAGE("ERROR : read fields values for MED_QUAD4 cells ...");
    free(quad4values);
    goto ERROR;
  }
  free(quad4values);

  ret=0;
 ERROR:

  /* close file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file ...");
    ret=-1;
  }

  return ret;
}
