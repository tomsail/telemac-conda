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
 * Field use case 13 : write a field on nodes elements in a MED file
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>


int main (int argc, char **argv) {
  med_idt fid;
  const char meshname[MED_NAME_SIZE+1] = "2D unstructured mesh";
  const char fieldname[MED_NAME_SIZE+1] = "TEMPERATURE_FIELD";
  const med_int ncomponent = 1;
  const char componentname[MED_SNAME_SIZE+1] = "TEMPERATURE";
  const char componentunit[MED_SNAME_SIZE+1] = "C";
  const med_int nquad4 = 4;
  const med_float quad4values_step1[4*4] = { 10000.,  20000.,  30000.,  40000.,
					     50000.,  60000.,  70000.,  80000.,
					     90000., 100000., 110000., 120000.,
					     130000., 140000., 150000., 160000. };
  const med_float quad4values_step2[4*4] = { 100.,  200.,  300.,  400.,
					     500.,  600.,  700.,  800.,
					     900., 1000., 1100., 1200.,
					     1300., 1400., 1500., 1600. };
  int ret=-1;

  /* file creation */
  fid = MEDfileOpen("UsesCase_MEDfield_13.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }

  /* create mesh link */
  if (MEDlinkWr(fid,meshname,"./UsesCase_MEDmesh_1.med") < 0) {
    MESSAGE("ERROR : create mesh link ...");
    goto ERROR;
  }

  /*
   * Temperature field  creation :
   * - 1 component
   * - component unit : celsius degree
   * - mesh is the 2D unstructured mesh of UsecaseMEDmesh_1.c use case.
   * - computation step unit in 'ms'
   */
  if (MEDfieldCr(fid, fieldname, MED_FLOAT64,
		 ncomponent, componentname, componentunit,
		 "ms", meshname) < 0) {
    MESSAGE("ERROR : create field");
    goto ERROR;
  }

  /* two computation steps */
  /* write values at nodes elements : 4 MED_QUAD4 */

  /* STEP 1 : dt1 = 5.5, it = 1*/
  /* MED_QUAD4  : with no profile */
  if (MEDfieldValueWithProfileWr(fid, fieldname, 1, 1, 5.5, MED_NODE_ELEMENT, MED_QUAD4,
				 MED_COMPACT_STMODE, MED_NO_PROFILE, MED_NO_LOCALIZATION,
				 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
				 nquad4, (unsigned char*) quad4values_step1) < 0) {
    MESSAGE("ERROR : write field values on MED_QUAD4 ");
    goto ERROR;
  }

  /* STEP 2 : dt2 = 8.9, it = 1*/
  /* MED_QUAD4 : with no profile */
  if (MEDfieldValueWithProfileWr(fid, fieldname, 2, 1, 8.9, MED_NODE_ELEMENT, MED_QUAD4,
				 MED_COMPACT_STMODE, MED_NO_PROFILE, MED_NO_LOCALIZATION,
				 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
				 nquad4, (unsigned char*) quad4values_step2) < 0) {
    MESSAGE("ERROR : write field values on MED_QUAD4 ... ");
    goto ERROR;
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

