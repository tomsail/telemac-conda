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
 * Field use case 4 : write a field with computing steps
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
  const med_int ntria3 = 8;
  const med_int nquad4 = 4;
  const med_float tria3values_step1[8] = {1000., 2000., 3000., 4000.,
					  5000., 6000., 7000., 8000.};
  const med_float quad4values_step1[4] = {10000., 20000., 30000., 4000.};
  const med_float tria3values_step2[8] = {1500., 2500., 3500., 4500.,
					  5500., 6500., 7500., 8500.};
  const med_float quad4values_step2[4] = {15000., 25000., 35000., 45000.};
  int ret=-1;

  /* file creation */
  fid = MEDfileOpen("UsesCase_MEDfield_4.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }

  /* create mesh link */
  if (MEDlinkWr(fid,meshname,"./UsesCase_MEDmesh_1.med") < 0) {
    MESSAGE("ERROR : create mesh link ...");
    goto ERROR;
  }

  /* field creation : temperature field  : 1 component in celsius degree
   *                  the mesh is the 2D unstructured mesh of UsescaseMEDmesh_1.c
   *                  use case. Computation step unit in 'ms'
   */
  if (MEDfieldCr(fid, fieldname, MED_FLOAT64,
		 ncomponent, componentname, componentunit,"ms", meshname) < 0) {
    MESSAGE("ERROR : create field");
    goto ERROR;
  }

  /* two computation steps :
   *  - first  on meshname MED_NO_DT,MED_NO_IT mesh computation step
   *  - second on meshname 1,3 mesh computation step */
  /* write values at cell centers : 8 MED_TRIA3 and 4 MED_QUAD4 */

  /* STEP 1 : dt1 = 5.5, it = 1*/
  /* MED_TRIA3 */
  if (MEDfieldValueWr(fid, fieldname, 1, 1, 5.5, MED_CELL, MED_TRIA3, MED_FULL_INTERLACE,
		      MED_ALL_CONSTITUENT, ntria3, (unsigned char*) tria3values_step1) < 0) {
    MESSAGE("ERROR : write field values on MED_TRIA3");
    goto ERROR;
  }
   /* MED_QUAD4 */
  if (MEDfieldValueWr(fid, fieldname, 1, 1, 5.5, MED_CELL,MED_QUAD4, MED_FULL_INTERLACE,
		      MED_ALL_CONSTITUENT, nquad4, (unsigned char*) quad4values_step1) < 0) {
    MESSAGE("ERROR : write field values on MED_QUAD4 ");
    goto ERROR;
  }

  /* STEP 2 : dt2 = 8.9, it = 1*/
  /* MED_TRIA3 */
  if (MEDfieldValueWr(fid, fieldname, 2 , 1 , 8.9 , MED_CELL, MED_TRIA3, MED_FULL_INTERLACE,
		      MED_ALL_CONSTITUENT, ntria3,  (unsigned char*)tria3values_step2) < 0) {
    MESSAGE("ERROR : write field values on MED_TRIA3");
    goto ERROR;
  }

  /* MED_QUAD4 */
  if (MEDfieldValueWr(fid, fieldname, 2, 1, 8.9, MED_CELL, MED_QUAD4, MED_FULL_INTERLACE,
		      MED_ALL_CONSTITUENT, nquad4, (unsigned char*)quad4values_step2) < 0) {
    MESSAGE("ERROR : write field values on MED_QUAD4 ");
    goto ERROR;
  }

  /* Write associated mesh computation step */
  if ( MEDfieldComputingStepMeshWr(fid, fieldname, 2, 1,
				   1, 3 ) < 0 ) {
    MESSAGE("ERROR : write field mesh computation step error ");
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

