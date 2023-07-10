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
 * Field use case 10 : write a field in a MED file with computing steps,
 *                     profiles, integration points and interpolation families
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
  const med_float tria3values_step1_profile1[9] = {1000.,1010.,1020.,
						   4000.,4010.,4020.,
						   8000.,8010.,8020. };
  const med_float tria3values_step2_profile1[24] = {1500.,1510.,1520.,
						    0.,   0.,   0.,
						    0.,   0.,   0.,
						    4500.,4510,4520.,
						    0.,   0.,   0.,
						    0.,   0.,   0.,
						    0.,   0.,   0.,
						    8500., 8510, 8520 };
  const med_float tria3values_step2_profile2[32] = {   0.,   0.,   0.,  0.,
						       2500.,2510.,2520,2530.,
						       3500.,3510.,3520.,3530.,
						       0.,   0.,   0.,  0.,
						       5500.,5510.,5520.,5530.,
						       6500.,6510.,6520.,6530.,
						       7500.,7510.,7520.,7530.,
						       0.,   0.,   0.,   0. };
  const med_int ntria3 = 8;
  const med_int nquad4 = 4;
  const med_float quad4values_step1[4] = {10000., 20000., 30000., 40000.};
  const med_float quad4values_step2[4] = {15000., 25000., 35000., 45000.};
  const char profile1name[MED_NAME_SIZE+1] = "MED_TRIA3_PROFILE1";
  const med_int profile1[3] = {1, 4, 8};
  const med_int profile1size = 3;
  const char profile2name[MED_NAME_SIZE+1] = "MED_TRIA3_PROFILE2";
  const med_int profile2[5] = {2, 3, 5, 6, 7};
  const med_int profile2size = 5;
  const char localization1name[MED_NAME_SIZE+1] = "TRIA3_INTEGRATION_POINTS_3";
  const med_float weight1[3] = {1.0/6, 1.0/6, 1.0/6};
  const med_float elementcoordinate[6] = {0.0, 0.0,  1.0, 0.0,  0.0,1.0};
  const med_float ipoint1coordinate[6] = {1.0/6, 1.0/6,  2.0/3, 1.0/6,  1.0/6, 2.0/6};
  const char localization2name[MED_NAME_SIZE+1] = "TRIA3_INTEGRATION_POINTS_4";
  const med_float weight2[6] = {25.0/(24*4), 25.0/(24*4), 25.0/(24*4), -27.0/(24*4)};
  const med_float ipoint2coordinate[8] = {1.0/5, 1.0/5,  3.0/5, 1.0/5,  1.0/5, 3.0/5,  1.0/3, 1.0/3};
  med_int nipoint, spacedim;
  const char interpname[MED_NAME_SIZE+1] = "MED_TRIA3 interpolation family";
  int ret=-1;

  /* file creation */
  fid = MEDfileOpen("UsesCase_MEDfield_10.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }

  /* create mesh link */
  if (MEDlinkWr(fid,meshname,"./UsesCase_MEDmesh_1.med") < 0) {
    MESSAGE("ERROR : create mesh link ...");
    goto ERROR;
  }

  /* create the profiles in the file */
  if (MEDprofileWr(fid, profile1name, profile1size, profile1 ) < 0) {
    MESSAGE("ERROR : create profile ...");
    goto ERROR;
  }

  if (MEDprofileWr(fid, profile2name, profile2size, profile2 ) < 0) {
    MESSAGE("ERROR : create profile ...");
    goto ERROR;
  }

  /* create the localization elements for integration points */
  spacedim = 2;
  nipoint = 3;
  if (MEDlocalizationWr(fid, localization1name, MED_TRIA3, spacedim,
			elementcoordinate, MED_FULL_INTERLACE,
			nipoint, ipoint1coordinate, weight1,
			MED_NO_INTERPOLATION, MED_NO_MESH_SUPPORT) < 0) {
    MESSAGE("ERROR : create famlily of integration points ...");
    goto ERROR;
  }

  spacedim = 2;
  nipoint = 4;
  if (MEDlocalizationWr(fid, localization2name, MED_TRIA3, spacedim,
			elementcoordinate, MED_FULL_INTERLACE,
			nipoint, ipoint2coordinate, weight2,
			MED_NO_INTERPOLATION, MED_NO_MESH_SUPPORT) < 0) {
    MESSAGE("ERROR : create famlily of integration points ...");
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

  /* write interpolation family name for MED_TRIA3 cell type */
  /* The interpolation family "interpname" is created in the UsesCase_MEDinterp_1
     use case */
  if (MEDfieldInterpWr(fid,fieldname,interpname) <0) {
    MESSAGE("ERROR : write field interpolation family name ...");
    goto ERROR;
  }

  /* two computation steps */
  /* write values at cell centers : 8 MED_TRIA3 and 4 MED_QUAD4 */

  /* STEP 1 : dt1 = 5.5, it = 1*/
  /* MED_TRIA3 : with a profile of 3 values in compact memory storage mode
     and a family of 3 integration points */
  if (MEDfieldValueWithProfileWr(fid, fieldname, 1, 1, 5.5, MED_CELL,MED_TRIA3,
				 MED_COMPACT_STMODE, profile1name, localization1name,
				 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
				 ntria3, (unsigned char*) tria3values_step1_profile1) < 0) {
    MESSAGE("ERROR : write field values on MED_TRIA3");
    goto ERROR;
  }
   /* MED_QUAD4  : with no profile */
  if (MEDfieldValueWithProfileWr(fid, fieldname, 1, 1, 5.5, MED_CELL, MED_QUAD4,
				 MED_COMPACT_STMODE, MED_NO_PROFILE, MED_NO_LOCALIZATION,
				 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
				 nquad4, (unsigned char*) quad4values_step1) < 0) {
    MESSAGE("ERROR : write field values on MED_QUAD4 ");
    goto ERROR;
  }

  /* STEP 2 : dt2 = 8.9, it = 1*/
  /* MED_TRIA3 : with a profile of 3 values then a profile of 5 values in global memory storage mode
   * For each profile, a family of 3 and then 4 integration points */
  if (MEDfieldValueWithProfileWr(fid, fieldname, 2 , 1 , 8.9 , MED_CELL, MED_TRIA3,
				 MED_GLOBAL_STMODE, profile1name, localization1name,
				 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
				 ntria3, (unsigned char*) tria3values_step2_profile1) < 0) {
    MESSAGE("ERROR : write field values on MED_TRIA3 ...");
    goto ERROR;
  }
  if (MEDfieldValueWithProfileWr(fid, fieldname, 2 , 1 , 8.9 , MED_CELL, MED_TRIA3,
				 MED_GLOBAL_STMODE, profile2name, localization2name,
				 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
				 ntria3, (unsigned char*) tria3values_step2_profile2) < 0) {
    MESSAGE("ERROR : write field values on MED_TRIA3 ...");
    goto ERROR;
  }

  /* MED_QUAD4 : with no profile */
  if (MEDfieldValueWithProfileWr(fid, fieldname, 2, 1, 8.9, MED_CELL, MED_QUAD4,
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

