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
 * Field use case 17 : write a field in a MED file with 
 *                     values defined on integration points
 *                     on struct elements 
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>


int main (int argc, char **argv) {
  med_idt fid=0,mfid=0,sfid=0;
  const med_int spacedim = 3;
  const char meshname[MED_NAME_SIZE+1] = "COMPUT_MESH";
  const char fieldname[MED_NAME_SIZE+1] = "TEMPERATURE";
  const med_int ncomponent = 1;
  /*                                      1234567890123456123456*/   
  const char componentname[MED_SNAME_SIZE+1] = "TEMP            ";
  const char componentunit[MED_SNAME_SIZE+1] = "CELSIUS         ";
  med_geometry_type geotype=MED_NONE;
  const med_int nbeam = 1;
  char structelementname[MED_NAME_SIZE+1];
  const med_float tempvalue[3*1*4] = { 1.1, 2.2, 3.3, 4.4, 
                                       5.5, 6.6, 7.7, 8.8, 
				       9.9, 10.1,11.11, 12.12};
  const char localization[MED_NAME_SIZE+1] = "BEAM_INTEGRATION_POINTS";
  const char localization2[MED_NAME_SIZE+1] = "BEAM_INTEGRATION_TRANSF";
  const med_float elementcoordinate[3*3] = { 0.0,0.0,0.0,
					     0.0,0.0,0.0,
					     0.0,0.0,0.0,};
  const med_float ipointcoordinate[3*3] = { 0.0,0.0,2.5,
					    0.0,0.0,3.5,
					    0.0,0.0,4.5};
  const med_float weight[4] = {1.0/4, 1.0/4, 1.0/4, 1.0/4};
  const char beamsectionname[MED_NAME_SIZE+1]="BEAM_SECTION_MESH";
  const med_int nipoint = 3;
  char interpname[MED_NAME_SIZE+1] = "geometrical transformation";
  const med_int nvariable=2;
  const med_int maxdegree=1;
  const med_int nmaxcoefficient=3;
  const med_int         ncoefficient1_1 = 3;
  const med_int   const power1_1[]         = {0,0,1,0,0,1};
  const med_float const coefficient1_1[]   = {1,-1,-1};
  const med_int         ncoefficient1_2 = 1;
  const med_int   const power1_2[]         = {1,0};
  const med_float const coefficient1_2[]   = {1};
  const med_int         ncoefficient1_3 = 1;
  const med_int   const power1_3[]         = {0,1};
  const med_float const coefficient1_3[]   = {1};
  int ret=-1;


  /* Open file to write the field */
  fid = MEDfileOpen("UsesCase_MEDfield_17.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
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

  /* Create mesh link */
  if (MEDlinkWr(fid,meshname,"./UsesCase_MEDstructElement_1.med") < 0) {
    MESSAGE("ERROR : create mesh link ...");
    goto ERROR;
  }

  /*
   * Read struct element geometric type
   */
  strcpy(structelementname,MED_BEAM_NAME);
  geotype = MEDstructElementGeotype(fid,structelementname);


  /* create a geometrical transformation fonction */
  if (MEDinterpCr(fid, interpname, geotype, MED_FALSE, nvariable, maxdegree, nmaxcoefficient) < 0) {
    MESSAGE("ERROR : interpolation family creation ...");
    goto ERROR;
  }
  /* Basis functions creation */
  if (MEDinterpBaseFunctionWr(fid,interpname,1,ncoefficient1_1,power1_1,coefficient1_1) < 0) {
    MESSAGE("ERROR : first base function creation ...");
    goto ERROR;
  }
  
  if (MEDinterpBaseFunctionWr(fid,interpname,2,ncoefficient1_2,power1_2,coefficient1_2) < 0) {
    MESSAGE("ERROR : second base function creation ...");
    goto ERROR;
  }

  if (MEDinterpBaseFunctionWr(fid,interpname,3,ncoefficient1_3,power1_3,coefficient1_3) < 0) {
    MESSAGE("ERROR : third base function creation ...");
    goto ERROR;
  }


  /* create the families of integration points 
     for the struct element */
  if (MEDlocalizationWr(fid, localization, geotype, spacedim, 
			elementcoordinate, MED_FULL_INTERLACE, 
			nipoint, ipointcoordinate, weight, 
			MED_NO_INTERPOLATION, beamsectionname) < 0) {
    MESSAGE("ERROR : create famlily of integration points ...");
    goto ERROR; 
  }

  if (MEDlocalizationWr(fid, localization2, geotype, spacedim, 
			elementcoordinate, MED_FULL_INTERLACE, 
			nipoint, ipointcoordinate, weight, 
			interpname, beamsectionname) < 0) {
    MESSAGE("ERROR : create famlily of integration points ...");
    goto ERROR; 
  }

  /*
   * Temperature  field creation for beam struct element :
   * - 1 component, 3 integration points, 4 cells in the support mesh for each 
   *   the section of each integration point 
   * - mesh is the 3D computation mesh of UsesCase_MEDstructElement_1 use case.
   */
  if (MEDfieldCr(fid, fieldname, MED_FLOAT64,
		 ncomponent, componentname, componentunit,
		 "ms", meshname) < 0) {
    MESSAGE("ERROR : create field");
    goto ERROR;
  }

  if (MEDfieldValueWithProfileWr(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT, 
				 MED_STRUCT_ELEMENT, geotype, 
				 MED_COMPACT_STMODE, MED_NO_PROFILE, localization,
				 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
				 nbeam, (unsigned char*) tempvalue) < 0) {
    MESSAGE("ERROR : write field values on MED_BEAM ");
    goto ERROR;
  }

  if (MEDfieldValueWithProfileWr(fid, fieldname, MED_NO_DT, 1, MED_UNDEF_DT, 
				 MED_STRUCT_ELEMENT, geotype, 
				 MED_COMPACT_STMODE, MED_NO_PROFILE, localization2,
				 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
				 nbeam, (unsigned char*) tempvalue) < 0) {
    MESSAGE("ERROR : write field values on MED_BEAM ");
    goto ERROR;
  }

  /* unmount file objects */


  if ( MEDfileObjectsUnmount(fid, mfid, MED_MESH_SUPPORT) < 0 ) {
    MESSAGE("ERROR : file unmounting ...");
    goto ERROR;
  }

  if ( MEDfileObjectsUnmount(fid, sfid, MED_ELSTRUCT) < 0 ) {
    MESSAGE("ERROR : file unmounting ...");
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

