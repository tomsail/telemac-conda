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
 * Field use case 15 : write a field in a MED file with
 * values defined on struct elements
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>


int main (int argc, char **argv) {
  med_idt fid=0,mfid=0;
  const char meshname[MED_NAME_SIZE+1] = "COMPUT_MESH";
  const char fieldname[MED_NAME_SIZE+1] = "SPEED";
  const med_int ncomponent = 3;
  /*                                              123456789012345612345678901234561234567890123456 */
  const char componentname[3*MED_SNAME_SIZE+1] = "Vx              Vy              Vz";
  const char componentunit[3*MED_SNAME_SIZE+1] = "m/s             m/s             m/s";
  med_geometry_type geotype=MED_NONE;
  const med_int npart = 3;
  char structelementname[MED_NAME_SIZE+1]="";
  const med_float part_speed1[3*3] = { 1.1, 2.2, 3.3,
				 4.4, 5.5, 6.6,
				 7.7, 8.8, 9.9 };
  int ret=-1;


  /* File creation to write the field */
  fid = MEDfileOpen("UsesCase_MEDfield_15.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
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
  if (( mfid=MEDfileObjectsMount(fid,  "UsesCase_MEDstructElement_1.med",MED_ELSTRUCT)) < 0 ) {
    MESSAGE("ERROR : file mounting ...");
    goto ERROR;
  }

  strcpy(structelementname,MED_PARTICLE_NAME);
  geotype = MEDstructElementGeotype(fid,structelementname);


  /*
   * Speed field  creation for particles :
   * - 3 component
   * - component unit : m/s
   * - mesh is the 3D computation mesh of UsesCase_MEDstructElement_1 use case.
   * - computation step unit in 'ms'
   */
  if (MEDfieldCr(fid, fieldname, MED_FLOAT64,
		 ncomponent, componentname, componentunit,
		 "ms", meshname) < 0) {
    MESSAGE("ERROR : create field");
    goto ERROR;
  }

  if (MEDfieldValueWithProfileWr(fid, fieldname, MED_NO_DT, MED_NO_IT, MED_UNDEF_DT, MED_STRUCT_ELEMENT, geotype,
				 MED_COMPACT_STMODE, MED_NO_PROFILE, MED_NO_LOCALIZATION,
				 MED_FULL_INTERLACE, MED_ALL_CONSTITUENT,
				 npart, (unsigned char*) part_speed1) < 0) {
    MESSAGE("ERROR : write field values on MED_PARTICLE ");
    goto ERROR;
  }

  if ( MEDfileObjectsUnmount(fid, mfid, MED_ELSTRUCT) < 0 ) {
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

