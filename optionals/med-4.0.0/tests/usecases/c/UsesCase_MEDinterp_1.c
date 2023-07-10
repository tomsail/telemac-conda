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
 * Interp use case 1 : write an interpolation family
 * In this example, the interpolation family can be used with
 * the TEMPERATURE field of UsesCase_MEDfield_10 use case
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  char interpname[MED_NAME_SIZE+1] = "MED_TRIA3 interpolation family";
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

  /* file creation */
  fid = MEDfileOpen("UsesCase_MEDinterp_1.med",MED_ACC_CREAT);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }
 
  /* Family interpolation creation :
     - reference element = MED_TRIA3 
     - integration points of UsesCase_MEDfield_10 use case 
     are used to build the interpolation                 
     - basis functions are P1(X)= 1-X1-X2, P2(X)= X1, P3(X)= X2 
   */
  if (MEDinterpCr(fid, interpname, MED_TRIA3, MED_FALSE, nvariable, maxdegree, nmaxcoefficient) < 0) {
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

  ret=0;
 ERROR:

  /* close file */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("ERROR : close file ...");             
    ret=-1; 
  } 
  
  return ret;
}
