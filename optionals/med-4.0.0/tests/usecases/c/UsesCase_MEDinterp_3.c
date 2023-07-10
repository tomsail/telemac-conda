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
 * Interp use case 1 : read an interpolation family with a generic access by an iterator.
 * In this example, the interpolation family can be used with
 * the TEMPERATURE field of UsesCase_MEDfield_10 use case
 */

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#include <string.h>

int main (int argc, char **argv) {
  med_idt fid;
  char              interpname[MED_NAME_SIZE+1]="";
  med_int           ninterp                    =0;
  int               it                         =0;
  med_geometry_type geotype                    =MED_NONE;
  med_bool          cellnodes                  =MED_FALSE;
  med_int           nbasisfunc              =0;
  med_int           nvariable               =0;
  med_int           maxdegree                  =0;
  med_int           nmaxcoefficient            =0;
  int               basisfuncit                =0;
  int               powerit                    =0;
  med_int           ncoefficient            =0;
  med_int*          power                      =NULL;
  med_float*        coefficient                =NULL;
  int               coefficientit              =0;
  int ret=-1;


  /* file creation */
  fid = MEDfileOpen("UsesCase_MEDinterp_1.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("ERROR : file creation ...");
    goto ERROR;
  }
 
  /* how many interpolation family in the file ? */
  if ((ninterp = MEDnInterp(fid)) < 0) {
    MESSAGE("ERROR : read number of interpolation ...");
    goto ERROR;
  }

  /* read each interpolation family */
  /* with an access by an iterator */
  for (it=1; it<= ninterp; it++) {

    if (MEDinterpInfo(fid,it,interpname,&geotype,&cellnodes,&nbasisfunc,
		      &nvariable,&maxdegree,&nmaxcoefficient) < 0) {
      MESSAGE("ERROR : interpolation function information ...");
      goto ERROR;
    }

    /* read each basis function */
    for ( basisfuncit=1; basisfuncit<= nbasisfunc; ++basisfuncit) {
      if ((ncoefficient = MEDinterpBaseFunctionCoefSize(fid,interpname,basisfuncit) ) <0 ) {
	MESSAGE("ERROR : read number of coefficient in the base function ...");
	goto ERROR;
      }
      
      coefficient = (med_float*) calloc(sizeof(med_float),ncoefficient);
      power       = (med_int*)   calloc(sizeof(med_int),nvariable*ncoefficient);
      
      if (MEDinterpBaseFunctionRd(fid,interpname,basisfuncit,&ncoefficient,power,coefficient) < 0) {
	MESSAGE("ERROR : read base function ...");
	free(coefficient); free(power);
	goto ERROR;
      }

      free(coefficient);
      free(power);
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
