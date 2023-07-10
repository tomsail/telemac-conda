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
#include "med_config.h"
#include "med_outils.h"
#include <string.h>
#include <stdlib.h>

/*
From Fortran call of following C functions :
- MEDlinkWr  
- MEDlinkInfo  
- MEDlinkRd  
- MEDnLink
- MEDlinkInfoByName  
*/

#define nmlnfliw F77_FUNC(mlnfliw,MLNFLIW)
#define nmlnflir F77_FUNC(mlnflir,MLNFLIR)
#define nmlnfnln F77_FUNC(mlnfnln,MLNFNLN)
#define nmlnflni F77_FUNC(mlnflni,MLNFLNI)
#define nmlnflai F77_FUNC(mlnflai,MLNFLAI)


#ifdef PPRO_NT
med_int
MLNFLIW(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  char *lname, unsigned int bidon2, med_int *lnamelen)
#else
med_int
nmlnfliw(med_idt *fid, char *mname, med_int *mnamelen,
	 char *lname, med_int *lnamelen)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
	return(-1);
  _fn2 = _MED2cstring((char *) lname, (int) *lnamelen);
  if (!_fn2)
    return(-1); 


  _ret = (med_int) MEDlinkWr((const med_idt) *fid, 
			     _fn1, 
			     _fn2);
  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int 
MLNFNLN(med_idt *fid)
#else
med_int 
nmlnfnln(med_idt *fid)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDnLink((const med_idt) *fid); 

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MLNFLNI(med_idt *fid, med_int *it, char*mname, unsigned int bidon1, 
		  med_int *lsize)
#else
med_int
nmlnflni(med_idt *fid, med_int *it, char*mname, med_int *lsize)
#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1]="";

  _ret = (med_int) MEDlinkInfo((const med_idt)*fid,
			       (const med_int) *it,
			       _fs1,
			       (med_int *) lsize);

  _MEDc2fString(_fs1,mname,MED_NAME_SIZE);

  return _ret;
}

#ifdef PPRO_NT
med_int 
MLNFLAI(med_idt *fid, char*mname, unsigned int bidon1, med_int *mnamelen, med_int *lsize)
#else
med_int
nmlnflai(med_idt *fid, char*mname, med_int *mnamelen, med_int *lsize)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDlinkInfoByName((const med_idt)*fid,
				      _fn1);

  *lsize = (med_int) _ret;

  _MEDcstringFree(_fn1);

  return _ret;
}


#ifdef PPRO_NT
med_int
MLNFLIR(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  char *lname, unsigned int bidon2, med_int *lnamelen)
#else
med_int
nmlnflir(med_idt *fid, char *mname, med_int *mnamelen, char *lname, med_int *lnamelen)
#endif
{
  med_err _ret=0;
  med_int _size;
  char *_fn1,*_fn2;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _size = MEDlinkInfoByName((const med_idt) *fid,_fn1);
  _fn2 = (char *) malloc(sizeof(char)*_size+1);
  if (!_fn2)
    return(-1);
  *_fn2='\0';

/*   ISCRUTE(_size); */
/*   SSCRUTE(_fn2); */

  _ret = (med_int) MEDlinkRd((const med_idt) *fid,
			     _fn1,
			     _fn2);
/*   SSCRUTE(_fn2); */
  _MEDc2fString(_fn2,lname,*lnamelen);

  _MEDcstringFree(_fn1);
  free(_fn2);

  return (_ret);
}
