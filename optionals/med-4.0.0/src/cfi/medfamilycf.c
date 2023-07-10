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
- MEDfamilyCr
- MEDnFamily 
- MEDnFamilyGroup
- MEDfamilyInfo
- MEDfamily23Info
- MEDnFamily23Attribute
*/

#define nmfafcre F77_FUNC(mfafcre,MFAFCRE)
#define nmfafnfa F77_FUNC(mfafnfa,MFAFNFA)
#define nmfafnfg F77_FUNC(mfafnfg,MFAFNFG)
#define nmfaffai F77_FUNC(mfaffai,MFAFFAI)
#define nmfafona F77_FUNC(mfafona,MFAFONA)
#define nmfafofi F77_FUNC(mfafofi,MFAFOFI)


#ifdef PPRO_NT
med_int
MFAFCRE(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  char *fname, unsigned int bidon2, med_int *fnamelen,
		  med_int *fnum, med_int *ng, char* gname, unsigned int bidon3,
		  med_int *gnamelen)
#else
med_int
nmfafcre(med_idt *fid, char *mname, med_int *mnamelen,
	 char *fname, med_int *fnamelen,
	 med_int *fnum, med_int *ng, char* gname,
	 med_int *gnamelen)
#endif
{
  char *_fn1,*_fn2,*_fn3;
  med_err _ret;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
	return(-1);
  _fn2 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn2)
	return(-1);
  _fn3 = _MED1cstring((char *) gname, (int) *gnamelen, (int) *ng*MED_LNAME_SIZE);
  if (!_fn3)
    return(-1);

  _ret = (med_int) MEDfamilyCr((med_idt) *fid,
			       _fn1,
			       _fn2,
			       (med_int) *fnum,
			       (med_int) *ng,
			       _fn3);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);

  return(_ret);
}




#ifdef PPRO_NT
int
MFAFNFA(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen)
#else
med_int
nmfafnfa(med_idt *fid, char *mname, med_int *mnamelen)
#endif
{
  med_int _ret;
  char *  _fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDnFamily((med_idt)*fid,
			      _fn1);

  _MEDcstringFree(_fn1);

  return(_ret);
}




#ifdef PPRO_NT
int
MFAFNFG(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, med_int *it)
#else
med_int
nmfafnfg(med_idt *fid, char *mname, med_int *mnamelen, med_int *it)
#endif
{
  med_int _ret;
  char *  _fn1;

  _fn1 = _MED2cstring(mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDnFamilyGroup((med_idt)*fid,
				   _fn1,
				   (int)*it);

  _MEDcstringFree(_fn1);

  return(_ret);
}



#ifdef  PPRO_NT
int
MFAFFAI(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
                  med_int *it, char *fname, unsigned int bidon2,
	          med_int *fnum, char *gname , int unsigned bidon4)
#else
med_int
nmfaffai(med_idt *fid, char *mname,med_int *mnamelen,
	 med_int *it, char *fname,
	 med_int *fnum, char *gname)
#endif
{
  med_int _ret;
  char * _fn1,*_fs2;
  char _fn3[MED_NAME_SIZE+1]="";
  int _ng,_j;
  char _str[MED_LNAME_SIZE+1]="";

  _fn1 = _MED2cstring(mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ng = MEDnFamilyGroup((med_idt) *fid,_fn1,*it);
  if (_ng < 0)
    return (-1);

  _fs2 = (char *) malloc(MED_LNAME_SIZE*_ng+1);*_fs2='\0';

  _ret = (med_int) MEDfamilyInfo((med_idt) *fid,
				 _fn1,
				 (int) *it,
				 (char *)_fn3 ,
				 (med_int *)fnum,
				 _fs2);

  _MEDc2fString(_fn3,fname,MED_NAME_SIZE);

  for (_j=0;_j<_ng;_j++) {
    strncpy(_str,&_fs2[_j*MED_LNAME_SIZE],MED_LNAME_SIZE);_str[MED_LNAME_SIZE]='\0';
    _MEDc2fString(_str,gname+_j*MED_LNAME_SIZE,MED_LNAME_SIZE);
  }

  _MEDcstringFree(_fn1);
  free(_fs2);

  return(_ret);
}




#ifdef PPRO_NT
int
MFAFONA(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, med_int *it)
#else
med_int
nmfafona(med_idt *fid, char *mname, med_int *mnamelen, med_int *it)
#endif
{
  med_int _ret;
  char *  _fn1;

  _fn1 = _MED2cstring(mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDnFamily23Attribute((med_idt)*fid,
					 _fn1,
					 (int)*it);

  _MEDcstringFree(_fn1);

  return(_ret);
}



#ifdef  PPRO_NT
int
MFAFOFI(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
                  med_int *it, char *fname, unsigned int bidon2,
		  med_int *attr_ident, med_int *attr_val,
	          char *attr_desc, unsigned int bidon3,
	          med_int *fnum, char *gname , int unsigned bidon4)
#else
med_int
nmfafofi(med_idt *fid, char *mname,med_int *mnamelen,
	 med_int *it, char *fname,
	 med_int *attr_ident, med_int *attr_val,
	 char *attr_desc,
	 med_int *fnum, char *gname)
#endif
{
  med_int _ret;
  char * _fn1,*_fs2;
  char _fn3[MED_NAME_SIZE+1]="";
  char *_fn4;
  int _ng,_j, _na;
  char _str[MED_LNAME_SIZE+1]="";

  _fn1 = _MED2cstring(mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ng = MEDnFamilyGroup((med_idt) *fid,_fn1,*it);
  if (_ng < 0)
    return (-1);

  _na = MEDnFamily23Attribute((med_idt) *fid,_fn1,*it);
  if (_na < 0)
    return (-1);

  _fs2 = (char *) malloc(MED_LNAME_SIZE*_ng+1);*_fs2='\0';
  _fn4 = (char *) malloc(MED_COMMENT_SIZE*_na+1);*_fn4='\0';

  _ret = (med_int) MEDfamily23Info((med_idt) *fid,
				   _fn1,
				   (int) *it,
				   (char *)_fn3 ,
				   (med_int *) attr_ident,
				   (med_int *) attr_val,
				   (char *) _fn4,
				   (med_int *) fnum,
				   _fs2);

  _MEDc2fString(_fn3,fname,MED_NAME_SIZE);
  _MEDc2fString(_fn4,attr_desc,MED_COMMENT_SIZE*_na);

  for (_j=0;_j<_ng;_j++) {
    strncpy(_str,&_fs2[_j*MED_LNAME_SIZE],MED_LNAME_SIZE);_str[MED_LNAME_SIZE]='\0';
    _MEDc2fString(_str,gname+_j*MED_LNAME_SIZE,MED_LNAME_SIZE);
  }


  _MEDcstringFree(_fn1);
  free(_fs2);
  free(_fn4);
  return(_ret);
}
