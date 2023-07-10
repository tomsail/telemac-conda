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
- MEDsupportMeshCr  
- MEDnSupportMesh
- MEDsupportMeshInfoByName  
- MEDsupportMeshInfo        
- MEDsupportMeshnAxis
- MEDsupportMeshnAxisByName          
*/

#define nmsmfcre F77_FUNC(msmfcre,MSMFCRE) 
#define nmsmfnsm F77_FUNC(msmfnsm,MSMFNSM) 
#define nmsmfsmi F77_FUNC(msmfsmi,MSMFSMI)
#define nmsmfsni F77_FUNC(msmfsni,MSMFSMI)
#define nmsmfnax F77_FUNC(msmfnax,MSMFNAX)
#define nmsmfnan F77_FUNC(msmfnan,MSMFNAN)

#ifdef PPRO_NT
med_int 
MSMFCRE(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  med_int * sdim, med_int * mdim,
		  char *desc, unsigned int bidon2, med_int *desclen,
		  med_int *atype, char *aname, unsigned int bidon3, med_int *anamelen, 
		  char *aunit, unsigned int bidon4, med_int* aunitlen)
#else
med_int 
nmsmfcre(med_idt *fid, char *mname, med_int *mnamelen, 
	 med_int * sdim, med_int * mdim,
	 char *desc, med_int *desclen,
	 med_int *atype, char *aname, med_int *anamelen, 
	 char *aunit, med_int* aunitlen)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2, *_fn4, *_fn5;
  med_axis_type _axistype = (med_axis_type) *atype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
	return(-1);
  _fn2 = _MED2cstring((char *) desc, (int) *desclen);
  if (!_fn2)
    return(-1);
  _fn4 = _MED1cstring((char *) aname, (int) *anamelen, (int) *sdim*MED_SNAME_SIZE);
  if (!_fn4)
    return(-1);
  _fn5 = _MED1cstring((char *) aunit, (int) *aunitlen, (int) *sdim*MED_SNAME_SIZE);
  if (!_fn5)
    return(-1);

  _ret = (med_int) MEDsupportMeshCr((const med_idt) *fid, 
				    _fn1, 
				    (const med_int) *sdim, 
				    (const med_int) *mdim,
				    _fn2, 
				    _axistype,
				    _fn4, 
				    _fn5);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn4);
  _MEDcstringFree(_fn5);

  return _ret;
}


#ifdef PPRO_NT
med_int
MSMFNSM(med_idt *fid)
#else
med_int
nmsmfnsm(med_idt *fid)
#endif
{
  med_int _ret;

  _ret = (med_int) MEDnSupportMesh((const med_idt) *fid);

  return(_ret);
}



#ifdef PPRO_NT
med_int
MSMFSNI(med_idt *fid, char*mname, unsigned int bidon, med_int *mnamelen,
                  med_int * sdim, med_int * mdim, 
		  char *desc, unsigned int bidon2,
		  med_int *atype,
		  char *aname, unsigned int bidon4,
		  char *aunit, unsigned int bidon5)
#else
med_int
nmsmfsni(med_idt *fid, char* mname, med_int *mnamelen,
	 med_int * sdim, med_int * mdim, 
	 char *desc,med_int *atype, char *aname, char *aunit)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_COMMENT_SIZE+1]="";
  char *_fs4, *_fs5;
  med_axis_type _atype;
  med_int _sdim;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _sdim = MEDsupportMeshnAxisByName((const med_idt)*fid, _fn1);
  if (_sdim < 0)
    return (-1);

  _fs4 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_sdim)+1);*_fs4='\0';
  _fs5 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_sdim)+1);*_fs5='\0';

  if (!(_fs4&&_fs5))
    return (-1);

  _ret = (med_int) MEDsupportMeshInfoByName((const med_idt)*fid, 
					    _fn1,
					    (med_int *) sdim, 
					    (med_int *) mdim, 
					    _fs2,
					    &_atype,
					    _fs4, 
					    _fs5);


  *atype = (med_int) _atype;

  _MEDc2fString(_fs2,desc,MED_COMMENT_SIZE);
  _MEDc2fString(_fs4,aname,MED_SNAME_SIZE*(*sdim));
  _MEDc2fString(_fs5,aunit,MED_SNAME_SIZE*(*sdim));

  _MEDcstringFree(_fn1);
  free(_fs4);
  free(_fs5);

  return _ret;
}


#ifdef PPRO_NT
med_int
MSMFSMI(med_idt *fid, med_int *it, char*mname, unsigned int bidon, 
                  med_int * sdim, med_int * mdim, 
		  char *desc, unsigned int bidon2,
		  med_int *atype,
		  char *aname, unsigned int bidon4,
		  char *aunit, unsigned int bidon5)
#else
med_int
nmsmfsmi(med_idt *fid, med_int *it, char* mname, 
	 med_int * sdim, med_int * mdim, 
	 char *desc,med_int *atype, char *aname, char *aunit)
#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1]="";
  char _fs2[MED_COMMENT_SIZE+1]="";
  char *_fs4, *_fs5;
  med_axis_type _atype;
  med_int _sdim;

  _sdim = MEDsupportMeshnAxis((const med_idt)*fid, (med_int) *it);
  if (_sdim < 0)
    return (-1);

  _fs4 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_sdim)+1);*_fs4='\0';
  _fs5 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_sdim)+1);*_fs5='\0';

  if (!(_fs4&&_fs5))
    return (-1);

  _ret = (med_int) MEDsupportMeshInfo((const med_idt)*fid, 
				      (med_int) *it,
				      _fs1,
				      (med_int *) sdim, 
				      (med_int *) mdim, 
				      _fs2,
				      &_atype,
				      _fs4, 
				      _fs5);
  

  *atype = (med_int) _atype;

  _MEDc2fString(_fs1,mname,MED_NAME_SIZE);
  _MEDc2fString(_fs2,desc,MED_COMMENT_SIZE);
  _MEDc2fString(_fs4,aname,MED_SNAME_SIZE*(*sdim));
  _MEDc2fString(_fs5,aunit,MED_SNAME_SIZE*(*sdim));

  free(_fs4);
  free(_fs5);

  return _ret;
}



#ifdef PPRO_NT
med_int
MSMFNAN(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen)
#else
med_int
nmsmfnan(med_idt *fid, char *mname, med_int *mnamelen)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDsupportMeshnAxisByName((const med_idt) *fid, (const char*) _fn1);

  _MEDcstringFree(_fn1);

  return(_ret);
}



#ifdef PPRO_NT
med_int 
MSMFNAX(med_idt *fid, med_int *it)
#else
med_int 
nmsmfnax(med_idt *fid, med_int *it)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDsupportMeshnAxis((const med_idt) *fid, (const med_int) *it); 

  return(_ret); 
}
