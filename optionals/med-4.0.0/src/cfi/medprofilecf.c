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
- MEDprofileWr
- MEDnProfile
- MEDprofileInfo
- MEDprofileRd
- MEDprofileSizeByName
*/

#define nmpffprw F77_FUNC(mpffprw,MPFFPRW)
#define nmpffnpf F77_FUNC(mpffnpf,MPFFNPF)
#define nmpffpfi F77_FUNC(mpffpfi,MPFFPFI)
#define nmpffprr F77_FUNC(mpffprr,MPFFPRR)
#define nmpffpsn F77_FUNC(mpffpsn,MPFFPSN)


#ifdef PPRO_NT
med_int
MPFFPRW(med_idt *fid, char *pname, unsigned int bidon1, med_int *pnamelen,
		  med_int *psize, med_int *profil)
#else
med_int
nmpffprw(med_idt *fid, char *pname, med_int *pnamelen, med_int *psize, med_int *profil)
#endif
{
  med_err _ret=0;
  char *_fn1;

  _fn1 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDprofileWr((const med_idt) *fid, 
				_fn1, 
				(med_int) *psize,
				(med_int *) profil);
  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int 
MPFFNPF(med_idt *fid)
#else
med_int 
nmpffnpf(med_idt *fid)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDnProfile((const med_idt) *fid); 

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MPFFPFI(med_idt *fid, med_int *it, char*pname, unsigned int bidon1, 
		  med_int *psize)
#else
med_int 
nmpffpfi(med_idt *fid, med_int *it, char*pname, med_int *psize)
#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1]="";

  _ret = (med_int) MEDprofileInfo((const med_idt)*fid, 
				  (const med_int) *it, 
				  _fs1,
				  (med_int *) psize);

  _MEDc2fString(_fs1,pname,MED_NAME_SIZE);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MPFFPSN(med_idt *fid, char*pname, unsigned int bidon1, med_int *pnamelen, 
		  med_int *psize)
#else
med_int 
nmpffpsn(med_idt *fid, char*pname, med_int *pnamelen, med_int *psize)
#endif
{
  med_int _ret=0;
  char *_fn1;
  med_int _psize;

  _fn1 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn1)
    return(-1);

  _psize = (med_int) MEDprofileSizeByName((med_idt)*fid, 
					  _fn1);

  *psize = (med_int) _psize;
  if (_psize < 0)
    _ret = _psize;

  _MEDcstringFree(_fn1);

  return _ret;
}



#ifdef PPRO_NT
med_int
MPFFPRR(med_idt *fid, char *pname, unsigned int bidon1, med_int *pnamelen,
		  med_int *profil)
#else
med_int
nmpffprr(med_idt *fid, char *pname, med_int *pnamelen, med_int *profil)
#endif
{
  med_err _ret=0;
  char *_fn1;

  _fn1 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDprofileRd((med_idt) *fid, 
				_fn1, 
				(med_int *) profil);
  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}
