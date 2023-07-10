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
- MEDinterpCr   
- MEDinterpBaseFunctionWr 
- MEDinterpBaseFunctionRd  
- MEDnInterp
- MEDinterpInfo   
- MEDinterpInfoByName  
- MEDinterpBaseFunctionCoeffSize   
*/

#define nmipfcre F77_FUNC(mipfcre,MIPFCRE)
#define nmipfbfw F77_FUNC(mipfbfw,MIPFBFW)
#define nmipfbfr F77_FUNC(mipfbfr,MIPFBFR)
#define nmipfnip F77_FUNC(mipfnip,MIPFNIP)
#define nmipfiin F77_FUNC(mipfiin,MIPFIIN)
#define nmipfipi F77_FUNC(mipfipi,MIPFIPI)
#define nmipfcsz F77_FUNC(mipfcsz,MIPFCSZ)


#ifdef PPRO_NT
med_int 
MIPFCRE(med_idt *fid,
                  char *name,  unsigned int bidon1, med_int *lon1,
		  med_int *gtype, med_int *cnode, med_int *nvar,
		  med_int *mdeg, med_int *nmaxc)
#else
med_int 
nmipfcre(med_idt *fid,
	 char *name, med_int *lon1,
	 med_int *gtype, med_int *cnode, med_int *nvar,
	 med_int *mdeg, med_int *nmaxc)
#endif
{
  med_int _ret;
  char *_fn1;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_bool _cnode = (med_bool) *cnode;

  _fn1 = _MED2cstring(name, (int) * lon1);

  if (!_fn1 )
    return(-1); 

  _ret = (med_int) MEDinterpCr((med_idt) *fid,
			       _fn1,
			       _gtype,
			       _cnode,
                               (med_int) *nvar,
	                       (med_int) *mdeg, 
                               (med_int) *nmaxc ); 

  _MEDcstringFree(_fn1);

  return(_ret);   
}


#ifdef PPRO_NT
med_int 
MIPFBFW(med_idt *fid,
                  char *name,  unsigned int bidon1, med_int *lon1,
		  med_int *it, med_int *nc, med_int *pw, med_float *co)
#else
med_int 
nmipfbfw(med_idt *fid,
	 char *name, med_int *lon1,
	 med_int *it, med_int *nc, med_int *pw, med_float *co)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring(name, (int) * lon1);

  if (!_fn1 )
    return(-1); 

  _ret = (med_int) MEDinterpBaseFunctionWr((med_idt) *fid,
					   _fn1,
					   (med_int) *it,
					   (med_int) *nc,
					   (med_int *) pw,
					   (med_float *) co ); 

  _MEDcstringFree(_fn1);

  return(_ret);   
}


#ifdef PPRO_NT
med_int 
MIPFBFR(med_idt *fid,
                  char *name,  unsigned int bidon1, med_int *lon1,
		  med_int *it, med_int *nc, med_int *pw, med_float *co)
#else
med_int 
nmipfbfr(med_idt *fid,
	 char *name, med_int *lon1,
	 med_int *it, med_int *nc, med_int *pw, med_float *co)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring(name, (int) * lon1);

  if (!_fn1 )
    return(-1); 

  _ret = (med_int) MEDinterpBaseFunctionRd((med_idt) *fid,
					   _fn1,
					   (med_int) *it,
					   (med_int *) nc,
					   (med_int *) pw,
					   (med_float *) co ); 

  _MEDcstringFree(_fn1);

  return(_ret);   
}



#ifdef PPRO_NT
med_int 
MIPFNIP(med_idt *fid)
#else
med_int 
nmipfnip(med_idt *fid)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDnInterp((const med_idt) *fid); 

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MIPFIIN(med_idt *fid,
                  char *name,  unsigned int bidon1, med_int *lon1,
		  med_int *gtype, med_int *cnode, med_int *nbf, med_int *nvar,
		  med_int *mdeg, med_int *nmaxc)
#else
med_int 
nmipfiin(med_idt *fid,
	 char *name, med_int *lon1,
	 med_int *gtype, med_int *cnode,  med_int *nbf, med_int *nvar,
	 med_int *mdeg, med_int *nmaxc)
#endif
{
  med_int _ret;
  char *_fn1;
  med_geometry_type _gtype;
  med_bool _cnode;

  _fn1 = _MED2cstring(name, (int) * lon1);

  if (!_fn1 )
    return(-1); 

  _ret = (med_int) MEDinterpInfoByName((med_idt) *fid,
				       _fn1,
				       &_gtype,
				       &_cnode,
				       (med_int*) nbf,
				       (med_int*) nvar,
				       (med_int*) mdeg, 
				       (med_int*) nmaxc ); 

  *gtype = (med_int) _gtype;
  *cnode = (med_int) _cnode;

  _MEDcstringFree(_fn1);

  return(_ret);
}



#ifdef PPRO_NT
med_int 
MIPFIPI(med_idt *fid, med_int *it,
                  char *name,  unsigned int bidon1,
		  med_int *gtype, med_int *cnode, med_int *nbf, med_int *nvar,
		  med_int *mdeg, med_int *nmaxc)
#else
med_int 
nmipfipi(med_idt *fid, med_int *it, char *name,
	 med_int *gtype, med_int *cnode,  med_int *nbf, med_int *nvar,
	 med_int *mdeg, med_int *nmaxc)
#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1];
  med_geometry_type _gtype;
  med_bool _cnode;

  _ret = (med_int) MEDinterpInfo((med_idt) *fid,
				 (med_int) *it,
				 _fs1,
				 &_gtype,
				   &_cnode,
				 (med_int *) nbf,
				 (med_int *) nvar,
				 (med_int *) mdeg, 
				 (med_int *) nmaxc ); 

  _MEDc2fString(_fs1,name,MED_NAME_SIZE);  
  *gtype = (med_int) _gtype;
  *cnode = (med_int) _cnode;

  return(_ret);
}



#ifdef PPRO_NT
med_int 
MIPFCSZ(med_idt *fid, 
                  char *name,  unsigned int bidon1, med_int *lon1,
		  med_int *it)
#else
med_int 
nmipfcsz(med_idt *fid, 
	 char *name, med_int *lon1,
	 med_int *it)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring(name, (int) *lon1);

  if (!_fn1 )
    return(-1); 

  _ret = (med_int) MEDinterpBaseFunctionCoefSize((med_idt) *fid,
						 _fn1,
						 (med_int) *it); 

  _MEDcstringFree(_fn1);

  return(_ret);
}


