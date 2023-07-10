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
- MEDnParameter.c  
- MEDparameterComputationStepInfo.c  
- MEDparameterCr.c  
- MEDparameterInfoByName.c  
- MEDparameterInfo.c  
- MEDparameterValueRd.c  
- MEDparameterValueWr.c
*/

#define nmprfcre F77_FUNC(mprfcre,MPRFCRE)
#define nmprfrvw F77_FUNC(mprfrvw,MPRFRVW)
#define nmprfivw F77_FUNC(mprfivw,MPRFIVW)
#define nmprfrvr F77_FUNC(mprfrvr,MPRFRVR)
#define nmprfivr F77_FUNC(mprfivr,MPRFIVR)
#define nmprfpri F77_FUNC(mprfpri,MPRFPRI)
#define nmprfpin F77_FUNC(mprfpin,MPRFPIN)
#define nmprfcsi F77_FUNC(mprfcsi,MPRFCSI)
#define nmprfnpr F77_FUNC(mprfnpr,MPRFNPR)

#ifdef PPRO_NT
med_int 
MPRFCRE(med_idt *fid, char *pname, unsigned int bidon1, med_int *lon1,
                  med_int *type, char *desc, unsigned int bidon2, med_int *lon2,
		  char *dtunit, unsigned int bidon3, med_int *lon3)
#else
med_int 
nmprfcre(med_idt *fid, char *pname, med_int *lon1,
	 med_int *type, char *desc, med_int *lon2,
	 char *dtunit, med_int *lon3)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2, *_fn3;
  med_parameter_type _type  = (med_parameter_type) *type;

  _fn1 = _MED2cstring(pname, (int) * lon1);
  _fn2 = _MED1cstring(desc, (int) * lon2,MED_COMMENT_SIZE);
  _fn3 = _MED2cstring(dtunit,  (int) * lon3);

  if (!_fn1 || !_fn2 || !_fn3)
    return(-1); 

  _ret = (med_int) MEDparameterCr((med_idt) *fid,
				  _fn1,
				  _type,
				  _fn2,
				  _fn3); 

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);

  return(_ret);   
}



#ifdef PPRO_NT
med_int
MPRFRVW(med_idt *fid, char *name, unsigned int bidon1, med_int *namelen,
		  med_int *numdt, med_int *numit, med_float *dt,
		  med_float *val)
#else
med_int
nmprfrvw(med_idt *fid, char *name, med_int *namelen,
	 med_int *numdt, med_int *numit, med_float *dt,
	 med_float *val)
#endif
{
  med_err _ret=0;
  char *_fn1;

  _fn1 = _MED2cstring((char *) name, (int) *namelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDparameterValueWr((const med_idt) *fid, 
				       _fn1, 
				       (med_int) *numdt,
				       (med_int) *numit, 
				       (med_float) *dt,
				       (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int
MPRFIVW(med_idt *fid, char *name, unsigned int bidon1, med_int *namelen,
		  med_int *numdt, med_int *numit, med_float *dt,
		  med_int *val)
#else
med_int
nmprfivw(med_idt *fid, char *name, med_int *namelen,
	 med_int *numdt, med_int *numit, med_float *dt,
	 med_int *val)
#endif
{
  med_err _ret=0;
  char *_fn1;

  _fn1 = _MED2cstring((char *) name, (int) *namelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDparameterValueWr((const med_idt) *fid, 
				       _fn1, 
				       (med_int) *numdt,
				       (med_int) *numit, 
				       (med_float) *dt,
				       (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}



#ifdef PPRO_NT
med_int
MPRFRVR(med_idt *fid, char *name, unsigned int bidon1, med_int *namelen,
		  med_int *numdt, med_int *numit, med_float *val)
#else
med_int
nmprfrvr(med_idt *fid, char *name, med_int *namelen,
	 med_int *numdt, med_int *numit, med_float *val)
#endif
{
  med_err _ret=0;
  char *_fn1;

  _fn1 = _MED2cstring((char *) name, (int) *namelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDparameterValueRd((const med_idt) *fid, 
				       _fn1, 
				       (med_int) *numdt,
				       (med_int) *numit, 
				       (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int
MPRFIVR(med_idt *fid, char *name, unsigned int bidon1, med_int *namelen,
		  med_int *numdt, med_int *numit, med_int *val)
#else
med_int
nmprfivr(med_idt *fid, char *name, med_int *namelen,
	 med_int *numdt, med_int *numit,  med_int *val)
#endif
{
  med_err _ret=0;
  char *_fn1;

  _fn1 = _MED2cstring((char *) name, (int) *namelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDparameterValueRd((const med_idt) *fid, 
				       _fn1, 
				       (med_int) *numdt,
				       (med_int) *numit, 
				       (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}

#ifdef PPRO_NT
med_int 
MPRFNPR(med_idt *fid)
#else
med_int 
nmprfnpr(med_idt *fid)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDnParameter((const med_idt) *fid); 

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MPRFPRI(med_idt *fid, med_int *it, char *name, unsigned int bidon1, 
		  med_int *type, char *desc, unsigned int bidon2,
		  char *dtunit, unsigned int bidon3, med_int *nstep) 
#else
med_int 
nmprfpri(med_idt *fid, med_int *it, char *name,  
	 med_int *type, char *desc,
	 char *dtunit, med_int *nstep)
#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1]="";
  char _fs2[MED_COMMENT_SIZE+1]="";
  char _fs3[MED_SNAME_SIZE+1]="";
  med_parameter_type _type;

  _ret = (med_int) MEDparameterInfo((const med_idt)*fid,
				    (const med_int) *it,
				    _fs1,
				    &_type,
				    _fs2,
				    _fs3,
				    (med_int *) nstep);


  *type =  (med_int) _type;

  _MEDc2fString(_fs1,name,MED_NAME_SIZE);
  _MEDc2fString(_fs2,desc,MED_COMMENT_SIZE);
  _MEDc2fString(_fs3,dtunit,MED_SNAME_SIZE);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MPRFPIN(med_idt *fid, char *name, unsigned int bidon1, med_int *namelen,
		  med_int *type, char *desc, unsigned int bidon2,
		  char *dtunit, unsigned int bidon3, 
		  med_int *nstep) 
#else
med_int 
nmprfpin(med_idt *fid, char *name, med_int *namelen,
	 med_int *type, char *desc,
	 char *dtunit, med_int *nstep)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_COMMENT_SIZE+1]="";
  char _fs3[MED_SNAME_SIZE+1];
  med_parameter_type _type;

  _fn1 = _MED2cstring(name, (int) *namelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDparameterInfoByName((const med_idt)*fid,
					  _fn1,
					  &_type,
					  _fs2,
					  _fs3,
					  (med_int *) nstep);


  *type =  (med_int) _type;

  _MEDc2fString(_fs2,desc,MED_COMMENT_SIZE);
  _MEDc2fString(_fs3,dtunit,MED_SNAME_SIZE);

  _MEDcstringFree(_fn1);

  return _ret;
}



#ifdef PPRO_NT
med_int
MPRFCSI(med_idt *fid, char* name, unsigned int bidon1, med_int *namelen,
		  med_int *it, med_int *numdt, med_int *numit, med_float *dt)
#else
med_int
nmprfcsi(med_idt *fid, char *name, med_int *namelen,
	 med_int *it, med_int *numdt, med_int *numit, med_float *dt)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) name, (int) *namelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDparameterComputationStepInfo((med_idt)*fid, 
						   _fn1,
						   (med_int) *it,
						   (med_int *) numdt,
						   (med_int *) numit,
						   (med_float *) dt);

  _MEDcstringFree(_fn1);

  return _ret;
}
