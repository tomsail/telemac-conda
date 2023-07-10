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
- MEDfieldCr
- MEDfieldValueWr
- MEDfieldValueWithProfileWr
- MEDnField
- MEDfieldnComponent
- MEDfieldInfo   
- MEDfieldComputingStepInfo
- MEDfieldnProfile  
- MEDfieldnValue
- MEDfieldnValueWithProfile 
- MEDfieldValueRd
- MEDfieldValueWithProfileRd.c
- MEDfieldInterpWr
- MEDfieldnInterp    
- MEDfieldInterpInfo 
- MEDfieldValueAdvancedWr
- MEDfieldValueAdvancedRd           
- MEDfieldnComponentByName                     
- MEDfieldInfoByName
- MEDfieldnValueWithProfileByName   
- MEDfield23ComputingStepMeshInfo  
- MEDfield23nProfile  
- MEDfield23nValueWithProfile  
- MEDfield23ValueWithProfileRd
*/

#define nmfdfcre F77_FUNC(mfdfcre,MFDFCRE)
#define nmfdfrvw F77_FUNC(mfdfrvw,MFDFRVW)
#define nmfdfivw F77_FUNC(mfdfivw,MFDFIVW)
#define nmfdfrpw F77_FUNC(mfdfrpw,MFDFRPW)
#define nmfdfipw F77_FUNC(mfdfipw,MFDFIPW)
#define nmfdfraw F77_FUNC(mfdfraw,MFDFRAW)
#define nmfdfiaw F77_FUNC(mfdfiaw,MFDFIAW)
#define nmfdfnfd F77_FUNC(mfdfnfd,MFDFNFD)
#define nmfdfnfc F77_FUNC(mfdfnfc,MFDFNFC)
#define nmfdfncn F77_FUNC(mfdfncn,MFDFNCN)
#define nmfdffdi F77_FUNC(mfdffdi,MFDFFDI)
#define nmfdffin F77_FUNC(mfdffin,MFDFFIN)
#define nmfdfcsi F77_FUNC(mfdfcsi,MFDFCSI)
#define nmfdfcmi F77_FUNC(mfdfcmi,MFDFCMI)
#define nmfdfcmw F77_FUNC(mfdfcmw,MFDFCMW)
#define nmfdfnpf F77_FUNC(mfdfnpf,MFDFNPF)
#define nmfdfnva F77_FUNC(mfdfnva,MFDFNVA)
#define nmfdfnvp F77_FUNC(mfdfnvp,MFDFNVP)
#define nmfdfnpn F77_FUNC(mfdfnpn,MFDFNPN)
#define nmfdfrvr F77_FUNC(mfdfrvr,MFDFRVR)
#define nmfdfivr F77_FUNC(mfdfivr,MFDFIVR)
#define nmfdfrpr F77_FUNC(mfdfrpr,MFDFRPR)
#define nmfdfipr F77_FUNC(mfdfipr,MFDFIPR)
#define nmfdfrar F77_FUNC(mfdfrar,MFDFRAR)
#define nmfdfiar F77_FUNC(mfdfiar,MFDFIAR)
#define nmfdfinw F77_FUNC(mfdfinw,MFDFINW)
#define nmfdfnin F77_FUNC(mfdfnin,MFDFNIN)
#define nmfdfini F77_FUNC(mfdfini,MFDFINI)
#define nmfdfoci F77_FUNC(mfdfoci,MFDFOCI)
#define nmfdfonp F77_FUNC(mfdfonp,MFDFONP)
#define nmfdfonv F77_FUNC(mfdfonv,MFDFONV)
#define nmfdforr F77_FUNC(mfdforr,MFDFORR)
#define nmfdfoir F77_FUNC(mfdfoir,MFDFOIR)


#ifdef PPRO_NT
med_int
MFDFCRE(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *ftype, med_int *ncomp, char *cname, unsigned int bidon2, med_int *cnamelen,
		  char *cuname, unsigned int bidon3, med_int *cunamelen,
		  char *dtunit, unsigned int bidon4, med_int *dtunitlen,
		  char* mname, unsigned int bidon5, med_int *mnamelen)
#else
med_int
nmfdfcre(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *ftype, med_int *ncomp, char *cname, med_int *cnamelen,
	 char *cuname, med_int *cunamelen, char *dtunit, med_int *dtunitlen,
	 char* mname, med_int *mnamelen)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2,*_fn3,*_fn4,*_fn5;
  med_field_type _ftype = (med_field_type) *ftype;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
	return(-1);
  _fn2 = _MED1cstring((char *) cname, (int) *cnamelen, (int) *ncomp*MED_SNAME_SIZE);
  if (!_fn2)
    return(-1);
  _fn3 = _MED1cstring((char *) cuname, (int) *cunamelen, (int) *ncomp*MED_SNAME_SIZE);
  if (!_fn3)
    return(-1);
  _fn4 = _MED2cstring((char *) dtunit, (int) *dtunitlen);
  if (!_fn4)
	return(-1);
  _fn5 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
	return(-1);

  _ret = (med_int) MEDfieldCr((const med_idt) *fid,
			      _fn1,
			      _ftype,
			      (med_int) *ncomp,
			      _fn2,
			      _fn3,
			      _fn4,
			      _fn5);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);
  _MEDcstringFree(_fn4);
  _MEDcstringFree(_fn5);
  
  return (_ret);
}




#ifdef PPRO_NT
med_int
MFDFRVW(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_float *dt,
		  med_int *etype, med_int *gtype, med_int *swm,
		  med_int *cs, med_int *n, med_float *val)
#else
med_int
nmfdfrvw(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,
	 med_int *etype, med_int *gtype, med_int *swm,
	 med_int *cs, med_int *n, med_float *val)
#endif
{
  med_err _ret=0;
  char *_fn1;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldValueWr((const med_idt) *fid, 
				   _fn1, 
				   (med_int) *numdt,
				   (med_int) *numit, 
				   (med_float) *dt,
				   _etype, 
				   _gtype, 
				   _swm,
				   (med_int) *cs,
				   (med_int) *n,
				   (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}




#ifdef PPRO_NT
med_int
MFDFIVW(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_float *dt,
		  med_int *etype, med_int *gtype, med_int *swm,
		  med_int *cs, med_int *n, med_int *val)
#else
med_int
nmfdfivw(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,
	 med_int *etype, med_int *gtype, med_int *swm,
	 med_int *cs, med_int *n, med_int *val)
#endif
{
  med_err _ret=0;
  char *_fn1;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldValueWr((const med_idt) *fid, 
				   _fn1, 
				   (med_int) *numdt,
				   (med_int) *numit, 
				   (med_float) *dt,
				   _etype, 
				   _gtype, 
				   _swm,
				   (med_int) *cs,
				   (med_int) *n,
				   (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}



#ifdef PPRO_NT
med_int
MFDFRPW(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_float *dt,
		  med_int *etype, med_int *gtype, 
		  med_int *stm, char *pname, unsigned int bidon2, med_int *pnamelen,  
		  char *lname, unsigned int bidon3, med_int *lnamelen,
		  med_int *swm,med_int *cs, med_int *n, med_float *val)
#else
med_int
nmfdfrpw(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,
	 med_int *etype, med_int *gtype,
	 med_int *stm, char *pname, med_int *pnamelen,
	 char *lname, med_int *lnamelen,
	 med_int *swm,med_int *cs, med_int *n, med_float *val)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2,*_fn3;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_storage_mode _stm = (med_storage_mode) *stm;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn2)
    return(-1);

  _fn3 = _MED2cstring((char *) lname, (int) *lnamelen);
  if (!_fn3)
    return(-1);

  _ret = (med_int)  MEDfieldValueWithProfileWr((const med_idt) *fid,
					       _fn1,
					       (med_int) *numdt,
					       (med_int) *numit,
					       (med_float) *dt,
					       _etype,
					       _gtype,
					       _stm,
					       _fn2,
					       _fn3,
					       _swm,
					       (med_int) *cs,
					       (med_int) *n,
					       (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int
MFDFIPW(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_float *dt,
		  med_int *etype, med_int *gtype, 
		  med_int *stm, char *pname, unsigned int bidon2, med_int *pnamelen,  
		  char *lname, unsigned int bidon3, med_int *lnamelen, 
		  med_int *swm,med_int *cs, med_int *n, med_int *val)
#else
med_int
nmfdfipw(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,
	 med_int *etype, med_int *gtype, 
	 med_int *stm, char *pname, med_int *pnamelen,  
	 char *lname, med_int *lnamelen, 
	 med_int *swm,med_int *cs, med_int *n, med_int *val)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2,*_fn3;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_storage_mode _stm = (med_storage_mode) *stm;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn2)
    return(-1);

  _fn3 = _MED2cstring((char *) lname, (int) *lnamelen);
  if (!_fn3)
    return(-1);

  _ret = (med_int)  MEDfieldValueWithProfileWr((const med_idt) *fid, 
					       _fn1, 
					       (med_int) *numdt,
					       (med_int) *numit, 
					       (med_float) *dt,
					       _etype, 
					       _gtype, 
					       _stm,
					       _fn2,
					       _fn3,
					       _swm,
					       (med_int) *cs,
					       (med_int) *n,
					       (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int
MFDFRAW(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_float *dt,
		  med_int *etype, med_int *gtype,
		  char *lname, unsigned int bidon2, med_int *lnamelen, 
		  unsigned char **flt, med_float *val)
#else
med_int
nmfdfraw(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,
	 med_int *etype, med_int *gtype,
	 char *lname, med_int *lnamelen, 
	 unsigned char **flt, med_float *val)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) lname, (int) *lnamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDfieldValueAdvancedWr((const med_idt) *fid, 
					   _fn1, 
					   (med_int) *numdt,
					   (med_int) *numit, 
					   (med_float) *dt,
					   _etype, 
					   _gtype, 
					   _fn2,
					   _filter,
					   (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  
  return (_ret);
}



#ifdef PPRO_NT
med_int
MFDFIAW(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_float *dt,
		  med_int *etype, med_int *gtype,
		  char *lname, unsigned int bidon2, med_int *lnamelen, 
		  unsigned char **flt, med_int *val)
#else
med_int
nmfdfiaw(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,
	 med_int *etype, med_int *gtype,
	 char *lname, med_int *lnamelen, 
	 unsigned char **flt, med_int *val)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) lname, (int) *lnamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDfieldValueAdvancedWr((const med_idt) *fid, 
					   _fn1, 
					   (med_int) *numdt,
					   (med_int) *numit, 
					   (med_float) *dt,
					   _etype, 
					   _gtype, 
					   _fn2,
					   _filter,
					   (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  
  return (_ret);
}



#ifdef PPRO_NT
med_int 
MFDFNFD(med_idt *fid)
#else
med_int 
nmfdfnfd(med_idt *fid)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDnField((const med_idt) *fid); 

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MFDFNFC(med_idt *fid, med_int *ind)
#else
med_int 
nmfdfnfc(med_idt *fid, med_int *ind)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDfieldnComponent((const med_idt) *fid,
				      (med_int) *ind); 

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MFDFNCN(med_idt *fid,  
		  char* fname, unsigned int bidon1, med_int *fnamelen)
#else
med_int 
nmfdfncn(med_idt *fid, 
	 char* fname, med_int *fnamelen)
#endif
{
  med_int _ret; 
  char *_fn1;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDfieldnComponentByName((const med_idt) *fid,
					    _fn1); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MFDFFDI(med_idt *fid, med_int *it, char *fname, unsigned int bidon1, 
		  char*mname, unsigned int bidon2,
		  med_int *localmesh, med_int *type,
		  char *cname, unsigned int bidon3, 
		  char *cunit, unsigned int bidon4,
		  char *dtunit, unsigned int bidon5, 
		  med_int *nc)
#else
med_int 
nmfdffdi(med_idt *fid, med_int *it, char *fname,char *mname,
	 med_int *localmesh, med_int *type,
	 char *cname,char *cunit, char *dtunit, med_int *nc)
#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1]="";
  char _fs2[MED_NAME_SIZE+1]="";
  char _fs3[MED_SNAME_SIZE+1]="";
  char *_fs4, *_fs5;
  med_field_type _type;
  med_int _nc;
  med_bool _lmesh;

  _nc = MEDfieldnComponent((const med_idt)*fid, (const med_int) *it);
  if (_nc < 0)
    return (-1);

  _fs4 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_nc)+1);*_fs4='\0';
  _fs5 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_nc)+1);*_fs5='\0';

  if (!(_fs4&&_fs5))
    return (-1);

  _ret = (med_int) MEDfieldInfo((const med_idt)*fid,
				(const med_int) *it,
				_fs1,
				_fs2,
				&_lmesh,
				&_type,
				_fs4,
				_fs5,
				_fs3,
				(med_int *) nc);


  *type = (med_int) _type;
  *localmesh = (med_int) _lmesh;

  _MEDc2fString(_fs1,fname,MED_NAME_SIZE);
  _MEDc2fString(_fs2,mname,MED_NAME_SIZE);
  _MEDc2fString(_fs3,dtunit,MED_SNAME_SIZE);
  _MEDc2fString(_fs4,cname,MED_SNAME_SIZE*_nc);
  _MEDc2fString(_fs5,cunit,MED_SNAME_SIZE*_nc);

  free(_fs4);
  free(_fs5);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MFDFFIN(med_idt *fid, char *fname, unsigned int bidon1,med_int *fnamelen,
		  char*mname, unsigned int bidon2,
		  med_int *localmesh, med_int *type,
		  char *cname, unsigned int bidon3, 
		  char *cunit, unsigned int bidon4,
		  char *dtunit, unsigned int bidon5, 
		  med_int *nc)
#else
med_int 
nmfdffin(med_idt *fid, char *fname, med_int *fnamelen,
	 char*mname, med_int *localmesh, med_int *type,
	 char *cname, char *cunit, char *dtunit, 
	 med_int *nc)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_NAME_SIZE+1]="";
  char _fs3[MED_SNAME_SIZE+1]="";
  char *_fs4, *_fs5;
  med_field_type _type;
  med_int _nc;
  med_bool _lmesh;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _nc = MEDfieldnComponentByName((const med_idt)*fid, _fn1);
  if (_nc < 0)
    return (-1);

  _fs4 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_nc)+1);*_fs4='\0';
  _fs5 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_nc)+1);*_fs5='\0';

  if (!(_fs4&&_fs5))
    return (-1);

  _ret = (med_int) MEDfieldInfoByName((const med_idt)*fid,
				      _fn1,
				      _fs2,
				      &_lmesh,
				      &_type,
				      _fs4,
				      _fs5,
				      _fs3,
				      (med_int *) nc);


  *type = (med_int) _type;
  *localmesh = (med_int) _lmesh;

  _MEDc2fString(_fs2,mname,MED_NAME_SIZE);
  _MEDc2fString(_fs3,dtunit,MED_SNAME_SIZE);
  _MEDc2fString(_fs4,cname,MED_SNAME_SIZE*_nc);
  _MEDc2fString(_fs5,cunit,MED_SNAME_SIZE*_nc);

  free(_fs4);
  free(_fs5);

  _MEDcstringFree(_fn1);

  return _ret;
}



#ifdef PPRO_NT
med_int
MFDFCSI(med_idt *fid, char* fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *it, med_int *numdt, med_int *numit, med_float *dt)
#else
med_int
nmfdfcsi(med_idt *fid, char* fname, med_int *fnamelen,
	 med_int *it, med_int *numdt, med_int *numit, med_float *dt)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldComputingStepInfo((med_idt)*fid,
					     _fn1,
					     (med_int) *it,
					     (med_int *) numdt,
					     (med_int *) numit,
					     (med_float *) dt);

  _MEDcstringFree(_fn1);

  return _ret;
}


#ifdef PPRO_NT
med_int
MFDFCMI(med_idt *fid, char* fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *it, med_int *numdt, med_int *numit, med_float *dt, med_int *meshnumdt, med_int *meshnumit)
#else
med_int
nmfdfcmi(med_idt *fid, char* fname, med_int *fnamelen,
	 med_int *it, med_int *numdt, med_int *numit, med_float *dt,
	 med_int *meshnumdt, med_int *meshnumit)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldComputingStepMeshInfo((med_idt)*fid,
						 _fn1,
						 (med_int) *it,
						 (med_int *) numdt,
						 (med_int *) numit,
						 (med_float *) dt,
						 (med_int *) meshnumdt,
						 (med_int *) meshnumit);

  _MEDcstringFree(_fn1);

  return _ret;
}

#ifdef PPRO_NT
med_int
MFDFCMW(med_idt *fid, char* fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_int *meshnumdt, med_int *meshnumit)
#else
med_int
nmfdfcmw(med_idt *fid, char* fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *meshnumdt, med_int *meshnumit)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldComputingStepMeshWr((med_idt)*fid,
					       _fn1,
					       (med_int ) *numdt,
					       (med_int ) *numit,
					       (med_int ) *meshnumdt,
					       (med_int ) *meshnumit);

  _MEDcstringFree(_fn1);

  return _ret;
}



#ifdef PPRO_NT
med_int 
MFDFNPF(med_idt *fid, char*fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
		  char *dpname, unsigned int bidon2, char *dlname, unsigned int bidon3)
#else
med_int 
nmfdfnpf(med_idt *fid, char*fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
	 char *dpname, char *dlname)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_NAME_SIZE+1]="";
  char _fs3[MED_NAME_SIZE+1]="";
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldnProfile((const med_idt) *fid,
				    _fn1,
				    (med_int) *numdt,
				    (med_int) *numit,
				    _etype,
				    _gtype,
				    _fs2,
				    _fs3);

  _MEDc2fString(_fs2,dpname,MED_NAME_SIZE);
  _MEDc2fString(_fs3,dlname,MED_NAME_SIZE);

  _MEDcstringFree(_fn1);

  return(_ret);
}


#ifdef PPRO_NT
med_int
MFDFNVA(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_int *etype, med_int *gtype)
#else
med_int 
nmfdfnva(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_int *etype, med_int *gtype)
#endif
{
  med_int _ret;
  char *_fn1;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldnValue((med_idt) *fid,
				  _fn1,
				  (med_int) *numdt,
				  (med_int) *numit,
				  _etype,
				  _gtype); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MFDFNVP(med_idt *fid, char*fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
		  med_int *pit, med_int *stm, char *pname, unsigned int bidon2, 
		  med_int *psize, char *lname, unsigned int bidon3, med_int *nip)
#else
med_int 
nmfdfnvp(med_idt *fid, char*fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
	 med_int *pit, med_int *stm, char *pname, 
	 med_int *psize, char *lname, med_int *nip)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_NAME_SIZE+1]="";
  char _fs3[MED_NAME_SIZE+1]="";
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_storage_mode _stm = (med_storage_mode) *stm;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldnValueWithProfile((med_idt) *fid,
					     _fn1,
					     (med_int) *numdt,
					     (med_int) *numit,
					     _etype,
					     _gtype,
					     (med_int) *pit,
					     _stm,
					     _fs2,
					     (med_int *) psize,
					     _fs3,
					     (med_int *) nip);

  _MEDc2fString(_fs2,pname,MED_NAME_SIZE);
  _MEDc2fString(_fs3,lname,MED_NAME_SIZE);

  _MEDcstringFree(_fn1);

  return(_ret);
}


#ifdef PPRO_NT
med_int 
MFDFNPN(med_idt *fid, char* fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
		  char *pname, unsigned int bidon2, med_int *pnamelen, med_int *stm, 
		  med_int *psize, char *lname, unsigned int bidon3, med_int *nip)
#else
med_int 
nmfdfnpn(med_idt *fid, char* fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
	 char *pname, med_int *pnamelen, med_int *stm, 
	 med_int *psize, char *lname, med_int *nip)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  char _fs3[MED_NAME_SIZE+1]="";
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_storage_mode _stm = (med_storage_mode) *stm;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDfieldnValueWithProfileByName((med_idt) *fid,
						   _fn1,
						   (med_int) *numdt,
						   (med_int) *numit,
						   _etype,
						   _gtype,
						   _fn2,
						   _stm,
						   (med_int*) psize,
						   _fs3,
						   (med_int*) nip);

  _MEDc2fString(_fs3,lname,MED_NAME_SIZE);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret);
}



#ifdef PPRO_NT
med_int
MFDFRVR(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, 
		  med_int *etype, med_int *gtype, med_int *swm,
		  med_int *cs, med_float *val)
#else
med_int
nmfdfrvr(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, 
	 med_int *etype, med_int *gtype, med_int *swm,
	 med_int *cs, med_float *val)
#endif
{
  med_err _ret=0;
  char *_fn1;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldValueRd((const med_idt) *fid, 
				   _fn1, 
				   (med_int) *numdt,
				   (med_int) *numit, 
				   _etype, 
				   _gtype, 
				   _swm,
				   (med_int) *cs,
				   (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}




#ifdef PPRO_NT
med_int
MFDFIVR(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit,
		  med_int *etype, med_int *gtype, med_int *swm,
		  med_int *cs, med_int *val)
#else
med_int
nmfdfivr(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *etype, med_int *gtype, med_int *swm,
	 med_int *cs, med_int *val)
#endif
{
  med_err _ret=0;
  char *_fn1;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldValueRd((const med_idt) *fid, 
				   _fn1, 
				   (med_int) *numdt,
				   (med_int) *numit, 
				   _etype, 
				   _gtype, 
				   _swm,
				   (med_int) *cs,
				   (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}



#ifdef PPRO_NT
med_int
MFDFRPR(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit,
		  med_int *etype, med_int *gtype, 
		  med_int *stm, char *pname, unsigned int bidon2, med_int *pnamelen,  
		  med_int *swm,med_int *cs, med_float *val)
#else
med_int
nmfdfrpr(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *etype, med_int *gtype, 
	 med_int *stm, char *pname, med_int *pnamelen,   
	 med_int *swm,med_int *cs, med_float *val)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_storage_mode _stm = (med_storage_mode) *stm;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int)  MEDfieldValueWithProfileRd((med_idt) *fid, 
					       _fn1, 
					       (med_int) *numdt,
					       (med_int) *numit, 
					       _etype, 
					       _gtype, 
					       _stm,
					       _fn2,
					       _swm,
					       (med_int) *cs,
					       (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int
MFDFIPR(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit,
		  med_int *etype, med_int *gtype, 
		  med_int *stm, char *pname, unsigned int bidon2, med_int *pnamelen,  
		  med_int *swm, med_int *cs, med_int *val)
#else
med_int
nmfdfipr(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, 
	 med_int *etype, med_int *gtype, 
	 med_int *stm, char *pname, med_int *pnamelen,  
	 med_int *swm,med_int *cs, med_int *val)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_storage_mode _stm = (med_storage_mode) *stm;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int)  MEDfieldValueWithProfileRd((med_idt) *fid, 
					       _fn1, 
					       (med_int) *numdt,
					       (med_int) *numit, 
					       _etype, 
					       _gtype, 
					       _stm,
					       _fn2,
					       _swm,
					       (med_int) *cs,
					       (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int
MFDFRAR(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit,
		  med_int *etype, med_int *gtype,
		  unsigned char **flt, med_float *val)
#else
med_int
nmfdfrar(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *etype, med_int *gtype,
	 unsigned char **flt, med_float *val)
#endif
{
  med_err _ret=0;
  char *_fn1;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldValueAdvancedRd((const med_idt) *fid, 
					   _fn1, 
					   (med_int) *numdt,
					   (med_int) *numit, 
					   _etype, 
					   _gtype, 
					   _filter,
					   (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int
MFDFIAR(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit,
		  med_int *etype, med_int *gtype,
		  unsigned char **flt, med_int *val)
#else
med_int
nmfdfiar(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *etype, med_int *gtype,
	 unsigned char **flt, med_int *val)
#endif
{
  med_err _ret=0;
  char *_fn1;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfieldValueAdvancedRd((const med_idt) *fid, 
					   _fn1, 
					   (med_int) *numdt,
					   (med_int) *numit, 
					   _etype, 
					   _gtype, 
					   _filter,
					   (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}



#ifdef PPRO_NT
med_int
MFDFINW(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  char *iname, unsigned int bidon2, med_int *inamelen)
#else
med_int
nmfdfinw(med_idt *fid, char *fname, med_int *fnamelen,
	 char *iname, med_int *inamelen)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) iname, (int) *inamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int)  MEDfieldInterpWr((med_idt) *fid, 
				     _fn1, 
				     _fn2);

  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  
  return (_ret);
}



#ifdef PPRO_NT
med_int 
MFDFNIN(med_idt *fid, 
		  char *fname, unsigned int bidon1, med_int *fnamelen)
#else
med_int 
nmfdfnin(med_idt *fid,char *fname, med_int *fnamelen)
#endif
{
  med_int _ret; 
  char *_fn1;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDfieldnInterp((const med_idt) *fid,_fn1); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}



#ifdef PPRO_NT
med_int
MFDFINI(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *it, char *iname, unsigned int bidon2)
#else
med_int
nmfdfini(med_idt *fid, char *fname, med_int *fnamelen, med_int *it, char *iname)
#endif
{
  med_err _ret=0;
  char *_fn1;
  char _fs1[MED_NAME_SIZE+1]="";

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int)  MEDfieldInterpInfo((med_idt) *fid, 
				       _fn1, 
				       (med_int) *it,
				       _fs1);

  _MEDc2fString(_fs1,iname,MED_NAME_SIZE);
  _MEDcstringFree(_fn1);
  
  return (_ret);
}



#ifdef PPRO_NT
med_int
MFDFOCI(med_idt *fid, char* fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *it, med_int *numdt, med_int *numit, med_float *dt,
		  med_int *nmesh, char *mname, unsigned int bidon2, med_int *lmesh,
		  med_int *mnumdt, med_int *mnumit)
#else
med_int
nmfdfoci(med_idt *fid, char* fname,med_int *fnamelen,
	 med_int *it, med_int *numdt, med_int *numit, med_float *dt,
	 med_int *nmesh, char *mname, med_int *lmesh,
	 med_int *mnumdt, med_int *mnumit)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs1[MED_NAME_SIZE+1]="";
  med_bool _lmesh;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfield23ComputingStepMeshInfo((med_idt)*fid,
						   _fn1,
						   (med_int) *it,
						   (med_int *) numdt,
						   (med_int *) numit,
						   (med_float *) dt,
						   (med_int *) nmesh,
						   _fs1,
						   &_lmesh,
						   (med_int *) mnumdt,
						   (med_int *) mnumit);

  *lmesh = (med_int) _lmesh;
  _MEDc2fString(_fs1,mname,MED_NAME_SIZE);
  
  _MEDcstringFree(_fn1);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MFDFONP(med_idt *fid, char* fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
		  med_int *it, char *mname, unsigned int bidon2,
		  char *dpname, unsigned int bidon3, char *dlname, unsigned int bidon4)
#else
med_int 
nmfdfonp(med_idt *fid, char*fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
	 med_int *it, char *mname, 
	 char *dpname, char *dlname)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_NAME_SIZE+1]="";
  char _fs3[MED_NAME_SIZE+1]="";
  char _fs4[MED_NAME_SIZE+1]="";
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDfield23nProfile((const med_idt) *fid,
				      _fn1,
				      (med_int) *numdt,
				      (med_int) *numit,
				      _etype,
				      _gtype,
				      (med_int) *it,
				      _fs2,
				      _fs3,
				      _fs4);

  _MEDc2fString(_fs2,mname,MED_NAME_SIZE);
  _MEDc2fString(_fs3,dpname,MED_NAME_SIZE);
  _MEDc2fString(_fs4,dlname,MED_NAME_SIZE);

  _MEDcstringFree(_fn1);

  return(_ret);
}



#ifdef PPRO_NT
med_int 
MFDFONV(med_idt *fid, char*fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
		  char* mname,  unsigned int bidon2, med_int *mnamelen,
		  med_int *pit, med_int *stm, char *pname, unsigned int bidon3, 
		  med_int *psize, char *lname, unsigned int bidon4, med_int *nip)
#else
med_int 
nmfdfonv(med_idt *fid, char*fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, med_int *etype, med_int *gtype,
	 char* mname, med_int *mnamelen,
	 med_int *pit, med_int *stm, char *pname, 
	 med_int *psize, char *lname, med_int *nip)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  char _fs2[MED_NAME_SIZE+1]="";
  char _fs3[MED_NAME_SIZE+1]="";
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_storage_mode _stm = (med_storage_mode) *stm;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2= _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDfield23nValueWithProfile((med_idt) *fid,
					       _fn1,
					       (med_int) *numdt,
					       (med_int) *numit,
					       _etype,
					       _gtype,
					       _fn2,
					       (med_int) *pit,
					       _stm,
					       _fs2,
					       (med_int *) psize,
					       _fs3,
					       (med_int *) nip);

  _MEDc2fString(_fs2,pname,MED_NAME_SIZE);
  _MEDc2fString(_fs3,lname,MED_NAME_SIZE);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret);
}



#ifdef PPRO_NT
med_int
MFDFORR(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit,
		  med_int *etype, med_int *gtype,
		  char *mname, unsigned int bidon2, med_int *mnamelen,
		  med_int *stm, char *pname, unsigned int bidon3, med_int *pnamelen,  
		  med_int *swm,med_int *cs, med_float *val)
#else
med_int
nmfdforr(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *etype, med_int *gtype, 
	 char *mname, med_int *mnamelen,
	 med_int *stm, char *pname, med_int *pnamelen,   
	 med_int *swm,med_int *cs, med_float *val)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2,*_fn3;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_storage_mode _stm = (med_storage_mode) *stm;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn2)
    return(-1);

  _fn3 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn3)
    return(-1);

  _ret = (med_int)  MEDfield23ValueWithProfileRd((med_idt) *fid, 
						 _fn1, 
						 (med_int) *numdt,
						 (med_int) *numit, 
						 _etype, 
						 _gtype,
						 _fn2,
						 _stm,
						 _fn3,
						 _swm,
						 (med_int) *cs,
						 (unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);
  
  return (_ret);
}


#ifdef PPRO_NT
med_int
MFDFOIR(med_idt *fid, char *fname, unsigned int bidon1, med_int *fnamelen,
		  med_int *numdt, med_int *numit,
		  med_int *etype, med_int *gtype, 
		  char *mname, unsigned int bidon2, med_int *mnamelen,
		  med_int *stm, char *pname, unsigned int bidon3, med_int *pnamelen,  
		  med_int *swm, med_int *cs, med_int *val)
#else
med_int
nmfdfoir(med_idt *fid, char *fname, med_int *fnamelen,
	 med_int *numdt, med_int *numit, 
	 med_int *etype, med_int *gtype,
	 char *mname, med_int *mnamelen, 
	 med_int *stm, char *pname, med_int *pnamelen,  
	 med_int *swm,med_int *cs, med_int *val)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2,*_fn3;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;
  med_entity_type _etype = (med_entity_type) *etype;
  med_storage_mode _stm = (med_storage_mode) *stm;

  _fn1 = _MED2cstring((char *) fname, (int) *fnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn2)
    return(-1);

  _fn3 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn3)
    return(-1);

  _ret = (med_int) MEDfield23ValueWithProfileRd((med_idt) *fid, 
						_fn1, 
						(med_int) *numdt,
						(med_int) *numit, 
						_etype, 
						_gtype,
						_fn2,
						_stm,
						_fn3,
						_swm,
						(med_int) *cs,
						(unsigned char *) val);

  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);
  
  return (_ret);
}
