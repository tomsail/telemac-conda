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
- MEDlocalizationWr 
- MEDlocalizationInfo  
- MEDlocalizationRd   
- MEDnLocalization
- MEDlocalizationInfoByName  
*/

#define nmlcflow F77_FUNC(mlcflow,MLCFLOW)
#define nmlcflor F77_FUNC(mlcflor,MLCFLOR)
#define nmlcfnlc F77_FUNC(mlcfnlc,MLCFNLC)
#define nmlcflci F77_FUNC(mlcflci,MLCFLCI)
#define nmlcflni F77_FUNC(mlcflni,MLCFLNI)

#ifdef PPRO_NT
med_int
MLCFLOW(med_idt *fid, char *lname, unsigned int bidon1, med_int *lnamelen,
		  med_int *gtype, med_int *sdim, med_float *ecoo,
		  med_int *swm, med_int *nip, med_float *icoo,
		  med_float *wght, char * giname, unsigned int bidon2, med_int *ginamelen,
		  char * isname,unsigned int bidon3, med_int *isnamelen)
#else
med_int
nmlcflow(med_idt *fid, char *lname,med_int *lnamelen,
	 med_int *gtype, med_int *sdim, med_float *ecoo,
	 med_int *swm, med_int *nip, med_float *icoo,
	 med_float *wght,char * giname, med_int *ginamelen,
	 char * isname, med_int *isnamelen)
#endif
{
  med_err _ret=0;
  char *_fn1,*_fn2,*_fn3;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_geometry_type _gtype = (med_geometry_type) *gtype;

  _fn1 = _MED2cstring((char *) lname, (int) *lnamelen);
  if (!_fn1)  return(-1);
  _fn2 = _MED2cstring((char *) giname, (int) *ginamelen);
  if (!_fn2)  return(-1);
  _fn3 = _MED2cstring((char *) isname, (int) *isnamelen);
  if (!_fn3)  return(-1);

  _ret = (med_int) MEDlocalizationWr((const med_idt) *fid, 
				     _fn1, 
				     _gtype,
				     (med_int) *sdim,
				     (med_float *) ecoo,
				     _swm,
				     (med_int) *nip,
				     (med_float *) icoo,
				     (med_float *) wght,
				     _fn2,
				     _fn3);
  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);
  
  return (_ret);
}



#ifdef PPRO_NT
med_int 
MLCFNLC(med_idt *fid)
#else
med_int 
nmlcfnlc(med_idt *fid)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDnLocalization((const med_idt) *fid); 

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MLCFLCI(med_idt *fid, med_int *it, char*lname, unsigned int bidon1, 
		  med_int *gtype, med_int *sdim, med_int nip, 
		  char * giname, unsigned int bidon2,
		  char * isname, unsigned int bidon3,
		  med_int *nsmc, med_int *sgtype)
#else
med_int
nmlcflci(med_idt *fid, med_int *it, char *lname, med_int *gtype, med_int *sdim, 
	 med_int *nip,char * giname, char * isname,
	 med_int *nsmc, med_int *sgtype)

#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1]="";
  char _fs2[MED_NAME_SIZE+1]="";
  char _fs3[MED_NAME_SIZE+1]="";
  med_geometry_type _gtype,_sgtype;

  _ret = (med_int) MEDlocalizationInfo((med_idt) *fid,
				       (med_int) *it,
				       _fs1,
				       &_gtype,
				       (med_int *) sdim,
				       (med_int *) nip,
				       _fs2,
				       _fs3,
				       (med_int *) nsmc, 
				       &_sgtype);

  _MEDc2fString(_fs1,lname,MED_NAME_SIZE);
  _MEDc2fString(_fs2,giname,MED_NAME_SIZE);
  _MEDc2fString(_fs3,isname,MED_NAME_SIZE);

  *gtype = (med_int) _gtype;
  *sgtype = (med_int) _sgtype;

  return _ret;
}

#ifdef PPRO_NT
med_int
MLCFLNI(med_idt *fid, char*lname, unsigned int bidon1, med_int *lnamelen,
		  med_int *gtype, med_int *sdim, med_int nip,
		  char * giname, unsigned int bidon2,
		  char * isname, unsigned int bidon3,
		  med_int *nsmc, med_int *sgtype)
#else
med_int
nmlcflni(med_idt *fid, char *lname,  med_int *lnamelen, med_int *gtype, med_int *sdim,
	 med_int *nip,char * giname, char * isname,
	 med_int *nsmc, med_int *sgtype)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_NAME_SIZE+1]="";
  char _fs3[MED_NAME_SIZE+1]="";
  med_geometry_type _gtype,_sgtype;

  _fn1 = _MED2cstring((char *) lname, (int) *lnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDlocalizationInfoByName((med_idt) *fid,
					     _fn1,
					     &_gtype,
					     (med_int *) sdim,
					     (med_int *) nip,
					     _fs2,
					     _fs3,
					     (med_int *) nsmc,	
					     &_sgtype);

  _MEDcstringFree(_fn1);
  _MEDc2fString(_fs2,giname,MED_NAME_SIZE);
  _MEDc2fString(_fs3,isname,MED_NAME_SIZE);

  *gtype = (med_int) _gtype;
  *sgtype = (med_int) _sgtype;

  return _ret;
}



#ifdef PPRO_NT
med_int
MLCFLOR(med_idt *fid, char *lname, unsigned int bidon1, med_int *lnamelen,
		  med_int *swm, med_float *ecoo, med_float *icoo, med_float *wght)
#else
med_int
nmlcflor(med_idt *fid, char *lname, med_int *lnamelen,
	 med_int *swm, med_float *ecoo, med_float *icoo, med_float *wght)
#endif
{
  med_err _ret=0;
  char *_fn1;
  med_switch_mode _swm = (med_switch_mode) *swm;

  _fn1 = _MED2cstring((char *) lname, (int) *lnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDlocalizationRd((const med_idt) *fid, 
				     _fn1, 
				     _swm,
				     (med_float *) ecoo,
				     (med_float *) icoo,
				     (med_float *) wght);
  
  _MEDcstringFree(_fn1);
  
  return (_ret);
}

