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
- MEDmeshCr  
- MEDnMesh     
- MEDmeshnAxis 
- MEDmeshnAxisByName  
- MEDmeshInfo   
- MEDmeshInfoByName
- MEDmeshUniversalNameWr
- MEDmeshAttributeRd    
- MEDmeshAttributeWr 
- MEDmeshUniversalNameRd
- MEDmeshGridTypeWr  
- MEDmeshGridTypeRd  
- MEDmeshGridStructWr  
- MEDmeshGridStructRd  
- MEDmeshNodeCoordinateWr
- MEDmeshNodeCoordinateRd   
- MEDmeshNodeCoordinateWithProfileWr
- MEDmeshNodeCoordinateWithProfileRd  
- MEDmeshGridIndexCoordinateWr  
- MEDmeshGridIndexCoordinateRd  
- MEDmeshEntityFamilyNumberWr     
- MEDmeshEntityNumberWr   
- MEDmeshEntityNameWr 
- MEDmeshnEntity
- MEDmeshEntityNameRd                                                                          
- MEDmeshEntityNumberRd     
- MEDmeshEntityFamilyNumberRd                               
- MEDmeshElementConnectivityRd                          
- MEDmeshElementConnectivityWr     
- MEDmeshElementConnectivityWithProfileRd
- MEDmeshnEntityWithProfile 
- MEDmeshElementConnectivityWithProfileWr  
- MEDmeshNodeWr   
- MEDmeshNodeRd   
- MEDmeshElementWr
                                                                                
- MEDmeshElementRd   
- MEDmeshNodeCoordinateAdvancedWr   
- MEDmeshNodeCoordinateAdvancedRd    
- MEDmeshElementConnectivityAdvancedRd     
- MEDmeshElementConnectivityAdvancedWr 
- MEDmeshPolygonWr   
- MEDmeshPolygonRd  
- MEDmeshPolyhedronRd  
- MEDmeshPolyhedronWr
- MEDmeshGlobalNumberWr    
- MEDmeshGlobalNumberRd   
- MEDmeshComputationStepCr    
- MEDmeshComputationStepInfo             
- MEDmeshComputationStepDtRd      
- MEDmeshSortingTypeRd                   
- MEDmeshStructElementVarAttRd
- MEDmeshStructElementVarAttWr  
- MEDmeshEntityInfo
- MEDmeshNodeCoordinateTrsfRd        
- MEDmeshNodeCoordinateTrsfWr
- MEDmeshEntityAttributeAdvancedWr
- MEDmeshEntityAttributeAdvancedRd
*/

#define nmmhfcre F77_FUNC(mmhfcre,MMHFCRE) 
#define nmmhfnmh F77_FUNC(mmhfnmh,MMHFNMH)
#define nmmhfnax F77_FUNC(mmhfnax,MMHFNAX) 
#define nmmhfnan F77_FUNC(mmhfnan,MMHFNAN)
#define nmmhfmhi F77_FUNC(mmhfmhi,MMHFMHI)
#define nmmhfmin F77_FUNC(mmhfmin,MMHFMIN)
#define nmmhfunw F77_FUNC(mmhfunw,MMHFUNW)
#define nmmhfunr F77_FUNC(mmhfunr,MMHFUNR)
#define nmmhfatw F77_FUNC(mmhfatw,MMHFATW)
#define nmmhfatr F77_FUNC(mmhfatr,MMHFATR)
#define nmmhfgtw F77_FUNC(mmhfgtw,MMHFGTW)
#define nmmhfgtr F77_FUNC(mmhfgtr,MMHFGTR)
#define nmmhfgsw F77_FUNC(mmhfgsw,MMHFGSW)
#define nmmhfgsr F77_FUNC(mmhfgsr,MMHFGSR)
#define nmmhfcow F77_FUNC(mmhfcow,MMHFCOW)
#define nmmhfcor F77_FUNC(mmhfcor,MMHFCOR)
#define nmmhfcpw F77_FUNC(mmhfcpw,MMHFCPW)
#define nmmhfcpr F77_FUNC(mmhfcpr,MMHFCPR)
#define nmmhfgcw F77_FUNC(mmhfgcw,MMHFGCW)
#define nmmhfgcr F77_FUNC(mmhfgcr,MMHFGCR)
#define nmmhfenw F77_FUNC(mmhfenw,MMHFENW)
#define nmmhfenr F77_FUNC(mmhfenr,MMHFENR)
#define nmmhffnw F77_FUNC(mmhffnw,MMHFFNW)
#define nmmhffnr F77_FUNC(mmhffnr,MMHFFNR)
#define nmmhfeaw F77_FUNC(mmhfeaw,MMHFEAW)
#define nmmhfear F77_FUNC(mmhfear,MMHFEAR)
#define nmmhfnme F77_FUNC(mmhfnme,MMHFNME)
#define nmmhfcyw F77_FUNC(mmhfcyw,MMHFCYW)
#define nmmhfcyr F77_FUNC(mmhfcyr,MMHFCYR)
#define nmmhfypw F77_FUNC(mmhfypw,MMHFYPW)
#define nmmhfypr F77_FUNC(mmhfypr,MMHFYPR)
#define nmmhfnep F77_FUNC(mmhfnep,MMHFNEP)
#define nmmhfnow F77_FUNC(mmhfnow,MMHFNOW)
#define nmmhfnor F77_FUNC(mmhfnor,MMHFNOR)
#define nmmhfelw F77_FUNC(mmhfelw,MMHFELW)
#define nmmhfelr F77_FUNC(mmhfelr,MMHFELR)
#define nmmhfcaw F77_FUNC(mmhfcaw,MMHFCAW)
#define nmmhfcar F77_FUNC(mmhfcar,MMHFCAR)
#define nmmhfyaw F77_FUNC(mmhfyaw,MMHFYAW)
#define nmmhfyar F77_FUNC(mmhfyar,MMHFYAR)
#define nmmhfpgw F77_FUNC(mmhfpgw,MMHFPGW)
#define nmmhfpgr F77_FUNC(mmhfpgr,MMHFPGR)
#define nmmhfphw F77_FUNC(mmhfphw,MMHFPHW)
#define nmmhfphr F77_FUNC(mmhfphr,MMHFPHR)
#define nmmhfgnw F77_FUNC(mmhfgnw,MMHFGNW)
#define nmmhfgnr F77_FUNC(mmhfgnr,MMHFGNR)
#define nmmhfcsc F77_FUNC(mmhfcsc,MMHFCSC)
#define nmmhfcsi F77_FUNC(mmhfcsi,MMHFCSI)
#define nmmhfcsr F77_FUNC(mmhfcsr,MMHFCSR)
#define nmmhfstr F77_FUNC(mmhfstr,MMHFSTR)
#define nmmhfraw F77_FUNC(mmhfraw,MMHFRAW)
#define nmmhfiaw F77_FUNC(mmhfiaw,MMHFIAW)
#define nmmhfsaw F77_FUNC(mmhfsaw,MMHFSAW)
#define nmmhfrar F77_FUNC(mmhfrar,MMHFRAR)
#define nmmhfiar F77_FUNC(mmhfiar,MMHFIAR)
#define nmmhfsar F77_FUNC(mmhfsar,MMHFSAR)
#define nmmhfeni F77_FUNC(mmhfeni,MMHFENI)
#define nmmhftfw F77_FUNC(mmhftfw,MMHFTFW)
#define nmmhftfr F77_FUNC(mmhftfr,MMHFTFR)
#define nmmhfaaw F77_FUNC(mmhfaaw,MMHFAAW)
#define nmmhfaar F77_FUNC(mmhfaar,MMHFAAR)
#define nmmhfasw F77_FUNC(mmhfasw,MMHFASW)
#define nmmhfasr F77_FUNC(mmhfasr,MMHFASR)


#ifdef PPRO_NT
med_int 
MMHFCRE(med_idt *fid, char *mname, unsigned int bidon, 
                  med_int *mnamelen, med_int * sdim, med_int * mdim, med_int *mtype,
		  char *desc, unsigned int bidon2, med_int *desclen,
		  char *dtunit, unsigned int bidon3, med_int *dtunitlen,
		  med_int *stype, med_int *atype, char *aname, unsigned int bidon4,
		  med_int *anamelen, char *aunit, unsigned int bidon5, med_int* aunitlen)
#else
med_int 
nmmhfcre(med_idt *fid, char *mname, med_int *mnamelen, med_int * sdim,
	 med_int *mdim, med_int *mtype, char *desc, med_int *desclen,
	 char *dtunit, med_int *dtunitlen,
	 med_int *stype, med_int *atype, char *aname,
	 med_int *anamelen, char *aunit, med_int* aunitlen)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2, *_fn3, *_fn4, *_fn5;
  med_mesh_type _meshtype = (med_mesh_type) *mtype;
  med_sorting_type _sortingtype = (med_sorting_type) *stype;
  med_axis_type _axistype = (med_axis_type) *atype;


  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
	return(-1);
  _fn2 = _MED2cstring((char *) desc, (int) *desclen);
  if (!_fn2)
	return(-1);
  _fn3 = _MED2cstring((char *) dtunit, (int) *dtunitlen);
  if (!_fn3)
	return(-1);
  _fn4 = _MED1cstring((char *) aname, (int) *anamelen, (int) *sdim*MED_SNAME_SIZE);
  if (!_fn4)
    return(-1);
  _fn5 = _MED1cstring((char *) aunit, (int) *aunitlen, (int) *sdim*MED_SNAME_SIZE);
  if (!_fn5)
	return(-1);

  _ret = (med_int) MEDmeshCr((const med_idt) *fid, _fn1, (const med_int) *sdim, (const med_int) *mdim,
			     _meshtype, _fn2, _fn3, _sortingtype, _axistype,
			     _fn4, _fn5);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);
  _MEDcstringFree(_fn4);
  _MEDcstringFree(_fn5);

  return _ret;
}



#ifdef PPRO_NT
med_int
MMHFNMH(med_idt *fid)
#else
med_int
nmmhfnmh(med_idt *fid)
#endif
{
  med_int _ret;

  _ret = (med_int) MEDnMesh((const med_idt) *fid);

  return(_ret);
}


#ifdef PPRO_NT
med_int
MMHFNAN(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen)
#else
med_int
nmmhfnan(med_idt *fid, char *mname, med_int *mnamelen)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshnAxisByName((const med_idt) *fid, (const char*) _fn1);

  _MEDcstringFree(_fn1);

  return(_ret);
}


#ifdef PPRO_NT
med_int 
MMHFNAX(med_idt *fid, med_int *it)
#else
med_int 
nmmhfnax(med_idt *fid, med_int *it)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDmeshnAxis((const med_idt) *fid, (const med_int) *it); 

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MMHFMHI(med_idt *fid, med_int *it, char*mname, unsigned int bidon, 
                  med_int * sdim, med_int * mdim, med_int *mtype,
		  char *desc, unsigned int bidon2,
		  char *dtunit, unsigned int bidon3, 
		  med_int *stype, med_int *nstep, med_int *atype, 
		  char *aname, unsigned int bidon4, 
		  char *aunit, unsigned int bidon5)
#else
med_int 
nmmhfmhi(med_idt *fid, med_int *it, char *mname, med_int * sdim,
	 med_int *mdim, med_int *mtype, char *desc,
	 char *dtunit, med_int *stype, med_int *nstep,
	 med_int *atype, char *aname, char *aunit)
#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1]="";
  char _fs2[MED_COMMENT_SIZE+1]="";
  char _fs3[MED_SNAME_SIZE+1]="";
  char *_fs4, *_fs5;
  med_mesh_type _mtype;
  med_sorting_type _stype;
  med_axis_type _atype;
  med_int _sdim;

  _sdim = MEDmeshnAxis((const med_idt)*fid, (const med_int) *it);
  if (_sdim < 0)
    return (-1);

  _fs4 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_sdim)+1);*_fs4='\0';
  _fs5 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_sdim)+1);*_fs5='\0';

  if (!(_fs4&&_fs5))
    return (-1);

  _ret = (med_int) MEDmeshInfo((const med_idt)*fid, (const med_int) *it, (char *) _fs1,
			       (med_int *) sdim, (med_int *) mdim, &_mtype, (char *) _fs2,
			       (char *) _fs3, &_stype, (med_int *) nstep, &_atype,
			       (char *) _fs4, (char *) _fs5);


  *mtype = (med_int) _mtype;
  *stype = (med_int) _stype;
  *atype = (med_int) _atype;

  _MEDc2fString(_fs1,mname,MED_NAME_SIZE);
  _MEDc2fString(_fs2,desc,MED_COMMENT_SIZE);
  _MEDc2fString(_fs3,dtunit,MED_SNAME_SIZE);
  _MEDc2fString(_fs4,aname,MED_SNAME_SIZE*(*sdim));
  _MEDc2fString(_fs5,aunit,MED_SNAME_SIZE*(*sdim));

  free(_fs4);
  free(_fs5);

  return _ret;
}


#ifdef PPRO_NT
med_int
MMHFMIN(med_idt *fid, char*mname, unsigned int bidon, med_int *mnamelen,
        med_int * sdim, med_int * mdim, med_int *mtype,
		char *desc, unsigned int bidon2,
		char *dtunit, unsigned int bidon3,
		med_int *stype, med_int *nstep, med_int *atype,
		char *aname, unsigned int bidon4,
		char *aunit, unsigned int bidon5)
#else
med_int
nmmhfmin(med_idt *fid, char *mname, med_int *mnamelen, med_int * sdim,
	 med_int *mdim, med_int *mtype, char *desc,
	 char *dtunit, med_int *stype, med_int *nstep,
	 med_int *atype, char *aname, char *aunit)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_COMMENT_SIZE+1]="";
  char _fs3[MED_SNAME_SIZE+1]="";
  char *_fs4, *_fs5;
  med_mesh_type _mtype;
  med_sorting_type _stype;
  med_axis_type _atype;
  med_int _sdim;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _sdim = MEDmeshnAxisByName((const med_idt)*fid, _fn1);
  if (_sdim < 0)
    return (-1);

  _fs4 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_sdim)+1);*_fs4='\0';
  _fs5 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*(_sdim)+1);*_fs5='\0';

  if (!(_fs4&&_fs5))
    return (-1);

  _ret = (med_int) MEDmeshInfoByName((const med_idt)*fid, (char *) _fn1,
				     (med_int *) sdim, (med_int *) mdim, &_mtype, (char *) _fs2,
				     (char *) _fs3, &_stype, (med_int *) nstep, &_atype,
				     (char *) _fs4, (char *) _fs5);


  *mtype = (med_int) _mtype;
  *stype = (med_int) _stype;
  *atype = (med_int) _atype;

  _MEDc2fString(_fs2,desc,MED_COMMENT_SIZE);
  _MEDc2fString(_fs3,dtunit,MED_SNAME_SIZE);
  _MEDc2fString(_fs4,aname,MED_SNAME_SIZE*(*sdim));
  _MEDc2fString(_fs5,aunit,MED_SNAME_SIZE*(*sdim));

  _MEDcstringFree(_fn1);
  free(_fs4);
  free(_fs5);

  return _ret;
}



#ifdef PPRO_NT
med_int
MMHFUNW(med_idt *fid, char *uname, unsigned int bidon, med_int *unamelen)
#else
med_int
nmmhfunw(med_idt *fid, char *uname, med_int *unamelen)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) uname, (int) *unamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshUniversalNameWr((const med_idt) *fid, (const char*) _fn1); 

  _MEDcstringFree(_fn1);

  return(_ret);
}



#ifdef PPRO_NT
med_int
MMHFUNR(med_idt *fid, char*mname, unsigned int bidon, med_int *mnamelen,
		  char *uname, unsigned int bidon1)
#else
med_int
nmmhfunr(med_idt *fid, char*mname, med_int *mnamelen,
	 char *uname)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_LNAME_SIZE+1]="";

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshUniversalNameRd((med_idt) *fid, (char*) _fn1,
					  (char*) _fs2);

  _MEDc2fString(_fs2,uname,MED_LNAME_SIZE);

  _MEDcstringFree(_fn1);

  return(_ret);
}


#ifdef PPRO_NT
med_int
MMHFATW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *nin, med_int *nvn, med_int *nnc)
#else
med_int
nmmhfatw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *nin, med_int *nvn, med_int *nnc)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshAttributeWr((const med_idt) *fid, (const char*) _fn1, (const med_int) *nin,
				      (const med_int) *nvn, (const med_int) *nnc); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}




#ifdef PPRO_NT
med_int 
MMHFATR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *nin, med_int *nvn, med_int *nnc)
#else
med_int 
nmmhfatr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *nin, med_int *nvn, med_int *nnc)
#endif
{
  med_int _ret; 
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshAttributeRd((const med_idt) *fid, (const char*) _fn1, (med_int *) nin,
				      (med_int *) nvn, (med_int *) nnc); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}




#ifdef PPRO_NT
med_int 
MMHFGTW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *gtype)
#else
med_int 
nmmhfgtw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *gtype)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_grid_type _gtype = (med_grid_type) *gtype;


  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshGridTypeWr((const med_idt) *fid, (const char*) _fn1, _gtype); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}




#ifdef PPRO_NT
med_int 
MMHFGTR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *gtype)
#else
med_int 
nmmhfgtr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *gtype)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_grid_type _gtype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshGridTypeRd((const med_idt) *fid, (const char*) _fn1, &_gtype);

  *gtype = (med_int) _gtype;

  _MEDcstringFree(_fn1);

  return(_ret);
}


#ifdef PPRO_NT
med_int 
MMHFGSW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float *dt, med_int *gstruct)
#else
med_int 
nmmhfgsw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float *dt, med_int *gstruct)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshGridStructWr((const med_idt) *fid, (const char*) _fn1, (const med_int) *numdt,
				       (const med_int) *numit, (med_float) *dt, (const med_int *) gstruct); 

  _MEDcstringFree(_fn1);

  return(_ret);
}




#ifdef PPRO_NT
med_int 
MMHFGSR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *gstruct)
#else
med_int 
nmmhfgsr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *gstruct)
#endif
{
  med_int _ret; 
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshGridStructRd((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
				       (med_int) *numit, (med_int *) gstruct); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MMHFCOW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float *dt, med_int *swm,
		  med_int *n, med_float *coo)
#else
med_int 
nmmhfcow(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,  
	 med_int *swm, med_int *n, med_float *coo)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_switch_mode _swm = (med_switch_mode) *swm;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshNodeCoordinateWr((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
					   (med_int) *numit, (med_float) *dt, _swm, (med_int) *n,
					   (med_float *) coo); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MMHFCOR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *swm,
		  med_float *coo)
#else
med_int 
nmmhfcor(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *swm, med_float *coo)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_switch_mode _swm = (med_switch_mode) *swm;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshNodeCoordinateRd((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
					   (med_int) *numit, _swm, (med_float *) coo); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MMHFCPW(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float *dt, 
		  med_int *smode, char *pname, unsigned int bidon2, med_int *pnamelen,
		  med_int *swm, med_int *dim, med_int *n, med_float *coo)
#else
med_int 
nmmhfcpw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,  
	 med_int *smode, char *pname, med_int *pnamelen,
	 med_int *swm, med_int *dim, med_int *n, med_float *coo)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_storage_mode _smode = (med_storage_mode) *smode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  _fn2 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn2)
    return(-1);
  
  _ret = (med_int) MEDmeshNodeCoordinateWithProfileWr((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
						      (med_int) *numit, (med_float) *dt,
						      _smode, _fn2,
						      _swm, (med_int) *dim, (med_int) *n, (med_float *) coo); 

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MMHFCPR(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit,  
		  med_int *smode, char *pname, unsigned int bidon2, med_int *pnamelen,
		  med_int *swm, med_int *dim, med_float *coo)
#else
med_int 
nmmhfcpr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *smode, char *pname, med_int *pnamelen,
	 med_int *swm, med_int *dim, med_float *coo)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_storage_mode _smode = (med_storage_mode) *smode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  _fn2 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn2)
    return(-1);
  
  _ret = (med_int) MEDmeshNodeCoordinateWithProfileRd((med_idt) *fid, (char*) _fn1, (med_int) *numdt, (med_int) *numit,
						      _smode, _fn2,
						      _swm, (med_int) *dim, (med_float *) coo); 

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MMHFGCW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float *dt, 
		  med_int *axis, med_int *size, med_float *index)
#else
med_int 
nmmhfgcw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,  
	 med_int *axis, med_int *size, med_float *index)
#endif
{
  med_int _ret; 
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshGridIndexCoordinateWr((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
						(med_int) *numit, (med_float) *dt, 
						(med_int) *axis, (med_int) *size,(med_float *) index);

    _MEDcstringFree(_fn1);

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MMHFGCR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, 
		  med_int *axis, med_float *index)
#else
med_int 
nmmhfgcr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *axis, med_float *index)
#endif
{
  med_int _ret; 
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshGridIndexCoordinateRd((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
						(med_int) *numit,
						(med_int) *axis, (med_float *) index);

  _MEDcstringFree(_fn1);

  return(_ret);
}


#ifdef PPRO_NT
med_int 
MMHFENW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *n, med_int *num)
#else
med_int 
nmmhfenw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, 
	 med_int *geotype, med_int *n, med_int *num)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int)  MEDmeshEntityNumberWr((med_idt) *fid, 
					  (char*) _fn1, 
					  (med_int) *numdt,
					  (med_int) *numit, 
					  _etype, 
					  _gtype, 
					  (med_int) *n,
					  (med_int *) num);

  _MEDcstringFree(_fn1);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFENR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *num)
#else
med_int 
nmmhfenr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, 
	 med_int *geotype, med_int *num)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int)  MEDmeshEntityNumberRd((med_idt) *fid, 
					  (char*) _fn1, 
					  (med_int) *numdt,
					  (med_int) *numit, 
					  _etype, 
					  _gtype,
					  (med_int *) num);

  _MEDcstringFree(_fn1);

  return (_ret);
}



#ifdef PPRO_NT
med_int 
MMHFFNW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *n, med_int *num)
#else
med_int 
nmmhffnw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, 
	 med_int *geotype, med_int *n, med_int *num)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshEntityFamilyNumberWr((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
					       (med_int) *numit, _etype, _gtype, (med_int) *n,
					       (med_int *) num);

  _MEDcstringFree(_fn1);

  return (_ret);
}



#ifdef PPRO_NT
med_int 
MMHFFNR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *num)
#else
med_int 
nmmhffnr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, 
	 med_int *geotype, med_int *num)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshEntityFamilyNumberRd((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
					       (med_int) *numit, _etype, _gtype,
					       (med_int *) num);

  _MEDcstringFree(_fn1);

  return (_ret);
}



#ifdef PPRO_NT
med_int 
MMHFEAW(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *n, char *ename, unsigned int bidon2, med_int *enamelen)
#else
med_int 
nmmhfeaw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
	 med_int *n, char *ename, med_int *enamelen)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  /* Ce traitement est-il utile ?
     A moins d'accès en subscript dont l'union ne constituerait pas le tableau Fortran complet, le fortran remplit le tableau de blancs t[1:?] */
  /* _fn2 = _MED1cstring(ename, (int) *enamelen,(int) *n*MED_SNAME_SIZE); */
  /* if (!_fn2) */
  /*   return(-1); */

  _ret = (med_int) MEDmeshEntityNameWr((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
				       (med_int) *numit, _etype, _gtype, (med_int) *n,
				       ename);
				       /* _fn2); */
  _MEDcstringFree(_fn1);
  /* _MEDcstringFree(_fn2); */

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFEAR(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  char *ename, unsigned int bidon2)
#else
med_int 
nmmhfear(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
	 char *ename)
#endif
{
  med_int _ret; 
  char *_fn1, *_fs2;
  med_int _n;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_bool _changement;
  med_bool _transformation;
  med_connectivity_mode _cmode = MED_NO_CMODE;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  if ((_gtype == MED_POLYGON) ||
      (_gtype == MED_POLYHEDRON))
    _cmode = MED_NODAL;

  _n = MEDmeshnEntity((med_idt) *fid,_fn1,(med_int) *numdt,(med_int) *numit,
		     _etype, _gtype,MED_NAME,_cmode,&_changement,
		     &_transformation);

  if (_n < 0) return(-1);

  _fs2 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*_n+1);*_fs2='\0';
  if (!_fs2)  return (-1);

  _ret = (med_int) MEDmeshEntityNameRd((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
				       (med_int) *numit, _etype, _gtype,
				       _fs2);

  _MEDc2fString(_fs2,ename,MED_SNAME_SIZE*_n);

  _MEDcstringFree(_fn1);
  free(_fs2);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFNME(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *datatype, med_int *cmode, med_int *chgt, med_int *tsf) 
#else
med_int 
nmmhfnme(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
	 med_int *datatype, med_int *cmode, med_int *chgt, med_int *tsf)
#endif
{
  med_int ret;
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_data_type _datatype = (med_data_type) *datatype; 
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;
  med_bool _changement;
  med_bool _transformation;

  _fn1 = _MED2cstring(mname, (int) * mnamelen);

  if (!_fn1 )
    return(-1);

  ret = (med_int) MEDmeshnEntity((med_idt) *fid, (char*) _fn1, (med_int) *numdt,
				 (med_int) *numit, _etype, _gtype, _datatype,_cmode,
				 &_changement,&_transformation);

  *chgt = (med_int) _changement;
  *tsf = (med_int) _transformation;

  _MEDcstringFree(_fn1);

  return(ret); 
}



#ifdef PPRO_NT
med_int 
MMHFCYW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float* dt,
		  med_int *entype, med_int *geotype, med_int *cmode, 
		  med_int *swm, med_int *n, med_int *connectivity)
#else
med_int 
nmmhfcyw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float* dt,
	 med_int *entype, med_int *geotype, med_int *cmode, 
	 med_int *swm, med_int *n, med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;
  med_switch_mode _swm = (med_switch_mode) *swm;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshElementConnectivityWr((med_idt) *fid, 
						(char*) _fn1, 
						(med_int) *numdt,
						(med_int) *numit, 
						(med_float) *dt,
						_etype, 
						_gtype,
						_cmode,
						_swm,
						(med_int) *n,
						(med_int *) connectivity);

  _MEDcstringFree(_fn1);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFCYR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, 
		  med_int *entype, med_int *geotype, med_int *cmode, 
		  med_int *swm, med_int *connectivity)
#else
med_int 
nmmhfcyr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, 
	 med_int *entype, med_int *geotype, med_int *cmode, 
	 med_int *swm, med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;
  med_switch_mode _swm = (med_switch_mode) *swm;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshElementConnectivityRd((med_idt) *fid, 
						(char*) _fn1, 
						(med_int) *numdt,
						(med_int) *numit, 
						_etype, 
						_gtype,
						_cmode,
						_swm,
						(med_int *) connectivity);

  _MEDcstringFree(_fn1);

  return (_ret);
}

#ifdef PPRO_NT
med_int 
MMHFYPW(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float* dt,
		  med_int *entype, med_int *geotype, med_int *cmode,
		  med_int *stmode, char *pname, unsigned int bidon2,med_int *pnamelen,
		  med_int *swm, med_int *dim, med_int *n, med_int *connectivity)
#else
med_int 
nmmhfypw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float* dt,
	 med_int *entype, med_int *geotype, med_int *cmode,
	 med_int *stmode,char *pname, med_int *pnamelen,
	 med_int *swm, med_int *dim, med_int *n, med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1,*_fn2;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_storage_mode _stmode = (med_storage_mode) *stmode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDmeshElementConnectivityWithProfileWr((med_idt) *fid, 
							   (char*) _fn1, 
							   (med_int) *numdt,
							   (med_int) *numit, 
							   (med_float) *dt,
							   _etype, 
							   _gtype,
							   _cmode,
							   _stmode,
							   (char *) _fn2,
							   _swm,
							   (med_int) *dim,
							   (med_int) *n,
							   (med_int *) connectivity);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFYPR(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, 
		  med_int *entype, med_int *geotype, med_int *cmode, 
		  med_int *stmode, char* pname, unsigned int bidon2, med_int *pnamelen,
		  med_int *swm, med_int*dim, med_int *n, med_int *connectivity)
#else
med_int 
nmmhfypr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, 
	 med_int *entype, med_int *geotype, med_int *cmode, 
	 med_int *stmode, char* pname, med_int *pnamelen,
	 med_int *swm, med_int*dim, med_int *n, med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_storage_mode _stmode = (med_storage_mode) *stmode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDmeshElementConnectivityWithProfileRd((med_idt) *fid, 
							   (char*) _fn1, 
							   (med_int) *numdt,
							   (med_int) *numit, 
							   _etype, 
							   _gtype,
							   _cmode,
							   _stmode,
							   _fn2,
							   _swm,
							   (med_int) *dim,
							   (med_int) *n,
							   (med_int *) connectivity);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return (_ret);
}



#ifdef PPRO_NT
med_int 
MMHFNEP(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *datatype, med_int *cmode, 
		  med_int *stmode, char* pname, unsigned int bidon1, med_int *psize,
		  med_int *chgt, med_int *tsf) 
#else
med_int 
nmmhfnep(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
	 med_int *datatype, med_int *cmode, 
	 med_int *stmode, char* pname, med_int *psize,
	 med_int *chgt, med_int *tsf)
#endif
{
  med_int ret;
  char *_fn1;
  char _fs2[MED_NAME_SIZE+1]="";
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_data_type _datatype = (med_data_type) *datatype; 
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;
  med_bool _changement;
  med_bool _transformation;
  med_storage_mode _stmode = (med_storage_mode) *stmode;

  _fn1 = _MED2cstring((char *) mname, (int) * mnamelen);
  if (!_fn1 ) return(-1);

  ret = (med_int) MEDmeshnEntityWithProfile((med_idt) *fid, 
					    (char*) _fn1, 
					    (med_int) *numdt,
					    (med_int) *numit, 
					    _etype, 
					    _gtype, 
					    _datatype,
					    _cmode,
					    _stmode,
					    _fs2,
					    (med_int *) psize,
					    &_changement,
					    &_transformation);

  *chgt = (med_int) _changement;
  *tsf = (med_int) _transformation;


  _MEDc2fString(_fs2,pname,MED_NAME_SIZE);
  _MEDcstringFree(_fn1);

  return(ret);
}



#ifdef PPRO_NT
med_int 
MMHFNOW(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float *dt, med_int *swm,
		  med_int *n, med_float *coo,
		  med_int *iname, char*nname, unsigned int bidon2, med_int *nnamelen,
		  med_int *inum, med_int *num, med_int *ifam, med_int *fam)
#else
med_int 
nmmhfnow(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float *dt, med_int *swm,
	 med_int *n, med_float *coo,
	 med_int *iname, char*nname, med_int *nnamelen,
	 med_int *inum, med_int *num, med_int *ifam, med_int *fam)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_bool _iname = (med_bool) *iname;
  med_bool _inum = (med_bool) *inum;
  med_bool _ifam = (med_bool) *ifam;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  /*TODO : cf nmmhfeaw*/
  _fn2 = _MED1cstring(nname, (int) *nnamelen,(int) *n*MED_SNAME_SIZE);
  if (!_fn2)
    return(-1);
  
  _ret = (med_int) MEDmeshNodeWr((med_idt) *fid, 
				 (char*) _fn1, 
				 (med_int) *numdt,
				 (med_int) *numit, 
				 (med_float) *dt, 
				 _swm, 
				 (med_int) *n,
				 (med_float *) coo,
				 _iname,
				 _fn2,
				 _inum,
				 (med_int*) num,
				 _ifam,
				 (med_int *) fam); 

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MMHFNOR(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *swm,med_float *coo,
		  med_int *iname, char*nname, unsigned int bidon,
		  med_int *inum, med_int *num, med_int *ifam, med_int *fam)
#else
med_int 
nmmhfnor(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *swm, med_float *coo,
	 med_int *iname, char*nname, med_int *inum, med_int *num, 
	 med_int *ifam, med_int *fam)
#endif
{
  med_int _ret,_n; 
  char *_fn1, *_fs2;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_bool _iname,_inum,_ifam,_changement,_transformation;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _n = MEDmeshnEntity((med_idt) *fid,_fn1,(med_int) *numdt,(med_int) *numit, 
		     MED_NODE,MED_NONE,MED_NAME,MED_NO_CMODE,&_changement,
		     &_transformation);
  if (_n < 0)
    return(-1);

  _fs2 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*_n+1);*_fs2='\0';
  if (!_fs2)
    return (-1);
  
  _ret = (med_int) MEDmeshNodeRd((med_idt) *fid, 
				 (char*) _fn1, 
				 (med_int) *numdt,
				 (med_int) *numit, 
				 _swm, 
				 (med_float *) coo,
				 &_iname,
				 _fs2,
				 &_inum,
				 (med_int*) num,
				 &_ifam,
				 (med_int *) fam); 

  _MEDc2fString(_fs2,nname,MED_SNAME_SIZE*_n);

  *iname = (med_int) _iname;
  *inum = (med_int) _inum;
  *ifam = (med_int) _ifam;

  _MEDcstringFree(_fn1);
  free(_fs2);

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MMHFELW(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float *dt,  med_int *entype, med_int *geotype,
		  med_int *cmode, med_int *swm, 
		  med_int *n, med_int *connectivity,
		  med_int *iname, char*nname, unsigned int bidon2, med_int *nnamelen,
		  med_int *inum, med_int *num, med_int *ifam, med_int *fam)
#else
med_int 
nmmhfelw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float *dt, med_int *entype, med_int *geotype,
	 med_int *cmode, med_int *swm, 
	 med_int *n, med_int *connectivity,
	 med_int *iname, char*nname, med_int *nnamelen,
	 med_int *inum, med_int *num, med_int *ifam, med_int *fam)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_bool _iname = (med_bool) *iname;
  med_bool _inum = (med_bool) *inum;
  med_bool _ifam = (med_bool) *ifam;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  /*TODO : nmmhfeaw */
  _fn2 = _MED1cstring(nname, (int) *nnamelen,(int) *n*MED_SNAME_SIZE);
  if (!_fn2)
    return(-1);
  
  _ret = (med_int) MEDmeshElementWr((med_idt) *fid, 
				    (char*) _fn1, 
				    (med_int) *numdt,
				    (med_int) *numit, 
				    (med_float) *dt, 
				    _etype,
				    _gtype,
				    _cmode,
				    _swm, 
				    (med_int) *n,
				    (med_int *) connectivity,
				    _iname,
				    _fn2,
				    _inum,
				    (med_int*) num,
				    _ifam,
				    (med_int *) fam); 

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MMHFELR(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *cmode, med_int *swm, 
		  med_int *connectivity,
		  med_int *iname, char*nname, unsigned int bidon,
		  med_int *inum, med_int *num, med_int *ifam, med_int *fam)
#else
med_int 
nmmhfelr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
	 med_int *cmode, med_int *swm, med_int *connectivity,
	 med_int *iname, char*nname, 
	 med_int *inum, med_int *num, med_int *ifam, med_int *fam)
#endif
{
  med_int _ret,_n;
  char *_fn1, *_fs2;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_bool _iname,_inum,_ifam, _changement,_transformation;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _n = MEDmeshnEntity((med_idt) *fid,_fn1,(med_int) *numdt,(med_int) *numit, 
		      _etype,_gtype,MED_NAME,_cmode,&_changement,
		      &_transformation);
  if (_n < 0)
    return(-1);

  _fs2 = (char *) malloc(sizeof(char)*MED_SNAME_SIZE*_n+1);*_fs2='\0';
  if (!_fs2)
    return (-1);

  _ret = (med_int) MEDmeshElementRd((med_idt) *fid, 
				    _fn1, 
				    (med_int) *numdt,
				    (med_int) *numit, 
				    _etype,
				    _gtype,
				    _cmode,
				    _swm, 
				    (med_int *) connectivity,
				    &_iname,
				    _fs2,
				    &_inum,
				    (med_int*) num,
				    &_ifam,
				    (med_int *) fam); 

  *iname = (med_int) _iname;
  *inum = (med_int) _inum;
  *ifam = (med_int) _ifam;

  _MEDc2fString(_fs2,nname,MED_SNAME_SIZE*_n);

  _MEDcstringFree(_fn1);
  free(_fs2);

  return(_ret); 
}




#ifdef PPRO_NT
med_int 
MMHFCAW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float *dt,
		  unsigned char **flt, med_float *coo)
#else
med_int 
nmmhfcaw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float *dt,  
	 unsigned char **flt,med_float *coo)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshNodeCoordinateAdvancedWr((med_idt) *fid, 
						   _fn1, 
						   (med_int) *numdt,
						   (med_int) *numit, 
						   (med_float) *dt,
						   _filter,
						   (med_float *) coo); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MMHFCAR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, unsigned char **flt,
		  med_float *coo)
#else
med_int 
nmmhfcar(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, unsigned char **flt,
	 med_float *coo)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshNodeCoordinateAdvancedRd((med_idt) *fid, 
						   _fn1, 
						   (med_int) *numdt,
						   (med_int) *numit, 
						   _filter, 
						   (med_float *) coo); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MMHFYAW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float* dt,
		  med_int *entype, med_int *geotype,  med_int *cmode, unsigned char **flt,
		  med_int *connectivity)
#else
med_int 
nmmhfyaw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float* dt,
	 med_int *entype, med_int *geotype, med_int *cmode,
	 unsigned char **flt, med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshElementConnectivityAdvancedWr((med_idt) *fid, 
							 _fn1, 
							(med_int) *numdt,
							(med_int) *numit, 
							(med_float) *dt,
							_etype, 
							_gtype,
							_cmode,
							_filter,
							(med_int *) connectivity);

  _MEDcstringFree(_fn1);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFYAR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, 
		  med_int *entype, med_int *geotype, med_int *cmode, 
		  unsigned char **flt, med_int *connectivity)
#else
med_int 
nmmhfyar(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, 
	 med_int *entype, med_int *geotype, med_int *cmode, 
	 unsigned char **flt, med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshElementConnectivityAdvancedRd((med_idt) *fid, 
							(char*) _fn1, 
							(med_int) *numdt,
							(med_int) *numit, 
							_etype, 
							_gtype,
							_cmode,
							_filter,
							(med_int *) connectivity);

  _MEDcstringFree(_fn1);

  return (_ret);
}



#ifdef PPRO_NT
med_int 
MMHFPGW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float* dt,
		  med_int *entype, med_int *cmode, 
		  med_int *indexsize, med_int *index, med_int *connectivity)
#else
med_int 
nmmhfpgw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float* dt,
	 med_int *entype, med_int *cmode, 
	 med_int *indexsize, med_int *index, med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshPolygonWr((med_idt) *fid, 
				    (char*)   _fn1, 
				    (med_int) *numdt,
				    (med_int) *numit, 
				    (med_float) *dt,
				    _etype, 
				    _cmode,
				    (med_int) *indexsize,
				    (med_int *) index,
				    (med_int *) connectivity);
  
  _MEDcstringFree(_fn1);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFPGR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, 
		  med_int *entype, med_int *cmode, 
		  med_int *index,
		  med_int *connectivity)
#else
med_int 
nmmhfpgr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, 
	 med_int *entype, med_int *cmode, 
	 med_int *index,
	 med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshPolygonRd((med_idt) *fid, 
				    (char*) _fn1, 
				    (med_int) *numdt,
				    (med_int) *numit, 
				    _etype, 
				    _cmode,
				    (med_int *) index,
				    (med_int *) connectivity);

  _MEDcstringFree(_fn1);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFPHW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float* dt,
		  med_int *entype, med_int *cmode, 
		  med_int *findexsize, med_int *findex,
 		  med_int *nindexsize, med_int *nindex,
		  med_int *connectivity)
#else
med_int 
nmmhfphw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float* dt,
	 med_int *entype, med_int *cmode, 
	 med_int *findexsize, med_int *findex,
	 med_int *nindexsize, med_int *nindex,
	 med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshPolyhedronWr((med_idt) *fid, 
				       (char*)   _fn1, 
				       (med_int) *numdt,
				       (med_int) *numit, 
				       (med_float) *dt,
				       _etype, 
				       _cmode,
				       (med_int) *findexsize,
				       (med_int *) findex,
				       (med_int) *nindexsize,
				       (med_int *) nindex,
				       (med_int *) connectivity);
  
  _MEDcstringFree(_fn1);

  return (_ret);
}



#ifdef PPRO_NT
med_int 
MMHFPHR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit,
		  med_int *entype, med_int *cmode, 
		  med_int *findex,
 		  med_int *nindex,
		  med_int *connectivity)
#else
med_int 
nmmhfphr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit,
	 med_int *entype, med_int *cmode, 
	 med_int *findex,
	 med_int *nindex,
	 med_int *connectivity)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_connectivity_mode _cmode = (med_connectivity_mode) *cmode;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshPolyhedronRd((med_idt) *fid, 
				       (char*)   _fn1, 
				       (med_int) *numdt,
				       (med_int) *numit, 
				       _etype, 
				       _cmode,
				       (med_int *) findex,
				       (med_int *) nindex,
				       (med_int *) connectivity);
  
  _MEDcstringFree(_fn1);

  return (_ret);
}

#ifdef PPRO_NT
med_int 
MMHFGNW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *n, med_int *num)
#else
med_int 
nmmhfgnw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, 
	 med_int *geotype, med_int *n, med_int *num)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int)  MEDmeshGlobalNumberWr((med_idt) *fid, 
					  (char*) _fn1, 
					  (med_int) *numdt,
					  (med_int) *numit, 
					  _etype, 
					  _gtype, 
					  (med_int) *n,
					  (med_int *) num);

  _MEDcstringFree(_fn1);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFGNR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *geotype,
		  med_int *num)
#else
med_int 
nmmhfgnr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, 
	 med_int *geotype, med_int *num)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int)  MEDmeshGlobalNumberRd((med_idt) *fid, 
					  (char*) _fn1, 
					  (med_int) *numdt,
					  (med_int) *numit, 
					  _etype, 
					  _gtype,
					  (med_int *) num);

  _MEDcstringFree(_fn1);

  return (_ret);
}

#ifdef PPRO_NT
med_int 
MMHFCSC(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt1, med_int *numit1, 
		  med_int *numdt2, med_int *numit2, med_float *dt2)
#else
med_int 
nmmhfcsc(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt1, med_int *numit1, 
	 med_int *numdt2, med_int *numit2, med_float *dt2)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshComputationStepCr((const med_idt) *fid, 
					    (const char*) _fn1, 
					    (const med_int) *numdt1,
					    (const med_int) *numit1, 
					    (const med_int) *numdt2,
					    (const med_int) *numit2, 
					    (med_float) *dt2); 

  _MEDcstringFree(_fn1);

  return(_ret);
}


#ifdef PPRO_NT
med_int 
MMHFCSI(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *csit, med_int *numdt, med_int *numit, med_float *dt)
#else
med_int 
nmmhfcsi(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *csit, med_int *numdt, med_int *numit, 
	 med_float *dt)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshComputationStepInfo((const med_idt) *fid, 
					      (const char*) _fn1, 
					      (const med_int) *csit,
					      (med_int *) numdt,
					      (med_int *) numit, 
					      (med_float *) dt); 

  _MEDcstringFree(_fn1);

  return(_ret);
}



#ifdef PPRO_NT
med_int 
MMHFCSR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float *dt)
#else
med_int 
nmmhfcsr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, 
	 med_float *dt)
#endif
{
  med_int _ret;
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshComputationStepDtRd((const med_idt) *fid, 
					      (const char*) _fn1, 
					      (const med_int) *numdt,
					      (const med_int) *numit, 
					      (med_float *) dt); 

  _MEDcstringFree(_fn1);

  return(_ret);
}


#ifdef PPRO_NT
med_int 
MMHFSTR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *stype)
#else
med_int 
nmmhfstr(med_idt *fid, char *mname, med_int *mnamelen,med_int *stype)
#endif
{
  med_int _ret;
  char *_fn1;
  med_sorting_type _st;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshSortingTypeRd((const med_idt) *fid, 
					(const char*) _fn1, 
					&_st);

  *stype = (med_int) _st;
  _MEDcstringFree(_fn1);

  return(_ret);
}



#ifdef PPRO_NT
med_int 
MMHFRAW(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *geotype, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *n, med_float *val)
#else
med_int 
nmmhfraw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *geotype, 
	 char *aname, med_int *anamelen,
	 med_int *n, med_float *val)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDmeshStructElementVarAttWr((med_idt) *fid, 
						_fn1, 
						(med_int) *numdt,
						(med_int) *numit, 
						_gtype,
						_fn2,
						(med_int) *n,
						(void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFIAW(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *geotype, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *n, med_int *val)
#else
med_int 
nmmhfiaw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *geotype, 
	 char *aname, med_int *anamelen,
	 med_int *n, med_int *val)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDmeshStructElementVarAttWr((med_idt) *fid, 
						_fn1, 
						(med_int) *numdt,
						(med_int) *numit, 
						_gtype,
						_fn2,
						(med_int) *n,
						(void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return (_ret);
}


#ifdef PPRO_NT
med_int
MMHFSAW(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *geotype,
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *n, char *val, unsigned int bidon3, med_int *vallen)
#else
med_int
nmmhfsaw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *geotype,
	 char *aname, med_int *anamelen,
	 med_int *n, char *val, med_int *vallen)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2, *_fn3;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  char _modelname[MED_NAME_SIZE+1]="";
  med_int _ncomp=0;
  med_attribute_type _type;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  /* nom de l'element de structure */
  _ret = MEDstructElementName((med_idt) *fid,
			      _gtype,
			      _modelname);
  if (_ret < 0)
    return (-1);

  /* nombre de composante de l'attribut */
  _ret = MEDstructElementVarAttInfoByName((med_idt) *fid,
					  _modelname,
					  _fn2,
					  &_type,
					  &_ncomp);
  if (_ncomp < 0)
    return (-1);

  /* TODO: cf nmmhfeaw */
  _fn3 = _MED1cstring(val,
		      (int) *vallen*_ncomp,
		      (int) *n*_ncomp*MED_NAME_SIZE);
  if (!_fn3)
    return(-1);

  _ret = (med_int) MEDmeshStructElementVarAttWr((med_idt) *fid,
						_fn1,
						(med_int) *numdt,
						(med_int) *numit,
						_gtype,
						_fn2,
						(med_int) *n,
						(void *) _fn3);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);

  return (_ret);
}



#ifdef PPRO_NT
med_int
MMHFRAR(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *geotype,
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_float *val)
#else
med_int 
nmmhfrar(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *geotype, 
	 char *aname, med_int *anamelen,
	 med_float *val)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDmeshStructElementVarAttRd((med_idt) *fid, 
						_fn1, 
						(med_int) *numdt,
						(med_int) *numit, 
						_gtype,
						_fn2,
						(void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFIAR(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *geotype, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *val)
#else
med_int 
nmmhfiar(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *geotype, 
	 char *aname, med_int *anamelen,
	 med_int *val)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  med_geometry_type _gtype = (med_geometry_type) *geotype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDmeshStructElementVarAttRd((med_idt) *fid, 
						_fn1, 
						(med_int) *numdt,
						(med_int) *numit, 
						_gtype,
						_fn2,
						(void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return (_ret);
}



#ifdef PPRO_NT
med_int 
MMHFSAR(med_idt *fid, char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *geotype, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  char *val, unsigned int bidon3)
#else
med_int 
nmmhfsar(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *geotype, 
	 char *aname, med_int *anamelen,
	 char *val)
#endif
{
  med_int _ret; 
  char *_fn1, *_fn2;
  char *_fs3;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_attribute_type _atype;
  med_int _nacomp=0;
  med_int _size=0;
  med_int _n;
  med_bool _coordinatechangement=MED_FALSE;
  med_bool _geotransformation=MED_FALSE;
  char _modelname[MED_NAME_SIZE+1]="";

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  /* on recupere le nom du modèle de structure */
  _ret = MEDstructElementName((med_idt) *fid, 
			      _gtype, 
			      _modelname);   
  if (_ret < 0)
    return (-1);

  /* on recupere le nombre de composante de l'attribut */
  if (MEDstructElementVarAttInfoByName((med_idt ) *fid, 
				       _modelname,  
				       _fn2, 
				       &_atype, 
				       &_nacomp) < 0) 
    return -1;

 /* On recupere le nombre d'element correspondant
    au type de modele de structure dans le maillage
    de calcul 
  */
  _n =  MEDmeshnEntity((med_idt) *fid, 
		       _fn1, 
		       (med_int) *numdt,
		       (med_int) *numit, 
		       MED_STRUCT_ELEMENT,
		       _gtype,
		       MED_CONNECTIVITY, 
		       MED_NODAL,
		       &_coordinatechangement,
		       &_geotransformation);
  if (_n < 0)
    return -1;

  /* memory allocation */
  _size = _n*_nacomp*MED_NAME_SIZE*sizeof(char);
  _fs3 = (char *) malloc(_size+1);
  *_fs3='\0';
  if (!_fs3)
    return(-1);

  _ret = (med_int) MEDmeshStructElementVarAttRd((med_idt) *fid, 
						_fn1, 
						(med_int) *numdt,
						(med_int) *numit, 
						_gtype,
						_fn2,
						(void *) _fs3);

  _MEDc2fString(_fs3,val,_size);
  free(_fs3);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return (_ret);
}



#ifdef PPRO_NT
med_int 
MMHFENI(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_int *entype, med_int *it, 
		  char *geoname, unsigned int bidon1, med_int *geotype)
#else
med_int 
nmmhfeni(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_int *entype, med_int *it, 
	 char *geoname, med_int *geotype)
#endif
{
  med_int _ret; 
  char *_fn1;
  char _fs1[MED_NAME_SIZE+1]="";
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int)  MEDmeshEntityInfo((med_idt) *fid, 
				      (char*) _fn1, 
				      (med_int) *numdt,
				      (med_int) *numit, 
				      _etype, 
				      (med_int) *it,
				      _fs1,
				      &_gtype);
  
  *geotype = (med_int) _gtype;
  _MEDc2fString(_fs1,geoname,MED_NAME_SIZE);
  
  _MEDcstringFree(_fn1);

  return (_ret);
}


#ifdef PPRO_NT
med_int 
MMHFTFW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *numdt, med_int *numit, med_float *dt, med_float *tsf)
#else
med_int 
nmmhftfw(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float *dt, med_float *tsf)
#endif
{
  med_int _ret; 
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshNodeCoordinateTrsfWr((med_idt) *fid, 
					       (char*) _fn1, 
					       (med_int) *numdt,
					       (med_int) *numit, 
					       (med_float) *dt,
					       (med_float *) tsf); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MMHFTFR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen,
		med_int *numdt, med_int *numit, med_float *tsf)
#else
med_int 
nmmhftfr(med_idt *fid, char *mname, med_int *mnamelen,
	 med_int *numdt, med_int *numit, med_float *tsf)
#endif
{
  med_int _ret; 
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDmeshNodeCoordinateTrsfRd((med_idt) *fid, 
					       (char*) _fn1, 
					       (med_int) *numdt,
					       (med_int) *numit, 
					       (med_float *) tsf); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}




#ifdef PPRO_NT
med_int 
MMHFAAW(med_idt *fid, 
		  char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *datatype, 
		  med_int *numdt, 
		  med_int *numit,
		  med_int *entype, 
		  med_int *geotype,  
		  unsigned char **flt,
		  med_int *val)
#else
med_int 
nmmhfaaw(med_idt *fid, 
	 char *mname, med_int *mnamelen,
	 med_int *datatype,
	 med_int *numdt, 
	 med_int *numit, 
	 med_int *entype,
	 med_int *geotype, 
	 unsigned char **flt, 
	 med_int *val)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_data_type _datatype = (med_data_type) *datatype;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshEntityAttributeAdvancedWr((med_idt) *fid, 
						    _fn1,
						    _datatype,
						    (med_int) *numdt,
						    (med_int) *numit, 
						    _etype, 
						    _gtype,
						    _filter,
						    (med_int *) val);

  _MEDcstringFree(_fn1);

  return (_ret);
}



#ifdef PPRO_NT
med_int 
MMHFAAR(med_idt *fid, 
		  char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *datatype, 
		  med_int *numdt, 
		  med_int *numit,
		  med_int *entype, 
		  med_int *geotype,  
		  unsigned char **flt,
		  med_int *val)
#else
med_int 
nmmhfaar(med_idt *fid, 
	 char *mname, med_int *mnamelen,
	 med_int *datatype,
	 med_int *numdt, 
	 med_int *numit, 
	 med_int *entype,
	 med_int *geotype, 
	 unsigned char **flt, 
	 med_int *val)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_data_type _datatype = (med_data_type) *datatype;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshEntityAttributeAdvancedRd((med_idt) *fid, 
						    _fn1,
						    _datatype,
						    (med_int) *numdt,
						    (med_int) *numit, 
						    _etype, 
						    _gtype,
						    _filter,
						    (med_int *) val);

  _MEDcstringFree(_fn1);

  return (_ret);
}





#ifdef PPRO_NT
med_int 
MMHFASW(med_idt *fid, 
		  char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *datatype, 
		  med_int *numdt, 
		  med_int *numit,
		  med_int *entype, 
		  med_int *geotype,  
		  unsigned char **flt,
		  char *val)
#else
med_int 
nmmhfasw(med_idt *fid, 
	 char *mname, med_int *mnamelen,
	 med_int *datatype,
	 med_int *numdt, 
	 med_int *numit, 
	 med_int *entype,
	 med_int *geotype, 
	 unsigned char **flt, 
	 char *val)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_data_type _datatype = (med_data_type) *datatype;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshEntityAttributeAdvancedWr((med_idt) *fid, 
						    _fn1,
						    _datatype,
						    (med_int) *numdt,
						    (med_int) *numit, 
						    _etype, 
						    _gtype,
						    _filter,
						    val);

  _MEDcstringFree(_fn1);

  return (_ret);
}




#ifdef PPRO_NT
med_int 
MMHFASR(med_idt *fid, 
		  char *mname, unsigned int bidon, med_int *mnamelen,
		  med_int *datatype, 
		  med_int *numdt, 
		  med_int *numit,
		  med_int *entype, 
		  med_int *geotype,  
		  unsigned char **flt,
		  char *val)
#else
med_int 
nmmhfasr(med_idt *fid, 
	 char *mname, med_int *mnamelen,
	 med_int *datatype,
	 med_int *numdt, 
	 med_int *numit, 
	 med_int *entype,
	 med_int *geotype, 
	 unsigned char **flt, 
	 char *val)
#endif
{
  med_int _ret; 
  char *_fn1;
  med_entity_type _etype = (med_entity_type) *entype;
  med_geometry_type _gtype = (med_geometry_type) *geotype;
  med_data_type _datatype = (med_data_type) *datatype;
  med_filter *_filter = (med_filter*) *flt; 

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDmeshEntityAttributeAdvancedRd((med_idt) *fid, 
						    _fn1,
						    _datatype,
						    (med_int) *numdt,
						    (med_int) *numit, 
						    _etype, 
						    _gtype,
						    _filter,
						    val);

  _MEDcstringFree(_fn1);

  return (_ret);
}
