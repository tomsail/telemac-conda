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
- MEDstructElementCr   
- MEDnStructElement          
- MEDstructElementInfo
- MEDstructElementInfoByName
- MEDstructElementName.c
- MEDstructElementGeotype 
- MEDstructElementVarAttCr
- MEDstructElementVarAttInfoByName
- MEDstructElementVarAttInfo
- MEDstructElementAttSizeof  
- MEDstructElementConstAttWr
- MEDstructElementConstAttWithProfileWr 
- MEDstructElementConstAttInfoByName        
- MEDstructElementConstAttInfo              
- MEDstructElementConstAttRd                                
*/


#define nmsefcre F77_FUNC(msefcre,MSEFCRE)
#define nmsefnse F77_FUNC(msefnse,MSEFNSE)
#define nmsefsei F77_FUNC(msefsei,MSEFSEI)
#define nmsefsin F77_FUNC(msefsin,MSEFSIN)
#define nmsefsen F77_FUNC(msefsen,MSEFSEN)
#define nmsefsgt F77_FUNC(msefsgt,MSEFSGT)
#define nmsefvac F77_FUNC(msefvac,MSEFVAC)
#define nmsefvni F77_FUNC(msefvni,MSEFVNI)
#define nmsefvai F77_FUNC(msefvai,MSEFVAI)
#define nmsefasz F77_FUNC(msefasz,MSEFASZ)
#define nmsefraw F77_FUNC(msefraw,MSEFRAW)
#define nmsefiaw F77_FUNC(msefiaw,MSEFIAW)
#define nmsefsaw F77_FUNC(msefsaw,MSEFSAW)
#define nmsefrpw F77_FUNC(msefrpw,MSEFRPW)
#define nmsefipw F77_FUNC(msefipw,MSEFIPW)
#define nmsefspw F77_FUNC(msefspw,MSEFSPW)
#define nmsefcni F77_FUNC(msefcni,MSEFCNI)
#define nmsefcai F77_FUNC(msefcai,MSEFCAI)
#define nmsefrar F77_FUNC(msefrar,MSEFRAR)
#define nmsefiar F77_FUNC(msefiar,MSEFIAR)
#define nmsefsar F77_FUNC(msefsar,MSEFSAR)


#ifdef PPRO_NT
med_int 
MSEFCRE(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  med_int * mdim,
		  char *smname, unsigned int bidon2, med_int *smnamelen,
		  med_int *setype, med_int *sgtype)
#else
med_int 
nmsefcre(med_idt *fid, 
         char *mname, med_int *mnamelen, 
         med_int * mdim,
	 char *smname, med_int *smnamelen,
	 med_int *setype, 
         med_int *sgtype)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_entity_type _setype = (med_entity_type) *setype;
  med_geometry_type _sgtype = (med_geometry_type) *sgtype;


  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  _fn2 = _MED2cstring((char *) smname, (int) *smnamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDstructElementCr((const med_idt) *fid, 
				      _fn1, 
				      (const med_int) *mdim,
				      _fn2, 
				      _setype,
				      _sgtype);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return _ret;
}



#ifdef PPRO_NT
med_int 
MSEFNSE(med_idt *fid)
#else
med_int 
nmsefnse (med_idt *fid)
#endif
{
  med_int _ret; 
  
  _ret = (med_int) MEDnStructElement((const med_idt) *fid); 

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MSEFSEI(med_idt *fid, med_int *it, 
		  char *mname, unsigned int bidon1,
		  med_int *mgtype, med_int * mdim,
		  char *smname, unsigned int bidon2,
		  med_int *setype, med_int *snnode, med_int *sncell,
		  med_int *sgtype, med_int *ncatt, med_int *ap,
		  med_int *nvatt)
#else
med_int 
nmsefsei(med_idt *fid, med_int *it, 
	 char *mname, 
	 med_int *mgtype, med_int * mdim,
	 char *smname, 
	 med_int *setype, med_int *snnode, med_int *sncell,
	 med_int *sgtype, med_int *ncatt, med_int *ap,
	 med_int *nvatt)
#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1]="";
  char _fs2[MED_NAME_SIZE+1]="";
  med_entity_type _setype;
  med_geometry_type _mgeotype;
  med_geometry_type _sgtype;
  med_bool _ap;


  _ret = (med_int) MEDstructElementInfo((const med_idt) *fid, 
					(med_int) *it,
					_fs1,
					&_mgeotype,
					(med_int *) mdim,
					_fs2,
					&_setype,
					(med_int *) snnode, 
					(med_int *) sncell,
					&_sgtype,
					(med_int *) ncatt,
					&_ap,
					(med_int *) nvatt);

  _MEDc2fString(_fs1,mname,MED_NAME_SIZE);
  _MEDc2fString(_fs2,smname,MED_NAME_SIZE);
  *mgtype = (med_int) _mgeotype;
  *setype = (med_int) _setype;
  *sgtype = (med_int) _sgtype;
  *ap = (med_int) _ap;

  return _ret;
}


#ifdef PPRO_NT
med_int 
MSEFSIN(med_idt *fid, 
		  char *mname, unsigned int bidon1, med_int *mnamelen,
		  med_int *mgtype, med_int * mdim,
		  char *smname, unsigned int bidon2,
		  med_int *setype, med_int *snnode, med_int *sncell,
		  med_int *sgtype, med_int *ncatt, med_int *ap,
		  med_int *nvatt)
#else
med_int 
nmsefsin(med_idt *fid, 
	 char *mname, med_int *mnamelen,
	 med_int *mgtype, med_int * mdim,
	 char *smname, 
	 med_int *setype, med_int *snnode, med_int *sncell,
	 med_int *sgtype, med_int *ncatt, med_int *ap,
	 med_int *nvatt)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs2[MED_NAME_SIZE+1]="";
  med_entity_type _setype;
  med_geometry_type _mgeotype;
  med_geometry_type _sgtype;
  med_bool _ap;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDstructElementInfoByName((const med_idt) *fid, 
					      _fn1,
					      &_mgeotype,
					      (med_int *) mdim,
					      _fs2,
					      &_setype,
					      (med_int *) snnode, 
					      (med_int *) sncell,
					      &_sgtype,
					      (med_int *) ncatt,
					      &_ap,
					      (med_int *) nvatt);
  
  _MEDc2fString(_fs2,smname,MED_NAME_SIZE);
  *mgtype = (med_int) _mgeotype;
  *setype = (med_int) _setype;
  *sgtype = (med_int) _sgtype;
  *ap = (med_int) _ap;

  _MEDcstringFree(_fn1);

  return _ret;
}



#ifdef PPRO_NT
med_int 
MSEFSEN(med_idt *fid, med_int *mgtype, char *mname, unsigned int bidon)
#else
med_int 
nmsefsen(med_idt *fid, med_int *mgtype, char *mname)
#endif
{
  med_int _ret;
  char _fs1[MED_NAME_SIZE+1]="";
  med_geometry_type _mgtype = (med_geometry_type) *mgtype;


  _ret = (med_int) MEDstructElementName((const med_idt) *fid, 
					_mgtype,
					_fs1);

  _MEDc2fString(_fs1,mname,MED_NAME_SIZE);

  return _ret;
}



#ifdef PPRO_NT
med_int 
MSEFSGT(med_idt *fid, char *mname, unsigned int bidon,med_int *mnamelen)
#else
med_int 
nmsefsgt(med_idt *fid, char *mname, med_int *mnamelen)
#endif
{
  med_int _ret; 
  char *_fn1;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);
  
  _ret = (med_int) MEDstructElementGeotype((const med_idt) *fid,
					   _fn1); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}



#ifdef PPRO_NT
med_int 
MSEFVAC(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *atype, med_int *anc)
#else
med_int 
nmsefvac(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *atype, med_int *anc)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_attribute_type _atype = (med_attribute_type) *atype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDstructElementVarAttCr((const med_idt) *fid, 
					    _fn1, 
					    _fn2, 
					    _atype,
					    (med_int) *anc);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MSEFVNI(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *atype, med_int *anc)
#else
med_int 
nmsefvni(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *atype, med_int *anc)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_attribute_type _atype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDstructElementVarAttInfoByName((const med_idt) *fid, 
						    _fn1, 
						    _fn2, 
						    &_atype,
						    (med_int *) anc);

  *atype = (med_int) _atype;

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MSEFVAI(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  med_int *it, char *aname, unsigned int bidon2,
		  med_int *atype, med_int *anc)
#else
med_int 
nmsefvai(med_idt *fid, char *mname, med_int *mnamelen, med_int *it,
	 char *aname, med_int *atype, med_int *anc)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs1[MED_NAME_SIZE+1]="";
  med_attribute_type _atype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDstructElementVarAttInfo((const med_idt) *fid, 
					      _fn1, 
					      (med_int) *it,
					      _fs1, 
					      &_atype,
					      (med_int *) anc);

  *atype = (med_int) _atype;
  _MEDc2fString(_fs1,aname,MED_NAME_SIZE);

  _MEDcstringFree(_fn1);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MSEFASZ(med_int *atype)
#else
med_int 
nmsefasz(med_int *atype)
#endif
{
  med_int _ret; 
  med_attribute_type _atype = (med_attribute_type) *atype;
  
  _ret = (med_int) MEDstructElementAttSizeof(_atype); 

  return(_ret); 
}


#ifdef PPRO_NT
med_int 
MSEFRAW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *atype, med_int *anc,
		  med_int *setype, med_float *val)
#else
med_int 
nmsefraw(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *atype, med_int *anc,
	 med_int *setype, med_float *val)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_attribute_type _atype = (med_attribute_type) *atype;
  med_entity_type _setype = (med_entity_type) *setype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDstructElementConstAttWr((const med_idt) *fid, 
					      _fn1, 
					      _fn2, 
					      _atype,
					      (med_int) *anc,
					      _setype,
					      (void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MSEFIAW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *atype, med_int *anc,
		  med_int *setype, med_int *val)
#else
med_int 
nmsefiaw(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *atype, med_int *anc,
	 med_int *setype, med_int *val)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_attribute_type _atype = (med_attribute_type) *atype;
  med_entity_type _setype = (med_entity_type) *setype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDstructElementConstAttWr((const med_idt) *fid, 
					      _fn1, 
					      _fn2, 
					      _atype,
					      (med_int) *anc,
					      _setype,
					      (void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return _ret;
}



#ifdef PPRO_NT
med_int 
MSEFSAW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *atype, med_int *anc, med_int *setype, 
		  char *val, unsigned int bidon3, med_int *vallen)
#else
med_int 
nmsefsaw(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *atype, med_int *anc, med_int *setype,
	 char *val, med_int *vallen)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2,*_fn3;
  med_attribute_type _atype = (med_attribute_type) *atype;
  med_entity_type _setype = (med_entity_type) *setype;
  int _nentity=0; 
  med_geometry_type _geotype=0;
  med_int _modeldim=0;
  char _supportmeshname[MED_NAME_SIZE+1]="";
  med_entity_type _entitytype=0;
  med_int _nnode=0;
  med_int _ncell=0; 
  med_geometry_type _sgeotype=0; 
  med_int _nconstatt=0; 
  med_bool _anyprofile=0;
  med_int _nvaratt=0;
  med_bool _coordinatechangement=MED_FALSE;
  med_bool _geotransformation=MED_FALSE;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  /* on recupere le nom du maillage support */
  _ret = MEDstructElementInfoByName((med_idt) *fid,
				    _fn1,
				    &_geotype, 
				    &_modeldim,
                                    _supportmeshname, 
				    &_entitytype, 
				    &_nnode, 
				    &_ncell,
                                    &_sgeotype, 
				    &_nconstatt, 
				    &_anyprofile, 
				    &_nvaratt);
  if (_ret < 0)
    return (-1);
  
  /* on recupere le nombre d'entite dans le maillage support :
     MED_NODE ou MED_CELL */
  _nentity = MEDmeshnEntity((med_idt) *fid, 
			    _supportmeshname, 
			    MED_NO_DT, 
			    MED_NO_IT, 
			    _setype, 
			    _sgeotype,
			    MED_COORDINATE, 
			    MED_NO_CMODE,
			    &_coordinatechangement,
			    &_geotransformation);
  if (_nentity < 0)
    return (-1); 
  
  _fn3 = _MED1cstring((char *) val, (int) *vallen*_nentity, (int) *anc*_nentity*MED_NAME_SIZE);
  if (!_fn3)
    return(-1);
  
  _ret = (med_int) MEDstructElementConstAttWr((const med_idt) *fid, 
					      _fn1, 
					      _fn2, 
					      _atype,
					      (med_int) *anc,
					      _setype,
					      (void *) _fn3);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);

  return _ret;
}



#ifdef PPRO_NT
med_int 
MSEFRPW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *atype, med_int *anc,
		  med_int *setype, 
		  char *pname, unsigned int bidon3, med_int *pnamelen,
		  med_float *val)
#else
med_int 
nmsefrpw(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *atype, med_int *anc,
	 med_int *setype, 
	 char *pname, med_int *pnamelen,
	 med_float *val)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2, *_fn3;
  med_attribute_type _atype = (med_attribute_type) *atype;
  med_entity_type _setype = (med_entity_type) *setype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _fn3 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn3)
    return(-1);

  _ret = (med_int) MEDstructElementConstAttWithProfileWr((const med_idt) *fid, 
							 _fn1, 
							 _fn2, 
							 _atype,
							 (med_int) *anc,
							 _setype,
							 _fn3,
							 (void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);

  return _ret;
}




#ifdef PPRO_NT
med_int 
MSEFIPW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *atype, med_int *anc,
		  med_int *setype, 
		  char *pname, unsigned int bidon3, med_int *pnamelen,
		  med_int *val)
#else
med_int 
nmsefipw(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *atype, med_int *anc,
	 med_int *setype, 
	 char *pname, med_int *pnamelen,
	 med_int *val)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2, *_fn3;
  med_attribute_type _atype = (med_attribute_type) *atype;
  med_entity_type _setype = (med_entity_type) *setype;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _fn3 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn3)
    return(-1);

  _ret = (med_int) MEDstructElementConstAttWithProfileWr((const med_idt) *fid, 
							 _fn1, 
							 _fn2, 
							 _atype,
							 (med_int) *anc,
							 _setype,
							 _fn3,
							 (void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);

  return _ret;
}



#ifdef PPRO_NT
med_int 
MSEFSPW(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *atype, med_int *anc, med_int *setype, 
		  char *pname, unsigned int bidon3, med_int *pnamelen,
		  char *val, unsigned int bidon4, med_int *vallen)
#else
med_int 
nmsefspw(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *atype, med_int *anc, med_int *setype,
	 char *pname, med_int *pnamelen,
	 char *val, med_int *vallen)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2,*_fn3, *_fn4;
  med_attribute_type _atype = (med_attribute_type) *atype;
  med_entity_type _setype = (med_entity_type) *setype;
  int _nentity=0; 
  med_geometry_type _geotype=0;
  med_int _modeldim=0;
  char _supportmeshname[MED_NAME_SIZE+1]="";
  med_entity_type _entitytype=0;
  med_int _nnode=0;
  med_int _ncell=0; 
  med_geometry_type _sgeotype=0; 
  med_int _nconstatt=0; 
  med_bool _anyprofile=0;
  med_int _nvaratt=0;
  med_bool _coordinatechangement=MED_FALSE;
  med_bool _geotransformation=MED_FALSE;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _fn3 = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn3)
    return(-1);

  /* on recupere le nom du maillage support */
  _ret = MEDstructElementInfoByName((med_idt) *fid,
				    _fn1,
				    &_geotype, 
				    &_modeldim,
                                    _supportmeshname, 
				    &_entitytype, 
				    &_nnode, 
				    &_ncell,
                                    &_sgeotype, 
				    &_nconstatt, 
				    &_anyprofile, 
				    &_nvaratt);
  if (_ret < 0)
    return (-1);
  
  /* on recupere le nombre d'entite dans le maillage support :
     MED_NODE ou MED_CELL */
  if (_setype == MED_NODE)
    _nentity = MEDmeshnEntity((med_idt) *fid, 
			      _supportmeshname, 
			      MED_NO_DT, 
			      MED_NO_IT, 
			      _setype, 
			      _sgeotype,
			      MED_COORDINATE, 
			      MED_NO_CMODE,
			      &_coordinatechangement,
			      &_geotransformation);
  else
    _nentity = MEDmeshnEntity((med_idt) *fid, 
			      _supportmeshname, 
			      MED_NO_DT, 
			      MED_NO_IT, 
			      _setype, 
			      _sgeotype,
			      MED_CONNECTIVITY, 
			      MED_NODAL,
			      &_coordinatechangement,
			      &_geotransformation);
  if (_nentity < 0)
    return (-1); 
  
  _fn4 = _MED1cstring((char *) val, (int) *vallen*_nentity, (int) *anc*_nentity*MED_NAME_SIZE);
  if (!_fn4)
    return(-1);

  _ret = (med_int) MEDstructElementConstAttWithProfileWr((const med_idt) *fid, 
							 _fn1, 
							 _fn2, 
							 _atype,
							 (med_int) *anc,
							 _setype,
							 _fn3,
							 (void *) _fn4);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);
  _MEDcstringFree(_fn4);


  return _ret;
}


#ifdef PPRO_NT
med_int 
MSEFCNI(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *atype, med_int *anc,
		  med_int *setype, char *pname, unsigned int bidon3, med_int *psize)
#else
med_int 
nmsefcni(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *atype, med_int *anc,
	 med_int *setype, char *pname, med_int *psize)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_attribute_type _atype;
  med_entity_type _setype;
  char _fs1[MED_NAME_SIZE+1]="";

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDstructElementConstAttInfoByName((const med_idt) *fid, 
						      _fn1, 
						      _fn2, 
						      &_atype,
						      (med_int *) anc,
						      &_setype,
						      _fs1,
						      (med_int *) psize);

  *atype = (med_int) _atype;
  *setype = (med_int) _setype;
  _MEDc2fString(_fs1,pname,MED_NAME_SIZE);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MSEFCAI(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  med_int *it,
		  char *aname, unsigned int bidon2,
		  med_int *atype, med_int *anc,
		  med_int *setype, char *pname, unsigned int bidon3 ,med_int *psize)
#else
med_int 
nmsefcai(med_idt *fid, char *mname, med_int *mnamelen, 
	 med_int *it,
	 char *aname, 
	 med_int *atype, med_int *anc,
	 med_int *setype, char *pname, med_int *psize)
#endif
{
  med_int _ret;
  char *_fn1;
  med_attribute_type _atype;
  med_entity_type _setype;
  char _fs1[MED_NAME_SIZE+1]="";
  char _fs2[MED_NAME_SIZE+1]="";


  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDstructElementConstAttInfo((const med_idt) *fid, 
						_fn1, 
						(med_int) *it,
						_fs1, 
						&_atype,
						(med_int *) anc,
						&_setype,
						_fs2,
						(med_int *) psize);

  *atype = (med_int) _atype;
  *setype = (med_int) _setype;
  _MEDc2fString(_fs1,aname,MED_NAME_SIZE);
  _MEDc2fString(_fs2,pname,MED_NAME_SIZE);

  _MEDcstringFree(_fn1);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MSEFRAR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_float *val)
#else
med_int 
nmsefrar(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_float *val)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDstructElementConstAttRd((const med_idt) *fid, 
					      _fn1, 
					      _fn2, 
					      (void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return _ret;
}


#ifdef PPRO_NT
med_int 
MSEFIAR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  med_int *val)
#else
med_int 
nmsefiar(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 med_int *val)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  _ret = (med_int) MEDstructElementConstAttRd((const med_idt) *fid, 
					      _fn1, 
					      _fn2, 
					      (void *) val);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return _ret;
}



#ifdef PPRO_NT
med_int 
MSEFSAR(med_idt *fid, char *mname, unsigned int bidon, med_int *mnamelen, 
		  char *aname, unsigned int bidon2, med_int *anamelen,
		  char *val, unsigned int bidon3)
#else
med_int 
nmsefsar(med_idt *fid, char *mname, med_int *mnamelen, 
	 char *aname, med_int *anamelen,
	 char *val)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  char *_fs1;
  med_entity_type _setype;
  med_geometry_type _mgeotype, _gctype;
  med_geometry_type _sgtype;
  med_int _mdim, _nnode, _ncell;
  char _meshname[MED_NAME_SIZE+1]="";
  char _pname[MED_NAME_SIZE+1]="";
  med_entity_type _etype;
  med_int _ncatt, _nvatt, _ncomp, _psize;
  med_bool _ap;
  med_attribute_type _type;
  int _size=0; 

  _fn1 = _MED2cstring((char *) mname, (int) *mnamelen);
  if (!_fn1)
    return(-1);

  _fn2 = _MED2cstring((char *) aname, (int) *anamelen);
  if (!_fn2)
    return(-1);

  if (MEDstructElementInfoByName((med_idt) *fid, 
				 _fn1, 
				 &_mgeotype, 
				 &_mdim,
                                 _meshname, 
				 &_etype, 
				 &_nnode, 
				 &_ncell,
                                 &_gctype, 
				 &_ncatt, 
				 &_ap, 
				 &_nvatt)  < 0) 
    return -1;

  if (MEDstructElementConstAttInfoByName((med_idt) *fid, 
					 _fn1,  
					 _fn2, 
					 &_type, 
					 &_ncomp, 
					 &_setype,
                                         _pname, 
					 &_psize) < 0) 
    return -1;

  if (_psize != 0)
    _size = _psize*_ncomp*MED_NAME_SIZE*sizeof(char);
  else
    if (_etype == MED_NODE)
      _size = _nnode*_ncomp*MED_NAME_SIZE*sizeof(char);
    else
      _size = _ncell*_ncomp*MED_NAME_SIZE*sizeof(char);
  _fs1 = (char *) malloc(_size+1);
  *_fs1='\0';

  _ret = (med_int) MEDstructElementConstAttRd((const med_idt) *fid, 
					      _fn1, 
					      _fn2, 
					      (void *) _fs1);

  _MEDc2fString(_fs1,val,_size);
  free(_fs1);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return _ret;
}
