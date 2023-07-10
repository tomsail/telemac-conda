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
- MEDsubdomainJointCr
- MEDsubdomainCorrespondenceWr    
- MEDnSubdomainJoint
- MEDsubdomainJointInfo  
- MEDsubdomainCorrespondenceSizeInfo
- MEDsubdomainCorrespondenceSize  
- MEDsubdomainCorrespondenceRd    
- MEDsubdomainComputingStepInfo  

*/

#define nmsdfjcr F77_FUNC(msdfjcr,MSDFJCR)
#define nmsdfcrw F77_FUNC(msdfcrw,MSDFCRW)
#define nmsdfnjn F77_FUNC(msdfnjn,MSDFNJN)
#define nmsdfjni F77_FUNC(msdfjni,MSDFJNI)
#define nmsdfszi F77_FUNC(msdfszi,MSDFSZI)
#define nmsdfcsz F77_FUNC(msdfcsz,MSDFCSZ)
#define nmsdfcrr F77_FUNC(msdfcrr,MSDFCRR)
#define nmsdfcsi F77_FUNC(msdfcsi,MSDFCSI)


#ifdef PPRO_NT
med_int 
MSDFJCR(med_idt *fid,
                   char *lmname,  unsigned int bidon1, med_int *lon1,
                   char *jname,   unsigned int bidon2, med_int *lon2, 
                   char *desc, unsigned int bidon3, med_int *lon3,
		   med_int *dom,
		   char *rmname,  unsigned int bidon4, med_int *lon4
		   )
#else
med_int 
nmsdfjcr(med_idt *fid, 
	 char *lmname,  med_int *lon1,
	 char *jname, med_int *lon2, 
	 char *desc, med_int *lon3,
	 med_int *dom,
	 char *rmname,med_int *lon4)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2, *_fn3, *_fn4;

  _fn1 = _MED2cstring(lmname, (int) * lon1);
  _fn2 = _MED2cstring(jname,  (int) * lon2);
  _fn3 = _MED1cstring(desc, (int) * lon3,MED_COMMENT_SIZE);
  _fn4 = _MED2cstring(rmname, (int) * lon4);

  if (!_fn1 || !_fn2 || !_fn3 || !_fn4)
    return(-1); 

  _ret = (med_int) MEDsubdomainJointCr((med_idt) *fid,
				      _fn1,
				      _fn2,
				      _fn3,
				      (med_int) *dom,
				      _fn4); 

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);
  _MEDcstringFree(_fn4);

  return(_ret);   
}


#ifdef PPRO_NT
med_int 
MSDFCRW(med_idt *fid, 
		  char *maa_local,unsigned int bidon1, med_int *lon1,
		  char *jn,unsigned int bidon2, med_int *lon2, 
		  med_int *numdt, med_int *numit,
		  med_int * typ_ent_local, med_int * typ_geo_local,
		  med_int * typ_ent_distant, med_int * typ_geo_distant,
		  med_int *n, med_int *corrtab)
#else
med_int 
nmsdfcrw (med_idt *fid, 
	  char *maa_local,med_int *lon1,
	  char *jn,med_int *lon2, 
	  med_int *numdt, med_int *numit,
	  med_int * typ_ent_local, med_int * typ_geo_local,
	  med_int * typ_ent_distant, med_int * typ_geo_distant,
	  med_int *n, med_int *corrtab)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_entity_type _let = (med_entity_type) *typ_ent_local;
  med_entity_type _rnt = (med_entity_type) *typ_ent_distant;
  med_geometry_type _ltg = (med_geometry_type) *typ_geo_local;
  med_geometry_type _rtg = (med_geometry_type) *typ_geo_distant;

  _fn1 = _MED2cstring(maa_local, (int) *lon1);
  _fn2 = _MED2cstring(jn,  (int) *lon2);

  if (!_fn1 || !_fn2)
    return(-1); 

  _ret = (med_int) MEDsubdomainCorrespondenceWr((med_idt) *fid, 
						_fn1, 
						_fn2,  
						(med_int) *numdt,
						(med_int) *numit,
						_let, 
						_ltg,
						_rnt,
						_rtg,
						(med_int) *n,
						(med_int *) corrtab);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);


  return(_ret);   
}


#ifdef PPRO_NT
med_int 
MSDFNJN(med_idt *fid,
                  char *maa,  unsigned int bidon1, med_int *lon1)
#else
med_int  
nmsdfnjn(med_idt *fid, 
	 char *maa, med_int *lon1)
#endif
{
  char *_fn1;
  med_int _n;

  _fn1 = _MED2cstring(maa, (int) * lon1);


  if (!_fn1)
    return(-1); 

  _n = (med_int) MEDnSubdomainJoint((med_idt) *fid,
				    _fn1);

  _MEDcstringFree(_fn1);

  return(_n);   
}





#ifdef PPRO_NT
med_int 
MSDFJNI(med_idt *fid,
                  char *maa, unsigned int bidon1, med_int *lon1,
		  med_int *ind, char *jname, unsigned int bidon2,
		  char *desc,unsigned int bidon3,
		  med_int *dom,char *rname,unsigned int bidon4,
		  med_int *nstep, med_int *ncor)
#else
med_int
nmsdfjni(med_idt *fid,
	 char *maa, med_int *lon1,
	 med_int *ind, char *jname,
	 char *desc, med_int *dom,char *rname,
	 med_int *nstep, med_int *ncor)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs1[MED_NAME_SIZE+1];   /* nom du joint OUT */
  char _fs2[MED_COMMENT_SIZE+1];  /* nom de la description   OUT */
  char _fs3[MED_NAME_SIZE+1];   /* nom du maillage distant OUT */


  /* nom maillage IN */
  _fn1 = _MED2cstring(maa, (int) * lon1);
  if (!_fn1)
    return(-1);

  _ret = (med_int) MEDsubdomainJointInfo((med_idt) *fid,
					 _fn1,                /* maillage local IN */
					 (med_int) *ind,      /* indice du joint dans le maillage IN */
					 _fs1,                /* nom joint OUT */
					 _fs2,                /* desc joint OUT */
					 (med_int * ) dom,    /* numero ss domaine distant OUT */
					 _fs3,                 /* nom maillage distant OUT */
					 (med_int *) nstep,
					 (med_int *) ncor);

  _MEDc2fString(_fs1,jname,MED_NAME_SIZE);
  _MEDc2fString(_fs2,desc,MED_COMMENT_SIZE);
  _MEDc2fString(_fs3,rname,MED_NAME_SIZE);

  _MEDcstringFree(_fn1);

  return(_ret);
}




#ifdef PPRO_NT
med_int 
MSDFSZI(med_idt *fid,
		  char *maa, unsigned int bidon1, med_int *lon1,
		  char *jname, unsigned int bidon2, med_int *lon2,
		  med_int *numdt, med_int *numit, med_int *it,
		  med_int * typ_ent_local, med_int * typ_geo_local,
		  med_int * typ_ent_distant, med_int * typ_geo_distant,
		  med_int *ncor)
#else
med_int 
nmsdfszi(med_idt *fid,
	 char *maa, med_int *lon1,
	 char *jname, med_int *lon2,
	 med_int *numdt, med_int *numit, med_int *it,
	 med_int *typ_ent_local, med_int *typ_geo_local,
	 med_int *typ_ent_distant, med_int *typ_geo_distant,
	 med_int *ncor)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_entity_type _let, _rnt;
  med_geometry_type _ltg,_rtg;

  _fn1 = _MED2cstring((char *) maa, (int) * lon1);
  if (!_fn1)
    return(-1); 

  _fn2 = _MED2cstring((char *) jname, (int) * lon2);
  if (!_fn2)
    return(-1); 

  _ret = (med_int) MEDsubdomainCorrespondenceSizeInfo((med_idt) *fid,
						      _fn1,        
						      _fn2,
						      (med_int) *numdt,
						      (med_int) *numit,
						      (med_int) *it,
						      &_let,
						      &_ltg,
						      &_rnt,
						      &_rtg,
						      (med_int *) ncor);


  *typ_ent_local = (med_int) _let;
  *typ_geo_local = (med_int) _ltg;
  *typ_ent_distant = (med_int) _rnt;
  *typ_geo_distant = (med_int) _rtg;

  _MEDcstringFree(_fn1); 
  _MEDcstringFree(_fn2);

  return(_ret);   
}



#ifdef PPRO_NT
med_int 
MSDFCSZ(med_idt *fid,
		  char *maa, unsigned int bidon1, med_int *lon1,
		  char *jname, unsigned int bidon2, med_int *lon2,
		  med_int *numdt, med_int *numit, 
		  med_int * typ_ent_local, med_int * typ_geo_local,
		  med_int * typ_ent_distant, med_int * typ_geo_distant,
		  med_int *ncor)
#else
med_int 
nmsdfcsz(med_idt *fid,
	 char *maa, med_int *lon1,
	 char *jname, med_int *lon2,
	 med_int *numdt, med_int *numit,
	 med_int *typ_ent_local, med_int *typ_geo_local,
	 med_int *typ_ent_distant, med_int *typ_geo_distant,
	 med_int *ncor)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_entity_type _let = (med_entity_type) *typ_ent_local;
  med_entity_type _rnt = (med_entity_type) *typ_ent_distant;
  med_geometry_type _ltg = (med_geometry_type) *typ_geo_local;
  med_geometry_type _rtg = (med_geometry_type) *typ_geo_distant;

  _fn1 = _MED2cstring((char *) maa, (int) * lon1);
  if (!_fn1)
    return(-1); 

  _fn2 = _MED2cstring((char *) jname, (int) * lon2);
  if (!_fn2)
    return(-1); 

  _ret = (med_int) MEDsubdomainCorrespondenceSize((med_idt) *fid,
						  _fn1,        
						  _fn2,
						  (med_int) *numdt,
						  (med_int) *numit,
						  _let,
						  _ltg,
						  _rnt,
						  _rtg,
						  (med_int *) ncor);

  _MEDcstringFree(_fn1); 
  _MEDcstringFree(_fn2);

  return(_ret);   
}



#ifdef PPRO_NT
med_int 
MSDFCRR(med_idt *fid, 
		  char *maa_local,unsigned int bidon1, med_int *lon1,
		  char *jn,unsigned int bidon2, med_int *lon2, 
		  med_int *numdt, med_int *numit,
		  med_int * typ_ent_local, med_int * typ_geo_local,
		  med_int * typ_ent_distant, med_int * typ_geo_distant,
		  med_int *corrtab)
#else
med_int 
nmsdfcrr (med_idt *fid, 
	  char *maa_local,med_int *lon1,
	  char *jn,med_int *lon2, 
	  med_int *numdt, med_int *numit,
	  med_int * typ_ent_local, med_int * typ_geo_local,
	  med_int * typ_ent_distant, med_int * typ_geo_distant,
	  med_int *corrtab)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_entity_type _let = (med_entity_type) *typ_ent_local;
  med_entity_type _rnt = (med_entity_type) *typ_ent_distant;
  med_geometry_type _ltg = (med_geometry_type) *typ_geo_local;
  med_geometry_type _rtg = (med_geometry_type) *typ_geo_distant;

  _fn1 = _MED2cstring(maa_local, (int) *lon1);
  _fn2 = _MED2cstring(jn,  (int) *lon2);

  if (!_fn1 || !_fn2)
    return(-1); 

  _ret = (med_int) MEDsubdomainCorrespondenceRd((med_idt) *fid, 
						_fn1, 
						_fn2,  
						(med_int) *numdt,
						(med_int) *numit,
						_let, 
						_ltg,
						_rnt,
						_rtg,
						(med_int *) corrtab);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret);   
}



#ifdef PPRO_NT
med_int 
MSDFCSI(med_idt *fid,
                  char *maa, unsigned int bidon1, med_int *lon1,
		  char *jname, unsigned int bidon2, med_int *lon2,
		  med_int *ind, med_int *numdt, med_int *numit,
		  med_int *ncor)
#else
med_int 
nmsdfcsi(med_idt *fid,
	 char *maa, med_int *lon1,
	 char *jname, med_int *lon2,
	 med_int *ind, med_int *numdt, med_int *numit,
	 med_int *ncor)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;

  _fn1 = _MED2cstring(maa, (int) * lon1);
  if (!_fn1)
    return(-1); 

  _fn2 = _MED2cstring(jname, (int) * lon2);
  if (!_fn2)
    return(-1); 

  _ret = (med_int) MEDsubdomainComputingStepInfo((med_idt) *fid,
						 _fn1,               
						 _fn2,
						 (med_int) *ind,      
						 (med_int*) numdt,
						 (med_int*) numit,
						 (med_int *) ncor);


  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret);   
}
