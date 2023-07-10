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
- MEDequivalenceCr   
- MEDequivalenceCorrespondenceWr  
- MEDnEquivalence
- MEDequivalenceInfo
- MEDequivalenceCorrespondenceSize   
- MEDequivalenceCorrespondenceSizeInfo 
- MEDequivalenceCorrespondenceRd       
- MEDequivalenceComputingStepInfo
*/

#define nmeqfcre F77_FUNC(meqfcre,MEQFCRE)
#define nmeqfcow F77_FUNC(meqfcow,MEQFCOW)
#define nmeqfneq F77_FUNC(meqfneq,MEQFNEQ)
#define nmeqfeqi F77_FUNC(meqfeqi,MEQFEQI)
#define nmeqfcsz F77_FUNC(meqfcsz,MEQFCSZ)
#define nmeqfszi F77_FUNC(meqfszi,MEQFSZI)
#define nmeqfcsi F77_FUNC(meqfcsi,MEQFCSI)
#define nmeqfcor F77_FUNC(meqfcor,MEQFCOR)


#ifdef PPRO_NT
med_int 
MEQFCRE(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  char *eq, unsigned int bidon2, med_int *lon2, char *desc,                        
		  unsigned int bidon3, med_int *lon3)
#else
med_int 
 nmeqfcre(med_idt *fid, char *maa, med_int *lon1, char *eq, med_int *lon2, 
	  char *desc, med_int *lon3)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2, *_fn3;

  _fn1 = _MED2cstring(maa, (int) * lon1);
  _fn2 = _MED2cstring(eq,  (int) * lon2);
  _fn3 = _MED1cstring(desc, (int) * lon3,MED_COMMENT_SIZE);

  if (!_fn1 || !_fn2 || !_fn3)
    return(-1); 

  _ret = (med_int) MEDequivalenceCr((med_idt) *fid,
				    _fn1,
				    _fn2,
				    _fn3); 

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);
  _MEDcstringFree(_fn3);

  return(_ret);   
}

#ifdef PPRO_NT
med_int
MEQFCOW(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
		  char *eq, unsigned int bidon2, med_int *lon2, 
		  med_int *numdt, med_int *numit,med_int *typ_ent,med_int *typ_geo,
		  med_int *n, med_int *corr)
#else
med_int 
nmeqfcow(med_idt *fid, char *maa, med_int *lon1,
	 char *eq, med_int *lon2, 
	 med_int *numdt, med_int *numit,
	 med_int *typ_ent,med_int *typ_geo,
	 med_int *n, med_int *corr)
#endif
{
  med_int _ret;
  char    *_fn1, *_fn2;
  med_entity_type _entype = (med_entity_type) *typ_ent;
  med_geometry_type _geotype = (med_geometry_type) *typ_geo;

  _fn1 = _MED2cstring(maa, (int) * lon1);
  _fn2 = _MED2cstring(eq, (int) * lon2);

  if (!_fn1 || !_fn2)
    return(-1); 

  _ret = (med_int) MEDequivalenceCorrespondenceWr((med_idt) *fid, 
						  _fn1,
						  _fn2,
						  (med_int) *numdt,
						  (med_int) *numit,
						  _entype,
						  _geotype,
						  (med_int) *n, 
						  (med_int *) corr);
						  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret);   
}



#ifdef PPRO_NT
med_int
MEQFNEQ(med_idt *fid, char *maa, unsigned int bidon, med_int *lon1)
#else
med_int
nmeqfneq(med_idt *fid, char *maa,med_int *lon1)
#endif
{
  med_int _ret;
  char *  _fn1;

  _fn1 = _MED2cstring(maa, (int) * lon1);

  if (!_fn1)
    return(-1); 

  _ret = (med_int) MEDnEquivalence((med_idt) *fid,
				   _fn1); 

  _MEDcstringFree(_fn1);

  return(_ret); 
}




#ifdef PPRO_NT
med_int 
MEQFEQI(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  med_int *ind, char *eq, unsigned int bidon2, 
		  char *des,unsigned int bidon3, med_int *nstep, med_int *ncstcor)
#else
med_int 
nmeqfeqi(med_idt *fid, char *maa,med_int *lon1,med_int *ind, char *eq,
	 char *des, med_int *nstep, med_int *ncstcor)
#endif
{
  med_int _ret;
  char *_fn1;
  char _fs1[MED_COMMENT_SIZE+1]="";
  char _fs2[MED_NAME_SIZE+1]="";
  
  _fn1 = _MED2cstring(maa, (int) * lon1);
  if (! _fn1)
    return(-1);

  _ret = (med_int) MEDequivalenceInfo((med_idt) *fid,
				      _fn1,
				      (int) *ind, 
				      _fs2, 
				      _fs1,
				      (med_int *) nstep,
				      (med_int *) ncstcor); 

  _MEDc2fString(_fs2,eq,MED_NAME_SIZE);
  _MEDc2fString(_fs1,des,MED_COMMENT_SIZE);

  _MEDcstringFree(_fn1);

  return(_ret);
}     



#ifdef PPRO_NT
med_int
MEQFCSZ(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  char *eq, unsigned int bidon2, med_int *lon2, 
		  med_int *numdt, med_int *numit,
	          med_int *typ_ent, med_int *typ_geo)
#else
med_int
nmeqfcsz(med_idt *fid, char *maa, med_int *lon1,
	 char *eq, med_int *lon2, 
	 med_int *numdt, med_int *numit,
	 med_int *typ_ent, med_int *typ_geo)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_entity_type _entype = (med_entity_type) *typ_ent;
  med_geometry_type _geotype = (med_geometry_type) *typ_geo;
  med_int _n;

  _fn1 = _MED2cstring(maa, (int) * lon1);
  _fn2 = _MED2cstring(eq, (int) * lon2);

  if (!_fn1 || !_fn2)
    return(-1); 

  _ret = (med_int) MEDequivalenceCorrespondenceSize((med_idt) *fid,
						    _fn1,
						    _fn2, 
						    (med_int) *numdt,
						    (med_int) *numit,
						    _entype,
						    _geotype,
						    &_n); 


  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  if (_ret == 0)
    _ret = _n;

  return(_ret); 
} 



#ifdef PPRO_NT
med_int
MEQFSZI(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  char *eq, unsigned int bidon2, med_int *lon2, 
		  med_int *numdt, med_int *numit, med_int *it,
	          med_int *typ_ent, med_int *typ_geo, med_int *n)
#else
med_int
nmeqfszi(med_idt *fid, char *maa, med_int *lon1,
	 char *eq, med_int *lon2, 
	 med_int *numdt, med_int *numit,med_int *it,
	 med_int *typ_ent, med_int *typ_geo, med_int *n)
#endif
{
  med_int _ret;
  char *_fn1, *_fn2;
  med_entity_type _entype = (med_entity_type) *typ_ent;
  med_geometry_type _geotype = (med_geometry_type) *typ_geo;

  _fn1 = _MED2cstring(maa, (int) * lon1);
  _fn2 = _MED2cstring(eq, (int) * lon2);

  if (!_fn1 || !_fn2)
    return(-1); 

  _ret = (med_int) MEDequivalenceCorrespondenceSizeInfo((med_idt) *fid,
							(char *)_fn1,
							(char *)_fn2, 
							(med_int) *numdt,
							(med_int) *numit,
							(med_int) *it,
							&_entype,
							&_geotype,
							(med_int *) n); 

  *typ_ent = (med_int) _entype;
  *typ_geo = (med_int) _geotype;

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret); 
} 



#ifdef PPRO_NT
med_int
MEQFCOR(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
		  char *eq, unsigned int bidon2, med_int *lon2, 
		  med_int *numdt, med_int *numit,med_int *typ_ent,med_int *typ_geo,
		  med_int *corr)
#else
med_int 
nmeqfcor(med_idt *fid, char *maa, med_int *lon1,
	 char *eq, med_int *lon2, 
	 med_int *numdt, med_int *numit,
	 med_int *typ_ent,med_int *typ_geo,
	 med_int *corr)
#endif
{
  med_int _ret;
  char    *_fn1, *_fn2;
  med_entity_type _entype = (med_entity_type) *typ_ent;
  med_geometry_type _geotype = (med_geometry_type) *typ_geo;

  _fn1 = _MED2cstring(maa, (int) * lon1);
  _fn2 = _MED2cstring(eq, (int) * lon2);

  if (!_fn1 || !_fn2)
    return(-1); 

  _ret = (med_int) MEDequivalenceCorrespondenceRd((med_idt) *fid, 
						  _fn1,
						  _fn2,
						  (med_int) *numdt,
						  (med_int) *numit,
						  _entype,
						  _geotype,
						  (med_int *) corr);
						  
  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret);   
}


#ifdef PPRO_NT
med_int 
MEQFCSI(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
        med_int *ind, char *eq, unsigned int bidon2, med_int *lon2,
		med_int *csit, med_int *numdt, med_int *numit, med_int *ncor)
#else
med_int 
nmeqfcsi(med_idt *fid, char *maa, med_int *lon1,
	 med_int *ind, char *eq, med_int *lon2,
	 med_int *csit, med_int *numdt, med_int *numit, 
	 med_int *ncor)
#endif
{
  med_int _ret;
  char *_fn1;
  char *_fn2;
  
  _fn1 = _MED2cstring(maa, (int) * lon1);
  if (! _fn1)
    return(-1);

  _fn2 = _MED2cstring(eq, (int) * lon2);
  if (! _fn2)
    return(-1);


  _ret = (med_int) MEDequivalenceComputingStepInfo((med_idt) *fid,
						   _fn1,
						   _fn2,
						   (int) *ind,
						   (med_int *) numdt,
						   (med_int *) numit,
						   (med_int *) ncor);

  _MEDcstringFree(_fn1);
  _MEDcstringFree(_fn2);

  return(_ret);
}     
