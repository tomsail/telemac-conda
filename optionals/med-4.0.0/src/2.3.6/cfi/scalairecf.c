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
#include <med_config.h>
#include <med_outils.h>
#include <stdlib.h>
#include <string.h>


#define nedfscac F77_FUNC(edfscac,EDFSCAC)
#define nedfscee F77_FUNC(edfscee,EDFSCEE)
#define nedfscfe F77_FUNC(edfscfe,EDFSCFE)
#define nedfnsca F77_FUNC(edfnsca,EDFNSCA)
#define nedfscai F77_FUNC(edfscai,EDFSCAI)
#define nedfnspd F77_FUNC(edfnspd,EDFNSPD)
#define nedfspdi F77_FUNC(edfspdi,EDFSPDI)
#define nedfscel F77_FUNC(edfscel,EDFSCEL)
#define nedfscfl F77_FUNC(edfscfl,EDFSCFL)
		      

#ifdef PPRO_NT
med_int
 EDFSCAC(med_idt *fid,char *scalaire, unsigned int bidon1, 
                  med_int *lon1,med_int *data_type, char *desc, 
                  unsigned int bidon2, med_int *lon2)
#else
  med_int
nedfscac(med_idt *fid,char *scalaire,med_int *lon1,med_int *data_type,
	 char *desc,med_int *lon2)
#endif
{
  med_int ret;
  med_type_champ typechamp;
  char *fn1, *fn2;

  fn1 = _MED2cstring(scalaire, (int) * lon1);
  fn2 = _MED1cstring(desc, (int) * lon2,MED_TAILLE_DESC);  

  if (!fn1 || !fn2)
    return(-1); 

  typechamp = (med_type_champ) *data_type;
  ret = (med_int) MEDscalaireCr( *fid, fn1, 
				typechamp, (char *)fn2);

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2); 

  return (ret);
}

#ifdef PPRO_NT
med_int
 EDFSCEE(med_idt *fid, 
		  char *scalaire, unsigned int bidon1, med_int *lon1, med_int *val,
		  med_int *numdt, char *dt_unit, unsigned int bidon2, med_int *lon2, 
		  med_float *dt, med_int *numo) 
#else
  med_int
nedfscee(med_idt *fid, 
	 char *scalaire, med_int *lon1, med_int *val,
	 med_int *numdt, char *dt_unit, med_int *lon2, 
	 med_float *dt, med_int *numo) 
#endif
{
  med_int ret;
  char *fn1, *fn2;

  fn1 = _MED2cstring(scalaire, (int) * lon1);
  fn2 = _MED2cstring(dt_unit, (int) * lon2);
  
  if (!fn1 || !fn2)
    return(-1); 
    
  ret = (int) MEDscalaireEntierEcr( *fid,(char *)fn1,(med_int) *val,
				   (med_int) *numdt, (char *) fn2, (med_float) *dt, 
				   (med_int) *numo );

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return(ret);
}


#ifdef PPRO_NT
med_int
 EDFSCFE(med_idt *fid, 
		  char *scalaire, unsigned int bidon1, med_int *lon1, med_float *val,
		  med_int *numdt, char *dt_unit, unsigned int bidon2, med_int *lon2, 
		  med_float *dt, med_int *numo) 
#else
  med_int
nedfscfe(med_idt *fid, 
	 char *scalaire, med_int *lon1, med_float *val,
	 med_int *numdt, char *dt_unit, med_int *lon2, 
	 med_float *dt, med_int *numo) 
#endif
{
  med_int ret;
  char *fn1, *fn2;

  fn1 = _MED2cstring(scalaire, (int) * lon1);
  fn2 = _MED2cstring(dt_unit, (int) * lon2);
  
  if (!fn1 || !fn2)
    return(-1); 
    
  ret = (int) MEDscalaireFlottantEcr( *fid,(char *)fn1,(med_float) *val,
				     (med_int) *numdt, (char *) fn2, (med_float) *dt, 
				     (med_int) *numo );

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return(ret);
}


#ifdef PPRO_NT
med_int 
 EDFNSCA(med_idt *fid)
#else
  med_int 
nedfnsca(med_idt *fid)
#endif
{
  med_int ret; 

  ret = (med_int) MEDnScalaire( *fid);

  return(ret);
}  


#ifdef PPRO_NT
med_int 
 EDFSCAI(med_idt *fid, med_int *ind, char *scalaire, unsigned int bidon1,
                  med_int *data_type, char *desc, unsigned int bidon2)
#else
  med_int 
nedfscai(med_idt *fid,med_int *ind,char *scalaire,
	 med_int *data_type,char *desc)
#endif
{
  med_int ret; 
  char *fs1,*fs2;
  med_type_champ typechamp;
  
  fs1 = (char *) malloc(sizeof(char)*MED_TAILLE_NOM+1);
  fs2 = (char *) malloc(sizeof(char)*MED_TAILLE_DESC+1);
 
  if (!(fs1&&fs2))
    return -1;     

  ret = (med_int) MEDscalaireInfo( *fid, (int)*ind, (char *)fs1, 
				  &typechamp ,(char *)fs2);
  *data_type = (med_int) typechamp;
   
  strncpy(scalaire,fs1,MED_TAILLE_NOM);
  strncpy(desc,fs2,MED_TAILLE_DESC);
  _MEDfstring(scalaire,MED_TAILLE_NOM);
  _MEDfstring(desc,MED_TAILLE_DESC);
  free(fs1);
  free(fs2);

  return(ret);
}   


#ifdef PPRO_NT
med_int
 EDFNSPD(med_idt *fid,char *scalaire, unsigned int bidon1, med_int *lon1)
#else
  med_int 
nedfnspd(med_idt *fid,char *scalaire, med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring(scalaire, (int) *lon1);

  if (!fn1)
    return(-1); 

  ret = (med_int) MEDnScalairePasdetemps( *fid,(char *) fn1);

  _MEDcstringFree(fn1);

  return (ret);
}


#ifdef PPRO_NT
med_int
 EDFSPDI(med_idt *fid, char *scalaire, unsigned int bidon1, med_int *lon1,
		  med_int *indice,med_int *numdt, char *dt_unit, unsigned int bidon2,
		  med_float *dt, med_int *numo)
#else
  med_int 
nedfspdi(med_idt *fid,char *scalaire, med_int *lon1,med_int *indice,
	 med_int *numdt,char *dt_unit, med_float *dt, med_int *numo)
#endif
{
  med_int ret;
  char *fn1;
  char fs1[MED_TAILLE_PNOM+1];

  fn1 = _MED2cstring(scalaire, (int) *lon1);

  if (!fn1)
    return(-1); 

  ret = (med_int) MEDscalairePasdetempsInfo( *fid,(char *) fn1,(int) *indice, 
					    (med_int *) numdt, (char *) fs1, 
					    (med_float *) dt, (med_int *) numo);

  strncpy(dt_unit,fs1,MED_TAILLE_PNOM);
  _MEDfstring(dt_unit,MED_TAILLE_PNOM);

  _MEDcstringFree(fn1);

  return (ret);
}

#ifdef PPRO_NT
med_int
 EDFSCEL(med_idt *fid, 
		  char *scalaire, unsigned int bidon1, med_int *lon1, med_int *val,
		  med_int *numdt,med_int *numo) 
#else
  med_int
nedfscel(med_idt *fid, 
	 char *scalaire, med_int *lon1, med_int *val,
	 med_int *numdt,med_int *numo) 
#endif
{
  med_int ret;
  char * fn1;

  fn1 = _MED2cstring(scalaire, (int) * lon1);   
  
  if (!fn1)
    return(-1); 

  ret = (med_int) MEDscalaireEntierLire( *fid,(char *)fn1,(med_int *) val,
					(med_int) *numdt, (med_int ) *numo);

  _MEDcstringFree(fn1);

  return(ret);
}


#ifdef PPRO_NT
med_int
 EDFSCFL(med_idt *fid, 
		  char *scalaire, unsigned int bidon1, med_int *lon1, med_float *val,
		  med_int *numdt,med_int *numo) 
#else
  med_int
nedfscfl(med_idt *fid, 
	 char *scalaire, med_int *lon1, med_float *val,
	 med_int *numdt, med_int *numo) 
#endif
{
  med_int ret;
  char * fn1;

  fn1 = _MED2cstring(scalaire, (int) * lon1);   
  
  if (!fn1)
    return(-1); 

  ret = (med_int) MEDscalaireFlottantLire( *fid,(char *)fn1,(med_float *) val,
					  (med_int) *numdt, (med_int ) *numo);

  _MEDcstringFree(fn1);

  return(ret);
}
