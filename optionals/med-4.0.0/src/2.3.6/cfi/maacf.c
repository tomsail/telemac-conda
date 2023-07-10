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


#define nedfdiml F77_FUNC(edfdiml,EDFDIML)
#define nedfmaac F77_FUNC(edfmaac,EDFMAAC)
#define nedfnmaa F77_FUNC(edfnmaa,EDFNMAA)
#define nedfmaai F77_FUNC(edfmaai,EDFMAAI)
#define nedfnnsl F77_FUNC(edfnnsl,EDFNNSL)
#define nedfnnse F77_FUNC(edfnnse,EDFNNSE)
#define nedfnnil F77_FUNC(edfnnil,EDFNNIL)
#define nedfnnie F77_FUNC(edfnnie,EDFNNIE)
#define nedfnnml F77_FUNC(edfnnml,EDFNNML)
#define nedfnnme F77_FUNC(edfnnme,EDFNNME)
#define nedfunvc F77_FUNC(edfunvc,EDFUNVC)
#define nedfunvl F77_FUNC(edfunvl,EDFUNVL)
#define nedfespc F77_FUNC(edfespc,EDFESPC)
#define nedfespl F77_FUNC(edfespl,EDFESPL)
#define nedfnage F77_FUNC(edfnage,EDFNAGE)
#define nedfnagl F77_FUNC(edfnagl,EDFNAGL)


#ifdef PPRO_NT
med_int
EDFDIML(med_idt *fid, char *maa, unsigned int bidon, 
                  med_int *lon)
#else
med_int
nedfdiml(med_idt *fid, char *maa,med_int *lon)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);  
  if (!fn)
    return(-1); 

  ret = (med_int) MEDdimLire(*fid,fn); 

  _MEDcstringFree(fn);

  return(ret); 
}

#ifdef PPRO_NT
med_int 
EDFMAAC(med_idt *fid , char *nom, unsigned int bidon, 
                  med_int *lon, med_int * dim, med_int *type,
		  char *desc, unsigned int bidon2, med_int *lon2)
#else
med_int 
nedfmaac(med_idt *fid , char *nom, med_int *lon, med_int * dim, med_int *type,
	 char *desc, med_int *lon2)
#endif
{
  char *fn1, *fn2;
  med_int ret;

  fn1 = _MED2cstring(nom, (int) * lon);
  fn2 = _MED2cstring(desc,(int) * lon2);
  
  if (!fn1)
    return(-1); 

  if (!fn2)
    return -1;

  ret = (med_int) MEDmaaCr(*fid, fn1, (med_int) *dim, 
			   (med_maillage) *type,fn2); 

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return(ret); 
}

#ifdef PPRO_NT
med_int 
 EDFMAAI(med_idt *fid, med_int *indice, char *maa, 
                  unsigned int bidon, med_int *dim, med_int *type,
		  char *desc, unsigned int bidon2)
#else
med_int
nedfmaai(med_idt *fid,med_int *indice, char *maa,med_int *dim, 
	 med_int *type,char *desc)
#endif
{
  med_int ret;
  char fs1[MED_TAILLE_NOM+1];
  char fs2[MED_TAILLE_DESC+1];
  med_maillage local_type;

  ret = (med_int) MEDmaaInfo(*fid,(med_int) *indice, (char *) fs1, 
			     (med_int *) dim,&local_type,
			     (char *) fs2); 

  *type = (med_int) local_type;
  
  strncpy(maa,fs1,MED_TAILLE_NOM);
  _MEDfstring(maa,MED_TAILLE_NOM);

  strncpy(desc,fs2,MED_TAILLE_DESC);
  _MEDfstring(desc,MED_TAILLE_DESC);

  return(ret); 
}

#ifdef PPRO_NT
med_int 
EDFNMAA(med_idt *fid)
#else
med_int 
nedfnmaa(med_idt *fid)
#endif
{
  med_int ret; 
  
  ret = (med_int) MEDnMaa(*fid); 

  return(ret); 
}

#ifdef PPRO_NT
med_int 
 EDFNNSL(med_idt *fid,char *maa,unsigned int bidon1, med_int *lon1)
#else
med_int
nedfnnsl(med_idt *fid,char *maa,med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring((char *)maa,(int)*lon1);

  if (!fn1)
    return -1;

  ret = (med_int) MEDnbnosoLire(*fid,(char *)fn1);

  _MEDcstringFree(fn1);

  return ret;
}

#ifdef PPRO_NT
med_int 
 EDFNNSE(med_idt *fid,char *maa, unsigned int bidon1, 
                  med_int *lon1,med_int *n)
#else
med_int
nedfnnse(med_idt *fid,char *maa,med_int *lon1,med_int *n)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring((char *)maa,(int)*lon1);
 
  if (!fn1)
    return -1;

  ret = (med_int) MEDnbnosoEcr(*fid,(char *)fn1,(med_int)*n);

  _MEDcstringFree(fn1);

  return ret;
}

#ifdef PPRO_NT
med_int 
 EDFNNIL(med_idt *fid,char *maa,unsigned bidon1, med_int *lon1)
#else
med_int
nedfnnil(med_idt *fid,char *maa,med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring((char *)maa,(int)*lon1);

  if (!fn1)
    return -1;

  ret = (med_int) MEDnbnosoLire(*fid,(char *)fn1);

  _MEDcstringFree(fn1);

  return ret;
}


#ifdef PPRO_NT
med_int 
 EDFNNIE(med_idt *fid, char *maa, unsigned int bidon1,
                  med_int *lon1,med_int *n)
#else
med_int
nedfnnie(med_idt *fid,char *maa,med_int *lon1,med_int *n)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring((char *)maa,(int)*lon1);

  if (!fn1)
    return -1;

  ret = (med_int) MEDnbnosoEcr(*fid,(char *)fn1,(med_int)*n);

  _MEDcstringFree(fn1);

  return ret;
}

#ifdef PPRO_NT
med_int 
 EDFNNML(med_idt *fid,char *maa,unsigned int bidon1,med_int *lon1)
#else
med_int
nedfnnml(med_idt *fid,char *maa,med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring((char *)maa,(int)*lon1);

  if (!fn1)
    return -1;

  ret = (med_int) MEDnbnosoLire(*fid,(char *)fn1);

  _MEDcstringFree(fn1);

  return ret;
}

#ifdef PPRO_NT
med_int 
 EDFNNME(med_idt *fid,char *maa,unsigned int bidon1,
                  med_int *lon1,med_int *n)
#else
med_int
nedfnnme(med_idt *fid,char *maa,med_int *lon1,med_int *n)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring((char *)maa,(int)*lon1);

  if (!fn1)
    return -1;

  ret = (med_int) MEDnbnosoEcr(*fid,(char *)fn1,(med_int)*n);

  _MEDcstringFree(fn1);

  return ret;
}


#ifdef PPRO_NT
med_int
 EDFUNVC(med_idt *fid , char *nom, unsigned int bidon, med_int *lon)
#else
med_int
nedfunvc(med_idt *fid , char *nom, med_int *lon)
#endif
{
  char *fn;
  med_int ret;
 
  fn = _MED2cstring(nom, (int) * lon);
 
  if (!fn)
    return(-1);
 
  ret = (med_int) MEDunvCr(*fid, fn);
 
  _MEDcstringFree(fn);
 
  return(ret);
}

#ifdef PPRO_NT
med_int
 EDFUNVL(med_idt *fid,char *nom, unsigned int bidon, med_int *lon,
                  char *nomu, unsigned int bidon1, med_int *lonu)
#else
med_int
nedfunvl(med_idt *fid,char *nom,med_int *lon,char *nomu,med_int *lonu)
#endif
{
  char *fn;
  char *fs1;
  med_int ret;

  fn = _MED2cstring(nom, (int) * lon);
  fs1 = (char *) malloc(sizeof(char)*MED_TAILLE_LNOM+1);
 
  if (!(fn&&fs1))
    return(-1);

  ret = (med_int) MEDunvLire(*fid,fn,fs1);

  strncpy(nomu,fs1,MED_TAILLE_LNOM);
  _MEDfstring(nomu,MED_TAILLE_LNOM);

  _MEDcstringFree(fn);
  free(fs1);

  return(ret);
}

#ifdef PPRO_NT
med_int
 EDFESPC(med_idt *fid, char *maa, unsigned int bidon, 
                  med_int *lon, med_int *dim)
#else
med_int
nedfespc(med_idt *fid, char *maa,med_int *lon,med_int *dim)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);
  
  if (!fn)
    return(-1); 

  ret = (med_int) MEDdimEspaceCr(*fid, fn, (med_int) *dim); 

  _MEDcstringFree(fn);

  return(ret); 
}

#ifdef PPRO_NT
med_int
 EDFESPL(med_idt *fid, char *maa, unsigned int bidon, 
                  med_int *lon)
#else
med_int
nedfespl(med_idt *fid, char *maa,med_int *lon)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);

  if (!fn)
    return(-1); 

  ret = (med_int) MEDdimEspaceLire(*fid,fn); 

  _MEDcstringFree(fn);

  return(ret); 
}

#ifdef PPRO_NT
med_int 
 EDFNAGE(med_idt *fid,char *maa, unsigned int bidon1, 
                  med_int *lon1,med_int *type)
#else
med_int
nedfnage(med_idt *fid,char *maa,med_int *lon1,med_int *type)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring((char *)maa,(int)*lon1);
 
  if (!fn1)
    return -1;

  ret = (med_int) MEDnatureGrilleEcr(*fid,(char *)fn1,(med_type_grille)*type);

  _MEDcstringFree(fn1);

  return ret;
}

#ifdef PPRO_NT
med_int 
 EDFNAGL(med_idt *fid,char *maa, unsigned int bidon1, 
                  med_int *lon1,med_int *type)
#else
med_int
nedfnagl(med_idt *fid,char *maa,med_int *lon1,med_int *type)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring((char *)maa,(int)*lon1);
 
  if (!fn1)
    return -1;

  ret = (med_int) MEDnatureGrilleLire(*fid,(char *)fn1,(med_type_grille *)type);

  _MEDcstringFree(fn1);

  return ret;
}
