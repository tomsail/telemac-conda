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

extern void *MedVersionedApiF;
extern void f77ApiIsSet(void * obj);

#define nedfouvr F77_FUNC(edfouvr,EDFOUVR)
#define nedfferm F77_FUNC(edfferm,EDFFERM)
#define nedflfde F77_FUNC(edflfde,EDFLFDE)
#define nedffien F77_FUNC(edffien,EDFFIEN)
#define nedffide F77_FUNC(edffide,EDFFIDE)
#define nedffoco F77_FUNC(edffoco,EDFFOCO)
#define nedfveco F77_FUNC(edfveco,EDFVECO)
#define nedfveli F77_FUNC(edfveli,EDFVELI)
#define nedfvedo F77_FUNC(edfvedo,EDFVEDO)
#define nedfmont F77_FUNC(edfmont,EDFMONT)
#define nedfdemo F77_FUNC(edfdemo,EDFDEMO)

#ifdef PPRO_NT
med_idt
 EDFOUVR (char *nom, unsigned int bidon, med_int * acces, 
                   med_int *lon)
#else
med_idt
nedfouvr (char *nom,med_int * acces,med_int *lon)
#endif
{
  char *fn;
  med_idt ret;

  /* Cette méthode a pour unique but de forcer la construction
   de l'objet MedVersionedApiF et donc la mise à jour de MedVersionedApi
  avec les APis fortran. Certains compilateurs suppriment le symbole MedVersionedApiF
  croyant qu'il nest pas utilisé et la mise à jour de MedVersionedApi n'est
  alors pas effectuée.*/
  f77ApiIsSet(MedVersionedApiF);

  fn = _MED2cstring(nom, (int) * lon);
  if (!fn) return(-1);

  ret = MEDouvrir(fn,(med_mode_acces) *acces); 

  _MEDcstringFree(fn);

  return(ret); 
}

#ifdef PPRO_NT
med_int
 EDFFERM(med_idt * fid)
#else
med_int
nedfferm(med_idt * fid)
#endif
{
  med_int ret;
  ret = (med_int) MEDfermer(* fid);

  return(ret);
}

#ifdef PPRO_NT
med_int
 EDFLFDE(med_idt *fid)
#else
med_int
nedflfde (med_idt *fid)
#endif
{
  med_int longueur;

  longueur = MEDlFichDes(*fid);

  return longueur;
}

#ifdef PPRO_NT
med_int
 EDFFIEN(med_idt *fid,med_int *quoi,char *str, unsigned int bidon1)
#else
med_int
nedffien (med_idt *fid,med_int *quoi,char *str)
#endif
{
  med_int ret;
  med_int longueur;
  char * fs1;

  /* Solution temporaire */
  longueur = MEDlFichDes(*fid);
/*   ISCRUTE(longueur); */
  fs1 = (char *) malloc(sizeof(char)*longueur+1);
  
  ret = (med_int) MEDfichEntete(*fid,(med_fich_info) *quoi,(char *) fs1);

  strncpy(str,fs1,longueur);
  _MEDfstring(str,longueur);
  free(fs1);
  
  return ret;
}

#ifdef PPRO_NT
int
 EDFFIDE(med_idt *fid,char *des, unsigned int bidon1,med_int *lon1)
#else
med_int
nedffide(med_idt *fid,char *des,med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED1cstring((char *)des,(int )*lon1,MED_TAILLE_DESC);

  ret = MEDfichDesEcr(*fid,(char*)fn1);

  _MEDcstringFree(fn1);

  return ret;
}
  

#ifdef PPRO_NT
med_int
 EDFFOCO (char *nom, unsigned int bidon, med_int *lon)
#else
med_int
nedffoco (char *nom,med_int *lon)
#endif
{
  char *fn;
  int ret;

  fn = _MED2cstring(nom, (int) * lon);
  if (!fn)
	return(-1);

  ret = (med_int) MEDformatConforme(fn); 

  _MEDcstringFree(fn);

  return(ret); 
}

#ifdef PPRO_NT
med_int
 EDFVECO (char *nom, unsigned int bidon, med_int *lon)
#else
med_int
nedfveco (char *nom,med_int *lon)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(nom, (int) * lon);
  if (!fn)
	return(-1);

  ret = (med_int) MEDversionConforme(fn); 

  _MEDcstringFree(fn);

  return(ret); 
}

#ifdef PPRO_NT
med_int
 EDFVELI (med_idt *fid, med_int *maj, med_int *min, med_int *rel)
#else
med_int
nedfveli (med_idt *fid, med_int *maj, med_int *min, med_int *rel)
#endif
{
  med_int ret;

  ret = (med_int) MEDversionLire(*fid, (med_int *) maj, (med_int *) min, (med_int *) rel); 

  return(ret); 
}

#ifdef PPRO_NT
med_int
 EDFVEDO (med_int *maj, med_int *min, med_int *rel)
#else
med_int
nedfvedo (med_int *maj, med_int *min, med_int *rel)
#endif
{
  med_int ret;

  ret = 0;

  MEDversionDonner((med_int *) maj, (med_int *) min, (med_int *) rel); 

  return(ret); 
}

#ifdef PPRO_NT
med_idt
 EDFMONT (med_idt *fid, char *acces, unsigned int bidon, med_int *lon, med_int *type)
#else
med_idt
nedfmont (med_idt *fid, char *acces, med_int *lon, med_int *type)
#endif
{
  char *fn;
  med_idt ret;

  fn = _MED2cstring(acces, (int) * lon);
  if (!fn)
    return(-1);

  ret = (med_idt) MEDmonter(*fid,(const char *) fn, (med_type_donnee) *type); 

  _MEDcstringFree(fn);

  return(ret); 
}


#ifdef PPRO_NT
med_int
 EDFDEMO (med_idt *fid, med_idt *mid, med_int *type)
#else
med_int
nedfdemo (med_idt *fid, med_idt *mid, med_int *type)
#endif
{
  int ret;

  ret = (med_int) MEDdemonter(*fid,*mid, (med_type_donnee) *type); 

  return (ret); 
}
