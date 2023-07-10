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

#define nedfequc F77_FUNC(edfequc,EDFEQUC)
#define nedfeque F77_FUNC(edfeque,EDFEQUE)
#define nedfequl F77_FUNC(edfequl,EDFEQUL)
#define nedfncor F77_FUNC(edfncor,EDFNCOR)
#define nedfnequ F77_FUNC(edfnequ,EDFNEQU)
#define nedfequi F77_FUNC(edfequi,EDFEQUI)


#ifdef PPRO_NT
med_int 
 EDFEQUC (med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                   char *eq, unsigned int bidon2, med_int *lon2, char *desc,                        unsigned int bidon3, med_int *lon3)
#else
med_int 
nedfequc (med_idt *fid, char *maa,med_int *lon1,char *eq,med_int *lon2, 
	  char *desc, med_int *lon3)
#endif
{
  med_int ret;
  char *fn1, *fn2, *fn3;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED2cstring(eq,  (int) * lon2);
  fn3 = _MED1cstring(desc, (int) * lon3,MED_TAILLE_DESC);

  if (!fn1 || !fn2 || !fn3)
    return(-1); 

  ret = (med_int) MEDequivCr(*fid,(char *)fn1,(char *)fn2,
			     (char *)fn3); 

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);
  _MEDcstringFree(fn3);

  return(ret);   
}

#ifdef PPRO_NT
med_int
 EDFEQUE (med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                   char *eq, unsigned int bidon2, med_int *lon2, med_int *corr,
                   med_int *n, med_int *typ_ent, 
                   med_int *typ_geo)
#else
med_int 
nedfeque (med_idt *fid, char *maa,med_int *lon1, char *eq,med_int *lon2, 
	  med_int *corr,med_int *n, 
	  med_int *typ_ent,med_int *typ_geo)
#endif
{
  med_int ret;
  char *  fn1, *fn2;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED2cstring(eq, (int) * lon2);

  if (!fn1 || !fn2)
    return(-1); 

  ret = (med_int) MEDequivEcr(*fid, (char *)fn1,(char *)fn2, 
			      (med_int *) corr, (med_int) *n, 
			      (med_entite_maillage) *typ_ent, 
			      (med_geometrie_element) *typ_geo); 

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return(ret);   
}

#ifdef PPRO_NT
med_int
 EDFEQUL (med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                   char *eq, unsigned int bidon2, med_int *lon2, 
	           med_int *corr, med_int *n, med_int *typ_ent, 
                   med_int *typ_geo)
#else
med_int
nedfequl (med_idt *fid, char *maa,med_int *lon1, char *eq,med_int *lon2, 
	  med_int *corr,med_int *n, 
	  med_int *typ_ent, med_int *typ_geo)
#endif
{
  med_int ret;
  char *  fn1, *fn2;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED2cstring(eq, (int) * lon2);

  if (!fn1 || !fn2)
    return(-1); 

  ret = (med_int) MEDequivLire(*fid, (char *)fn1,(char *)fn2, 
			       (med_int *) corr, (med_int) *n, 
			       (med_entite_maillage) *typ_ent, 
			       (med_geometrie_element) *typ_geo); 

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return(ret);   
}

#ifdef PPRO_NT
med_int
 EDFNCOR(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  char *eq, unsigned int bidon2, med_int *lon2, 
	          med_int *typ_ent, med_int *typ_geo)
#else
med_int
nedfncor(med_idt *fid, char *maa,med_int *lon1, char *eq, med_int *lon2, 
	 med_int *typ_ent, med_int *typ_geo)
#endif
{
  med_int ret;
  char *  fn1, *fn2;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED2cstring(eq, (int) * lon2);

  if (!fn1 || !fn2)
    return(-1); 

  ret = (med_int) MEDnCorres(*fid,(char *)fn1,(char *)fn2, 
			     (med_entite_maillage) *typ_ent, 
			     (med_geometrie_element) *typ_geo); 

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return(ret); 
} 


#ifdef PPRO_NT
med_int
 EDFNEQU(med_idt *fid, char *maa, unsigned int bidon, med_int *lon1)
#else
med_int
nedfnequ(med_idt *fid, char *maa,med_int *lon1)
#endif
{
  med_int ret;
  char *  fn1;

  fn1 = _MED2cstring(maa, (int) * lon1);

  if (!fn1)
    return(-1); 

  ret = (med_int) MEDnEquiv(*fid,(char *)fn1); 

  _MEDcstringFree(fn1);

  return(ret); 
}



#ifdef PPRO_NT
med_int 
 EDFEQUI(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  med_int *ind, char *eq, unsigned int bidon2, char *des,                          unsigned int bidon3)
#else
med_int 
nedfequi(med_idt *fid, char *maa,med_int *lon1,med_int *ind, char *eq,
	 char *des)
#endif
{
  med_int ret;
  char *  fn1, * fs2;
  char fs1[MED_TAILLE_DESC+1];
  fn1 = _MED2cstring(maa, (int) * lon1);
  fs2 = malloc(sizeof(char)*MED_TAILLE_NOM); 
  if (!(fn1&&fs2))
    return(-1);

  ret = (med_int) MEDequivInfo(*fid,(char *)fn1,(int) *ind, 
			       (char *)fs2, (char *)fs1); 

  strncpy(eq,fs2,MED_TAILLE_NOM);
  _MEDfstring(eq,MED_TAILLE_NOM);
  strncpy(des,fs1,MED_TAILLE_DESC);
  _MEDfstring(des,MED_TAILLE_DESC);

  _MEDcstringFree(fn1);
  free(fs2);

  return(ret);
}     

