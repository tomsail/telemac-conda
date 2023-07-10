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

#define nedffamc F77_FUNC(edffamc,EDFFAMC)
#define nedffame F77_FUNC(edffame,EDFFAME)
#define nedffaml F77_FUNC(edffaml,EDFFAML)
#define nedffami F77_FUNC(edffami,EDFFAMI)
#define nedfnfam F77_FUNC(edfnfam,EDFNFAM)
#define nedfngro F77_FUNC(edfngro,EDFNGRO)
#define nedfnatt F77_FUNC(edfnatt,EDFNATT)


#ifdef PPRO_NT
med_int
 EDFFAMC(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  char *fam, unsigned int bidon2, med_int *lon2, 
	          med_int *num, med_int *attr_ident, med_int *attr_val,
                  char *attr_desc, unsigned int bidon3, 
	          med_int *lon3, med_int *n_attr, char *groupe , 
                  unsigned int bidon4, med_int *lon4, med_int *n_groupe)
#else
med_int
nedffamc(med_idt *fid,char *maa,med_int *lon1,char *fam,med_int *lon2, 
	 med_int *num,med_int *attr_ident,med_int *attr_val,char *attr_desc, 
	 med_int *lon3,med_int *n_attr,char *groupe ,med_int *lon4, 
	 med_int *n_groupe)
#endif
{


  char *  name = "nedffamc";
  int     dummy;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDversionLire( *fid, &majeur, &mineur, &release );
  
  func = _MEDversionedApi(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy, fid, maa, lon1, fam, lon2, 
	  num, attr_ident, attr_val, attr_desc, 
	  lon3, n_attr, groupe , lon4, 
	  n_groupe , &fret);

  return fret;
}

#ifdef PPRO_NT
med_int
 EDFFAME (med_idt *fid, char *maa, unsigned int bidon, med_int *lon1,
                   med_int *fam, med_int *n, med_int *type_ent,
                   med_int *type_geo)
#else
med_int
nedffame(med_idt *fid,char *maa,med_int *lon1,med_int *fam, 
	 med_int *n, med_int *type_ent,med_int *type_geo)
#endif
{
  med_int ret;
  char *  fn1;
  
  fn1 = _MED2cstring(maa, (int) * lon1);
  if (!fn1 )
    return(-1);

  ret = (med_int) MEDfamEcr(*fid,fn1,(med_int *)fam,
			   (med_int) *n,
			   (med_entite_maillage) *type_ent,
			   (med_geometrie_element) *type_geo); 

  _MEDcstringFree(fn1);

  return(ret);
}


#ifdef PPRO_NT
med_int
 EDFFAML(med_idt *fid, char *maa, unsigned int bidon, med_int *lon1,
                  med_int *fam, med_int *n, med_int *type_ent,
                  med_int *type_geo)
#else
med_int
nedffaml(med_idt *fid, char *maa, med_int *lon1, med_int *fam, 
	 med_int *n,med_int *type_ent,med_int *type_geo)
#endif
{
  med_int ret;
  char *  fn1;
  
  fn1 = _MED2cstring(maa, (int) * lon1);
  if (!fn1 )
    return(-1);

  ret = (med_int) MEDfamLire(*fid,fn1,(med_int*) fam,(med_int) *n, 
			     (med_entite_maillage) *type_ent,
			     (med_geometrie_element) *type_geo); 

  _MEDcstringFree(fn1);

  return(ret);
}


#ifdef  PPRO_NT
int
 EDFFAMI(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  med_int *ind, char *fam, unsigned int bidon2, med_int *num,
                  med_int *attr_ident, med_int *attr_val, 
	          char *attr_desc, int unsigned bidon3, med_int *n_attr,
	          char *groupe , int unsigned bidon4, med_int *n_groupe)
#else
med_int
nedffami(med_idt *fid, char *maa,med_int *lon1,med_int *ind, 
	 char *fam,med_int *num,med_int *attr_ident,med_int *attr_val, 
	 char *attr_desc,med_int *n_attr,
	 char *groupe ,med_int *n_groupe)
#endif
{
  med_int ret;
  char * fn1,* fs1, * fs2;
  char fs3[MED_TAILLE_NOM+1];
  int natt,ngro,j;
  char str[MED_TAILLE_LNOM+1];

  fn1 = _MED2cstring(maa, (int) * lon1);
  if (!fn1)
    return(-1);

  if ((ngro = MEDnGroupe  (*fid,fn1,*ind)) < 0) return -1; 
  if ((natt = MEDnAttribut(*fid,fn1,*ind)) < 0) return -1; 

  fs1 = (char *) malloc(MED_TAILLE_DESC*natt+1);
  fs2 = (char *) malloc(MED_TAILLE_LNOM*ngro+1);

  ret = (med_int) MEDfamInfo(*fid,fn1,(int) *ind, (char *)fs3 , 
			     (med_int *)num, 
			     (med_int *) attr_ident,(med_int *) attr_val,
			     (char *) fs1,
			     (med_int *) n_attr,
			     (char *)fs2 ,(med_int *) n_groupe);

  strncpy(fam,fs3,MED_TAILLE_NOM);
  _MEDfstring(fam,MED_TAILLE_NOM);
  strncpy(attr_desc,fs1,MED_TAILLE_DESC*natt);
  _MEDfstring(attr_desc,MED_TAILLE_DESC*natt);

  for (j=0;j<ngro;j++) {
      strncpy(str,fs2+j*MED_TAILLE_LNOM,MED_TAILLE_LNOM);
      _MEDfstring(str,MED_TAILLE_LNOM);
      str[MED_TAILLE_LNOM] = '\0';
      if (j == 0) 
	      strcpy(groupe,str);
      else
	      strcat(groupe,str);
  } 
  _MEDfstring(groupe,MED_TAILLE_LNOM*ngro);
  free(fs1);
  free(fs2);
  
  _MEDcstringFree(fn1);
  
  return(ret);
}

#ifdef PPRO_NT
int
 EDFNFAM(med_idt *fid, char *maa, unsigned int bidon, med_int *lon1)
#else
med_int
nedfnfam(med_idt *fid,char *maa,med_int *lon1)
#endif
{
  med_int ret;
  char *  fn1;
  
  fn1 = _MED2cstring(maa, (int) * lon1);
  if (!fn1)
    return(-1); 

  ret = (med_int) MEDnFam((med_idt)*fid,fn1); 

  _MEDcstringFree(fn1);

  return(ret);  
}

#ifdef PPRO_NT
int
 EDFNGRO(med_idt *fid, char *maa, unsigned int bidon, med_int *lon1, med_int *indice)
#else
med_int
nedfngro(med_idt *fid,char *maa,med_int *lon1,med_int *indice)
#endif
{
  med_int ret;
  char *  fn1;
  
  fn1 = _MED2cstring(maa, (int) * lon1);
  if (!fn1)
    return(-1); 

  ret = (med_int) MEDnGroupe((med_idt)*fid,fn1,(int)*indice); 

  _MEDcstringFree(fn1);

  return(ret);  
}


#ifdef PPRO_NT
int
 EDFNATT(med_idt *fid, char *maa, unsigned int bidon, med_int *lon1, med_int *indice)
#else
med_int
nedfnatt(med_idt *fid,char *maa,med_int *lon1,med_int *indice)
#endif
{
  med_int ret;
  char *  fn1;
  
  fn1 = _MED2cstring(maa, (int) * lon1);
  if (!fn1)
    return(-1); 

  ret = (med_int) MEDnAttribut((med_idt)*fid,fn1,(int)*indice); 

  _MEDcstringFree(fn1);

  return(ret);  
}





