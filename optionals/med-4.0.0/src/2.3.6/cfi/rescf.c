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

#include <stdio.h>

#define nedfchac  F77_FUNC(edfchac,EDFCHAC)
#define nedfchae  F77_FUNC(edfchae,EDFCHAE)
#define nedfchal  F77_FUNC(edfchal,EDFCHAL)
#define nedfchai  F77_FUNC(edfchai,EDFCHAI)
#define nedfncha  F77_FUNC(edfncha,EDFNCHA)
#define nedfnref  F77_FUNC(edfnref,EDFNREF)
#define nedfnval  F77_FUNC(edfnval,EDFNVAL)
#define nedfnpdt  F77_FUNC(edfnpdt,EDFNPDT)
#define nedfpdti  F77_FUNC(edfpdti,EDFPDTI)
#define nedfrefi  F77_FUNC(edfrefi,EDFREFI)
#define nedfliee  F77_FUNC(edfliee,EDFLIEE)
#define nedfnlie  F77_FUNC(edfnlie,EDFNLIE)
#define nedfliei  F77_FUNC(edfliei,EDFLIEI)
#define nedfnvli  F77_FUNC(edfnvli,EDFNVLI)
#define nedfliel  F77_FUNC(edfliel,EDFLIEL)
		       

#ifdef PPRO_NT
med_int
 EDFCHAC(med_idt *fid,char *cha, unsigned int bidon1, 
                  med_int *lon1, med_int *data_type, char *comp,
	          unsigned int bidon2, med_int *lon2, char *unit, 
                  unsigned int bidon3, med_int *lon3, med_int *n_comp)
#else
med_int
nedfchac(med_idt *fid,char *cha,med_int *lon1,med_int *data_type,char *comp,
	 med_int *lon2, char *unit,med_int *lon3,med_int *n_comp)
#endif
{
  med_int ret;
  char *  fn1, *fn2, *fn3;

  fn1 = _MED2cstring(cha, (int) * lon1);
  fn2 = _MED1cstring(comp, (int) * lon2,(int) *n_comp*MED_TAILLE_PNOM);
  fn3 = _MED1cstring(unit, (int) * lon3,(int) *n_comp*MED_TAILLE_PNOM); 
  
  if (!fn1 || !fn2 || !fn3)
    return(-1); 

  ret = (med_int) MEDchampCr( *fid, fn1, (med_type_champ) *data_type, 
			     (char *)fn2,(char *)fn3, (med_int) *n_comp);

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2); 
  _MEDcstringFree(fn3);

  return(ret);
}     

#ifdef PPRO_NT
med_int
 EDFCHAE(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  char *cha, unsigned int bidon2, med_int *lon2, 
		  med_int *val, med_int *interlace, med_int *n,
		  char *locname,unsigned int bidon5, med_int *lon5,
		  med_int *numco,
		  char *profil, unsigned int bidon3, med_int *lon3, med_int *pflmod,  
                  med_int *type_ent, med_int *type_geo,
		  med_int *numdt, char *dt_unit, unsigned int bidon4, med_int *lon4, 
		  med_float *dt, med_int *numo) 
#else
med_int
nedfchae(med_idt *fid, char *maa, med_int *lon1,
	 char *cha, med_int *lon2, 
	 med_int *val, med_int *interlace, med_int *n,
	 char *locname, med_int *lon5, med_int *numco,
	 char *profil, med_int *lon3,med_int *pflmod,
	 med_int *type_ent, med_int *type_geo,
	 med_int *numdt, char *dt_unit, med_int *lon4, 
	 med_float *dt, med_int *numo) 
#endif
{
#ifdef _DEBUG_
  int i;
#endif
  med_int ret;
  char  *fn1, *fn2, *fn3, *fn4, *fn5;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED2cstring(cha, (int) * lon2);
  fn3 = _MED2cstring(profil, (int) * lon3);
  fn4 = _MED2cstring(dt_unit, (int) * lon4);
  fn5 = _MED2cstring(locname, (int) * lon5);
 
  if ( !fn1 || !fn2 || !fn3 || !fn4 || !fn5 )
    return(-1); 

  
  if (!strcmp(fn3,MED_NOPFLi)) {
    _MEDcstringFree(fn3);
    fn3 = "";
  }
  
  if (!strcmp(fn5,MED_NOGAUSSi)) {
    _MEDcstringFree(fn5);
    fn5 = "";
  }
  
#ifdef _DEBUG_
  printf(" - Valeur de numo     : |%d|\n", *numo);
  printf(" - Valeur de fid      : |%ld|\n", *fid);
  printf(" - Valeur de maillage : |%s|\n", fn1);
  printf(" - Valeur de champ    : |%s|\n", fn2);
  for (i=0; i < (*n)*2; i++ )
    printf(" - Valeur de val[%d]: |%f|\n", i, ( (double *) val)[i]);
  printf(" - Valeur de interlace: |%d|\n", *interlace);
  printf(" - Valeur de nbval    : |%d|\n", *n);
  printf(" - Valeur de numco    : |%d|\n", *numco);
  printf(" - Valeur de profil   : |%s|\n", fn3);
  printf(" - Valeur de type_ent : |%d|\n", *type_ent);
  printf(" - Valeur de type_geo : |%d|\n", *type_geo);
  printf(" - Valeur de numdt    : |%d|\n", *numdt);
  printf(" - Valeur de dt_unit  : |%s|\n", fn4);
  printf(" - Valeur de de       : |%f|\n", *dt);
  printf(" - Valeur de numo     : |%d|\n", *numo);
  fflush(stdout);
#endif

  ret = (int) MEDchampEcr( *fid,(char *)fn1,
			  (char *) fn2, (void *) val,
			  (med_mode_switch) *interlace, (med_int) *n,
			  (char *) fn5,
			  (med_int) *numco, (char *) fn3, ( med_mode_profil ) *pflmod,
			  (med_entite_maillage) *type_ent, 
			  (med_geometrie_element) *type_geo,
			  (med_int) *numdt, 
			  (char *) fn4, 
			  (med_float) *dt, 
                          (med_int) *numo );


 
  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);
  _MEDcstringFree(fn4);

  if (fn3) 
    _MEDcstringFree(fn3);
  
  if (fn5) 
    _MEDcstringFree(fn5);
  
  return(ret);
}

#ifdef PPRO_NT
med_int
 EDFCHAL(med_idt *fid, char *maa, unsigned int bidon1, med_int *lon1,
                  char *cha, unsigned int bidon2, med_int *lon2, med_int *val,
		  med_int *interlace, med_int *numco, 
		  char * locname,unsigned int bidon4,
		  char *profil, unsigned int bidon3, med_int * pflmod, 
		  med_int *type_ent, med_int *type_geo,
		  med_int *numdt, med_int *numo)
#else
med_int
nedfchal(med_idt *fid, char *maa, med_int *lon1, char *cha, med_int *lon2, 
	 med_int *val,
	 med_int *interlace,med_int *numco, 
	 char * locname, char *profil, med_int * pflmod,	 
	 med_int *type_ent, med_int *type_geo,
	 med_int *numdt, med_int *numo) 
#endif
{
  med_int ret=-1;
  char * fn1, *fn2;
  char * fs3, *fs4;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED2cstring(cha, (int) * lon2);
  fs3 = (char *) malloc(sizeof(char)*MED_TAILLE_NOM+1);   
  fs4 = (char *) malloc(sizeof(char)*MED_TAILLE_NOM+1);   

  
  if (!fn1 || !fn2 || !fs3|| !fs4) goto ERROR; 

  ret = (med_int) MEDchampLire( *fid, 
			       (char *)fn1,(char *)fn2, (unsigned char *) val,
			       (med_mode_switch) *interlace,
			       (med_int) *numco,
			       (char *) fs4,(char *) fs3,(med_mode_profil) *pflmod,
			       (med_entite_maillage) *type_ent, 
			       (med_geometrie_element) *type_geo,
			       (med_int) *numdt, (med_int ) *numo);

  if (! strcmp(fs3,MED_NOPFL))
    strncpy(profil,MED_NOM_BLANC,MED_TAILLE_NOM);
  else {
    strncpy(profil,fs3,MED_TAILLE_NOM);
    _MEDfstring(profil,MED_TAILLE_NOM);
  }

  if (! strcmp(fs4,MED_NOGAUSS))
    strncpy(locname,MED_NOM_BLANC,MED_TAILLE_NOM);
  else {
    strncpy(locname,fs4,MED_TAILLE_NOM);
    _MEDfstring(locname,MED_TAILLE_NOM);
  }

  ret = 0;

 ERROR:
    
  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);
  free(fs3);
  free(fs4);

  return(ret);
}

#ifdef PPRO_NT
med_int 
 EDFCHAI(med_idt *fid, med_int *ind, char *cha, unsigned int bidon1,
                  med_int *data_type, char *comp, unsigned int bidon2, 
                  char *unit, unsigned int bidon3, med_int *n_comp)
#else
med_int 
nedfchai(med_idt *fid,med_int *ind,char *cha,
	 med_int *data_type, 
	 char *comp,char *unit,
	 med_int *n_comp)
#endif
{
  med_int ncomp;
  med_int ret; 
  char *fs1,*fs2,*fs3;
  med_type_champ datatype;

  ncomp = MEDnChamp(*fid,(int)*ind);
  
  fs1 = (char *) malloc(sizeof(char)*MED_TAILLE_NOM+1);
  fs2 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*ncomp+1);
  fs3 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*ncomp+1);   

  if (!(fs1&&fs2&&fs3))
    return -1;     

  ret = (med_int) MEDchampInfo( *fid, (int)*ind, (char *)fs1, 
			       &datatype, 
			       (char *)fs2, (char *)fs3,(med_int )*n_comp);
  
  *data_type = (med_int) datatype; 
  strncpy(cha,fs1,MED_TAILLE_NOM);
  strncpy(comp,fs2,MED_TAILLE_PNOM*ncomp);
  strncpy(unit,fs3,MED_TAILLE_PNOM*ncomp);
  _MEDfstring(cha,MED_TAILLE_NOM);
  _MEDfstring(comp,MED_TAILLE_PNOM*ncomp);
  _MEDfstring(unit,MED_TAILLE_PNOM*ncomp);
  free(fs1);
  free(fs2);
  free(fs3);

  return(ret);
}

#ifdef PPRO_NT
med_int 
 EDFNCHA(med_idt *fid, med_int *indice)
#else
med_int 
nedfncha(med_idt *fid, med_int *indice)
#endif
{
  med_int ret; 

  ret = (med_int) MEDnChamp( *fid, (int)*indice);

  return(ret);
}  

   
#ifdef PPRO_NT
med_int
 EDFNREF(med_idt *fid,
                  char *cha, unsigned int bidon1, med_int *lon1, 
                  med_int *typ_ent, med_int *typ_geo,
		  med_int *numdt, med_int *numo)
#else
med_int
nedfnref(med_idt *fid, char *cha,med_int *lon1,
	 med_int *typ_ent,med_int *typ_geo,
	 med_int *numdt, med_int *numo)
#endif
{
  med_int ret; 
  char *  fn1;

  fn1 = _MED2cstring(cha, (int) * lon1);

  if (!fn1)
    return(-1); 
 
  ret = (med_int) MEDnChampRef( *fid, (char *)fn1,
			       (med_entite_maillage) *typ_ent, 
			       (med_geometrie_element) *typ_geo,
			       (med_int) *numdt, (med_int) *numo);

  _MEDcstringFree(fn1);

  return(ret);
}

#ifdef PPRO_NT
med_int
 EDFNVAL(med_idt *fid,
                  char *cha, unsigned int bidon1, med_int *lon1, 
                  med_int *typ_ent, med_int *typ_geo,
		  med_int *numdt, med_int *numo,
		  char * maa, unsigned int bidon2, med_int *lon2, med_int * pflmod)
#else
med_int
nedfnval(med_idt *fid, char *cha,med_int *lon1,
	 med_int *typ_ent,med_int *typ_geo,
	 med_int *numdt, med_int *numo,
	 char * maa, unsigned int *lon2, med_int * pflmod)
#endif
{
  med_int ret; 
  char *  fn1;
  char *  fn2;

  fn1 = _MED2cstring(cha, (int) * lon1);
  fn2 = _MED2cstring(maa, (int) * lon2);
 
  if (!fn1 || !fn2 )
    return(-1); 

  ret = (med_int) MEDnVal( *fid, (char *)fn1,
			  (med_entite_maillage) *typ_ent, 
			  (med_geometrie_element) *typ_geo,
			  (med_int) *numdt, (med_int) *numo,(char *) fn2, (med_mode_profil) * pflmod);

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return(ret);
}

/* nouvelles routines pour MED V2.1 */

#ifdef PPRO_NT
med_int
 EDFNPDT(med_idt *fid,char *cha, unsigned int bidon1, med_int *lon1,
		  med_int * type_ent, med_int *type_geo)
#else
med_int 
nedfnpdt(med_idt *fid,char *cha, med_int *lon1,
	 med_int * type_ent, med_int *type_geo)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring(cha, (int) *lon1);

  if (!fn1)
    return(-1); 

  ret = (med_int) MEDnPasdetemps( *fid,(char *) fn1,
				 (med_entite_maillage) *type_ent,
				 (med_geometrie_element) *type_geo);

  _MEDcstringFree(fn1);

  return (ret);
}


#ifdef PPRO_NT
med_int
 EDFPDTI(med_idt *fid, char *cha, unsigned int bidon1, med_int *lon1,
		  med_int *type_ent, med_int *type_geo,
		  med_int *indice,
		  med_int *ngauss,
		  med_int *numdt  , med_int *numo,
		  char    *dt_unit, unsigned int bidon3,
		  med_float *dt,
		  char *maa, unsigned int bidon4,
		  med_int* local, med_int * nmaa)
#else
med_int 
nedfpdti(med_idt *fid,char *cha, med_int *lon1,
	 med_int *type_ent, med_int *type_geo,
	 med_int *indice,
	 med_int *ngauss, 
	 med_int *numdt,   med_int *numo,
	 char    *dt_unit, med_float *dt,
	 char    *maa,     med_int* local, med_int * nmaa)
#endif
{
  med_int ret;
  char *fn1;
  char fs1[MED_TAILLE_NOM+1];
  char fs2[MED_TAILLE_PNOM+1];
  med_booleen _local=MED_FAUX;

  fn1 = _MED2cstring(cha, (int) *lon1);

  if (!fn1)
    return(-1); 

  ret = (med_int) MEDpasdetempsInfo( *fid, (char *) fn1,
				    (med_entite_maillage) *type_ent, 
				    (med_geometrie_element) *type_geo,(int) *indice, 
				    (med_int *) ngauss, 
				    (med_int *) numdt, (med_int *) numo,
				    (char *)    fs2  , (med_float *) dt, 
				    (char *) fs1, (med_booleen *) &_local, (med_int *) nmaa);

  strncpy(maa, fs1,MED_TAILLE_NOM);
  _MEDfstring(maa,MED_TAILLE_NOM);

  strncpy(dt_unit,fs2,MED_TAILLE_PNOM);
  _MEDfstring(dt_unit,MED_TAILLE_PNOM);
  *local = (med_int) _local;

  _MEDcstringFree(fn1);

  return (ret);
}


#ifdef PPRO_NT
med_int
 EDFREFI(med_idt *fid, char *cha, unsigned int bidon1, med_int *lon1,
		  med_int *type_ent, med_int *type_geo,
		  med_int *indice,
		  med_int *numdt  , med_int *numo,
		  char *maa, unsigned int bidon4,
		  med_int * local, med_int *ngauss)
#else
med_int 
nedfrefi(med_idt *fid, char *cha, med_int *lon1,
	 med_int *type_ent, med_int *type_geo,
	 med_int *indice,
	 med_int *numdt,   med_int *numo,
	 char    *maa, 
	 med_int *local, med_int *ngauss)
#endif
{
  med_int ret;
  char *fn1;
  char fs1[MED_TAILLE_NOM+1];
  med_booleen _local=MED_FAUX;

  fn1 = _MED2cstring(cha, (int) *lon1);

  if (!fn1)
    return(-1); 

  ret = (med_int) MEDchampRefInfo( *fid, (char *) fn1,
				    (med_entite_maillage) *type_ent, 
				    (med_geometrie_element) *type_geo,(int) *indice,
				    (med_int) *numdt, (med_int) *numo,
				    (char *) fs1, (med_booleen *) &_local,(med_int *) ngauss);

  strncpy(maa, fs1,MED_TAILLE_NOM);
  *local = (med_int) _local;

  _MEDcstringFree(fn1);

  return (ret);
}


#ifdef PPRO_NT
med_int 
 EDFLIEI(med_idt *fid, med_int *indice, char *maa, 
                  unsigned int bidon, med_int *n)
#else
med_int
nedfliei(med_idt *fid,med_int *indice, char *maa,med_int *n)
#endif
{
  med_int ret;
  char fs1[MED_TAILLE_NOM+1];

  ret = (med_int) MEDlienInfo(*fid,(med_int) *indice, (char *) fs1, 
			     (med_int *) n); 
  
  strncpy(maa,fs1,MED_TAILLE_NOM);
  _MEDfstring(maa,MED_TAILLE_NOM);

  return(ret); 
}


#ifdef PPRO_NT
med_int 
 EDFNLIE(med_idt *fid)
#else
med_int 
nedfnlie(med_idt *fid)
#endif
{
  med_int ret; 
  
  ret = (med_int) MEDnLien( *fid); 

  return(ret); 
}


#ifdef PPRO_NT
med_int
 EDFLIEE(med_idt *fid, char *lienval, unsigned int bidon0, med_int *lon0,
		  char *maa, unsigned int bidon1, med_int *lon1)
#else
med_int 
nedfliee(med_idt *fid, char *lienval, med_int *lon0, char *maa, med_int *lon1)
#endif
{
  med_int ret;
  char *fn0;
  char *fn1;

  fn0 = _MED2cstring(lienval, (int) * lon0);
  fn1 = _MED2cstring(maa,     (int) * lon1);

  if (!fn0 || !fn1) return(-1); 
  
  ret = (med_int) MEDlienEcr( *fid,(char *) fn0,(char *) fn1);

  _MEDcstringFree(fn0);
  _MEDcstringFree(fn1);

  return (ret);
}




#ifdef PPRO_NT
med_int
 EDFNVLI(med_idt *fid,char *maa, unsigned int bidon1, med_int *lon1)
#else
med_int 
nedfnvli(med_idt *fid ,char *maa, med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;
  
  fn1 = _MED2cstring(maa, (int) * lon1);

  if (!fn1)
    return(-1); 
  
  ret = (med_int) MEDnValLien( *fid,(char *)fn1);

  _MEDcstringFree(fn1);

  return (ret);
}


#ifdef PPRO_NT
med_int
 EDFLIEL(med_idt *fid, char *lienval,unsigned int bidon0, med_int *n,
		  char *maa,unsigned int bidon1, med_int *lon1)
#else
med_int 
nedfliel(med_idt *fid, char *lienval, med_int *n,
	 char *maa, med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;
  char *fs1;

  fs1 = (char *) malloc(sizeof(char)*(*n)+1);  
  fn1 = _MED2cstring(maa, (int) * lon1);

  if (!(fs1&&fn1)) return(-1); 
  
  ret = (med_int) MEDlienLire( *fid,(char *) fs1,
				(char *)fn1);
  
  strncpy(lienval,fs1,*n);
  free(fs1);

  _MEDcstringFree(fn1);

  return (ret);
}





