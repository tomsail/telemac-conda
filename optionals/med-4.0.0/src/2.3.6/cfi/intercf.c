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
#include <stdlib.h>
#include <string.h>

#define nedfnent F77_FUNC(edfnent,EDFNENT)
#define nedfnoel F77_FUNC(edfnoel,EDFNOEL)
#define nedfnoee F77_FUNC(edfnoee,EDFNOEE)
#define nedfelee F77_FUNC(edfelee,EDFELEE)
#define nedfelel F77_FUNC(edfelel,EDFELEL)
#define nedfg2fc F77_FUNC(edfg2fc,EDFG2FC)


#ifdef PPRO_NT
med_int 
 EDFNENT(med_idt *fid,char *maa, unsigned int bidon1,
                  med_int *lon1, med_int *typ_ent, 
                  med_int *typ_con)
#else
med_int 
nedfnent(med_idt *fid,char *maa, med_int *lon1, med_int *typ_ent, 
         med_int *typ_con)
#endif
{
  char *fn1;
  med_int ret;

  fn1 = _MED2cstring((char *)maa,(int) *lon1);

  if (! fn1)
    return -1;

  ret = (med_int) MEDnEntites(*fid, (char*)fn1,
                              (med_entite_maillage) *typ_ent, 
                              (med_connectivite) *typ_con);

  _MEDcstringFree(fn1);

  return (ret);
    
}

#ifdef PPRO_NT
med_int 
 EDFNOEL(med_idt *fid,char *maa,unsigned int bidon1,
                  med_int *lon1, med_int *mdim, med_float *coo,
	          med_int *modcoo,
	          med_int *rep, char *nomcoo, unsigned int bidon2, 
                  char *unicoo, unsigned int bidon3, 
                  char *nom, unsigned int bidon4,
                  med_int *inom,
	          med_int *num, med_int *inum, med_int *fam, med_int *nnoe)
#else
med_int 
nedfnoel(med_idt *fid,char *maa,med_int *lon1,med_int *mdim,med_float *coo,
	 med_int *modcoo,
	 med_int *rep,char *nomcoo,char *unicoo,char *nom,med_int *inom,
	 med_int *num,med_int *inum,med_int *fam,med_int *nnoe)
#endif
{
  med_int ret;
  char *fn1;
  char *fs1,*fs2,*fs3;
  med_booleen _inom=MED_FAUX,_inum=MED_FAUX;

  fn1 = _MED2cstring((char *)maa,(int) *lon1);
  fs1 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*(*mdim)+1);
  fs2 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*(*mdim)+1);
  fs3 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*(*nnoe)+1);

  if (!(fn1&&fs1&&fs2&&fs3))
     return -1;

  ret = (med_int) MEDnoeudsLire(*fid,(char*)fn1,(med_int)*mdim,
				(med_float *)coo,(med_mode_switch)*modcoo,
				(med_repere*)rep,(char*)fs1,
				(char*)fs2,(char*)fs3,
				(med_booleen*)&_inom,(med_int*)num,(med_booleen*)&_inum,
				(med_int*)fam,(med_int)*nnoe);

  strncpy(nomcoo,fs1,MED_TAILLE_PNOM*(*mdim));
  strncpy(unicoo,fs2,MED_TAILLE_PNOM*(*mdim));
  strncpy(nom   ,fs3,MED_TAILLE_PNOM*(*nnoe));
  _MEDfstring(nomcoo,MED_TAILLE_PNOM*(*mdim));
  _MEDfstring(unicoo,MED_TAILLE_PNOM*(*mdim));
  _MEDfstring(nom   ,MED_TAILLE_PNOM*(*nnoe));
  *inom=_inom;
  *inum=_inum;

  _MEDcstringFree(fn1);
  free(fs1);
  free(fs2);
  free(fs3);

  return ret;
}
 
#ifdef PPRO_NT
med_int 
 EDFNOEE(med_idt *fid, char *maa, unsigned int bidon1, 
                  med_int *lon1, med_int *mdim, med_float *coo,
	          med_int *modcoo,
	          med_int *rep, char *nomcoo, unsigned int bidon2, 
                  med_int *lon2, char *unicoo, unsigned int bidon3, 
                  med_int *lon3,
	          char *nom, unsigned int bidon4, med_int *lon4,
                  med_int *inom,med_int *num,med_int *inum,
	          med_int *fam,med_int *nnoe)
#else 
med_int 
nedfnoee(med_idt *fid,char *maa,med_int *lon1,med_int *mdim,med_float *coo,
	 med_int *modcoo,
	 med_int *rep,char *nomcoo,med_int *lon2,char *unicoo,med_int *lon3,
	 char *nom,med_int *lon4,med_int *inom,med_int *num,med_int *inum,
	 med_int *fam,med_int *nnoe)
#endif
{
  med_int ret;
  char *fn1,*fn2,*fn3,*fn4;
  med_booleen _inom=(med_booleen) *inom,_inum=(med_booleen)*inum;

  fn1 = _MED2cstring((char*)maa,(int)*lon1);
  fn2 = _MED1cstring((char*)nomcoo,(int)*lon2,(int)*mdim*MED_TAILLE_PNOM);
  fn3 = _MED1cstring((char*)unicoo,(int)*lon3,(int)*mdim*MED_TAILLE_PNOM);
  fn4 = _MED1cstring((char*)nom,(int)*lon4,(int)*nnoe*MED_TAILLE_PNOM);

  if (!(fn1&&fn2&&fn3&&fn4))
     return -1;
 
  ret = (med_int) MEDnoeudsEcr(*fid,(char*)fn1,(med_int)*mdim,
			       (med_float *)coo,(med_mode_switch)*modcoo,
			       (med_repere)*rep,(char*)fn2,
			       (char*)fn3,(char*)fn4,(med_booleen)_inom,
			       (med_int*)num,(med_booleen)_inum,
			       (med_int*)fam,(med_int)*nnoe);

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);
  _MEDcstringFree(fn3); 
  _MEDcstringFree(fn4);

  return ret;
}


#ifdef PPRO_NT
med_int 
 EDFELEE(med_idt *fid, char *maa, unsigned int bidon1,
                  med_int*lon1, med_int*mdim, med_int*conn,med_int *mode_switch,
	          char*nom, unsigned int bidon2, med_int*lon2, med_int*inom,
	          med_int *num, med_int*inum, med_int*fam, med_int*nele,
                  med_int*typent,
	          med_int*typgeo,med_int*typcon)
#else
med_int
nedfelee(med_idt *fid,char *maa,med_int*lon1,med_int*mdim,med_int*conn,med_int *mode_switch,
	 char*nom,med_int*lon2,med_int*inom,
	 med_int *num,med_int*inum,med_int*fam,med_int*nele,med_int*typent,
	 med_int*typgeo,med_int*typcon)
#endif
{
  med_int ret;
  char *fn1,*fn2;
  med_booleen _inom=(med_booleen) *inom,_inum=(med_booleen)*inum;

  fn1 = _MED2cstring((char*)maa,(int)*lon1);
  fn2 = _MED1cstring((char*)nom,(int)*lon2,*nele*MED_TAILLE_PNOM);

  if (!(fn1&&fn2))
     return -1;

  ret = (med_int) MEDelementsEcr(*fid,(char*)fn1,(med_int)*mdim,
				 (med_int*)conn,(med_mode_switch) *mode_switch,
				 (char*)fn2,(med_booleen)_inom,
				 (med_int*)num,(med_booleen)_inum,
				 (med_int*)fam,(med_int)*nele,
				 (med_entite_maillage)*typent,
				 (med_geometrie_element)*typgeo,
				 (med_connectivite)*typcon);

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return ret;
}

#ifdef PPRO_NT
med_int 
 EDFELEL(med_idt *fid,char *maa, unsigned int bidon1,
                  med_int*lon1, med_int*mdim, med_int*conn,med_int *mode_switch,
	          char*nom, unsigned int bidon2, med_int*inom,
	          med_int *num, med_int*inum, med_int*fam, med_int*nele,
                  med_int*typent, med_int*typgeo,med_int*typcon)
#else  
med_int
nedfelel(med_idt *fid,char *maa,med_int*lon1,med_int*mdim,med_int*conn,
	 med_int *mode_switch,
	 char*nom,med_int*inom,
	 med_int *num,med_int*inum,med_int*fam,med_int*nele,med_int*typent,
	 med_int*typgeo,med_int*typcon)
#endif
{
  med_int ret;
  char *fn1,*fs1;
  med_booleen _inom=MED_FAUX,_inum=MED_FAUX;

  fn1 = _MED2cstring((char*)maa,(int)*lon1);
  fs1 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*(*nele)+1);

  if (!(fn1&&fs1))
     return -1;

  ret = (med_int) MEDelementsLire(*fid,(char*)fn1,(med_int)*mdim,
				 (med_int*)conn,(med_mode_switch) *mode_switch,
				 (char*)fs1,(med_booleen*)&_inom,
				 (med_int*)num,(med_booleen*)&_inum,
				 (med_int*)fam,(med_int)*nele,
				 (med_entite_maillage)*typent,
				 (med_geometrie_element)*typgeo,
				 (med_connectivite)*typcon);

  strncpy(nom,fs1,MED_TAILLE_PNOM*(*nele));
  _MEDfstring(nom,MED_TAILLE_PNOM*(*nele));
  *inom=_inom;
  *inum=_inum;

  _MEDcstringFree(fn1);
  free(fs1);

  return ret;
} 


#ifdef PPRO_NT
med_int 
 EDFG2FC(med_idt *fid, char *maa, unsigned int bidon1,med_int *lon1, 
		  char *groupes, unsigned int bidon2,med_int *lon2, 
		  med_int *index, med_int *ngroup,med_int *entites,
		  med_int *nent, med_int *type_ent,med_int *type_geo,
		  med_int *indexgeo, med_int *ngeo)
#else 
med_int 
nedfg2fc(med_idt *fid, char *maa,med_int *lon1, 
	 char *groupes,med_int *lon2, 
	 med_int *index, med_int *ngroup,med_int *entites,
	 med_int *nent, med_int *type_ent,med_int *type_geo,
	 med_int *indexgeo, med_int *ngeo)
#endif
{
  med_int ret,i;
  char *fn1,*fn2;
  med_geometrie_element *types = (med_geometrie_element *) malloc (*ngeo*sizeof(med_geometrie_element));

  fn1 = _MED2cstring((char*)maa,(int)*lon1);
  fn2 = _MED1cstring((char*)groupes,(int)*lon2,(int)*ngroup*MED_TAILLE_LNOM);

  if (!(fn1&&fn2))
     return -1;

  for (i=0;i<*ngeo;i++)
    *(types+i) = (med_geometrie_element) *(type_geo+i);

  ret = (med_int) MEDgro2famCr(*fid,(char*)fn1,
			       (char*)fn2,(med_int*)index,
			       (med_int)*ngroup,(med_int *)entites,
			       (med_int) *nent,
			       (med_entite_maillage)*type_ent,
			       (med_geometrie_element *)types,
			       (med_int *)indexgeo,(med_int) *ngeo);

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);
  free(types);

  return ret;
}
