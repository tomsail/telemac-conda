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

#define nedfjntc F77_FUNC(edfjntc,EDFJNTC)
#define nedfnjnt F77_FUNC(edfnjnt,EDFNJNT)
#define nedfjnti F77_FUNC(edfjnti,EDFJNTI)
#define nedfjnte F77_FUNC(edfjnte,EDFJNTE)
#define nedfjntl F77_FUNC(edfjntl,EDFJNTL)
#define nedfjnco F77_FUNC(edfjnco,EDFJNCO)
#define nedfjtco F77_FUNC(edfjtco,EDFJTCO)
#define nedfgnme F77_FUNC(edfgnme,EDFGNME)
#define nedfgnml F77_FUNC(edfgnml,EDFGNML)

/* creation d'un joint */

#ifdef PPRO_NT
med_int 
 EDFJNTC (med_idt *fid,
                   char *maa_lcl,  unsigned int bidon1, med_int *lon1,
                   char *jn,   unsigned int bidon2, med_int *lon2, 
                   char *desc, unsigned int bidon3, med_int *lon3,
		   med_int *dom,
		   char *maa_dist,  unsigned int bidon4, med_int *lon4
		   )
#else
med_int 
nedfjntc (med_idt *fid, 
	  char *maa_lcl,med_int *lon1,
	  char *jn,med_int *lon2, 
	  char *desc, med_int *lon3,
	  med_int *dom,
	  char *maa_dist, med_int *lon4
	  )
#endif
{
  med_int ret;
  char *fn1, *fn2, *fn3, *fn4;

  fn1 = _MED2cstring(maa_lcl, (int) * lon1);
  fn2 = _MED2cstring(jn,  (int) * lon2);
  fn3 = _MED1cstring(desc, (int) * lon3,MED_TAILLE_DESC);
  fn4 = _MED2cstring(maa_dist, (int) * lon4);

  if (!fn1 || !fn2 || !fn3 || !fn4)
    return(-1); 

  ret = (med_int) MEDjointCr( *fid,
			     (char *)fn1,(char *)fn2,
			     (char *)fn3,(med_int) *dom,
			     (char *)fn4); 

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);
  _MEDcstringFree(fn3);
  _MEDcstringFree(fn4);

  return(ret);   
}



/* Lecture du nombre de joints dans un maillage */

#ifdef PPRO_NT
med_int 
 EDFNJNT (med_idt *fid,
                   char *maa_lcl,  unsigned int bidon1, med_int *lon1,
		   med_int *n
		   )
#else
med_int 
nedfnjnt (med_idt *fid, 
	  char *maa_lcl,med_int *lon1,
	  med_int *n
	  )
#endif
{
  char *fn1;

  fn1 = _MED2cstring(maa_lcl, (int) * lon1);


  if (!fn1)
    return(-1); 

  *n = (med_int) MEDnJoint( *fid,
			   (char *)fn1);

  _MEDcstringFree(fn1);

  return(*n);   
}


/* Lecture des informations relatives à un joint */


#ifdef PPRO_NT
med_int 
 EDFJNTI (med_idt *fid,
                   char *maa_lcl,  unsigned int bidon1, med_int *lon1,
		   med_int *ind,
                   char *jn, unsigned int bidon2,  
                   char *desc, unsigned int bidon3,
		   med_int *dom,
		   char *maa_dist, unsigned int bidon4
		   )
#else
med_int 
nedfjnti (med_idt *fid, 
	  char *maa_lcl,med_int *lon1,
	  med_int *ind,
	  char *jn,
	  char *desc,
	  med_int *dom,
	  char *maa_dist
	  )
#endif
{
  med_int ret;
  char *fn1;
  char fs1[MED_TAILLE_NOM+1];   /* nom du joint OUT */
  char fs2[MED_TAILLE_DESC+1];  /* nom de la description   OUT */
  char fs3[MED_TAILLE_NOM+1];   /* nom du maillage distant OUT */


  /* nom maillage IN */
  fn1 = _MED2cstring(maa_lcl, (int) * lon1);

  if (!fn1)
    return(-1); 

  ret = (med_int) MEDjointInfo( *fid,
			       (char *)   fn1,     /* maillage local IN */
			       (med_int) *ind,      /* indice du joint dans le maillage IN */
			       (char *)fs1,        /* nom joint OUT */
			       (char *)fs2,        /* desc joint OUT */
			       (med_int * ) dom,   /* numero ss domaine distant OUT */
			       (char *)fs3);        /* nom maillage distant OUT */

  strncpy(jn,fs1,MED_TAILLE_NOM);
  _MEDfstring(jn,MED_TAILLE_NOM);
  strncpy(desc,fs2,MED_TAILLE_DESC);
  _MEDfstring(desc,MED_TAILLE_DESC);
  strncpy(maa_dist,fs3,MED_TAILLE_NOM);
  _MEDfstring(maa_dist,MED_TAILLE_NOM);


  _MEDcstringFree(fn1);


  return(ret);   
}







/* ecriture du contenu d'une correspondance */

#ifdef PPRO_NT
med_int 
 EDFJNTE (med_idt *fid, 
		   char *maa_local,unsigned int bidon1, med_int *lon1,
		   char *jn,     unsigned int bidon2, med_int *lon2, 
		   med_int *corrtab, med_int *n,
		   med_int * typ_ent_local, med_int * typ_geo_local,
		   med_int * typ_ent_distant, med_int * typ_geo_distant)
#else
med_int 
nedfjnte (med_idt *fid, 
	  char *maa_local,med_int *lon1,
	  char *jn,med_int *lon2, 
	  med_int *corrtab, med_int *n,
	  med_int * typ_ent_local, med_int * typ_geo_local,
	  med_int * typ_ent_distant, med_int * typ_geo_distant)
#endif
{
  med_int ret;
  char *fn1, *fn2;

  fn1 = _MED2cstring(maa_local, (int) *lon1);
  fn2 = _MED2cstring(jn,  (int) *lon2);

  if (!fn1 || !fn2)
    return(-1); 

  ret = (med_int) MEDjointEcr( *fid, (char *)fn1, (char *) fn2,  (med_int *) corrtab, (med_int) *n,
				  (med_entite_maillage) *typ_ent_local,   (med_geometrie_element) *typ_geo_local,
				  (med_entite_maillage) *typ_ent_distant, (med_geometrie_element) *typ_geo_distant);

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return(ret);   
}


/* lecture du contenu d'une correspondance */


#ifdef PPRO_NT
med_int 
 EDFJNTL (med_idt *fid, 
		   char *maa_local,unsigned int bidon1, med_int *lon1,
		   char *jn,     unsigned int bidon2, med_int *lon2, 
		   med_int *corrtab, med_int *n,
		   med_int * typ_ent_local, med_int * typ_geo_local,
		   med_int * typ_ent_distant, med_int * typ_geo_distant)
#else
med_int 
nedfjntl (med_idt *fid, 
	  char *maa_local,med_int *lon1,
	  char *jn,med_int *lon2, 
	  med_int *corrtab, med_int *n,
	  med_int * typ_ent_local, med_int * typ_geo_local,
	  med_int * typ_ent_distant, med_int * typ_geo_distant)
#endif
{
  med_int ret;
  char *fn1, *fn2;

  fn1 = _MED2cstring(maa_local, (int) * lon1);
  fn2 = _MED2cstring(jn,  (int) * lon2);

  if (!fn1 || !fn2 )
    return(-1); 

  ret = (med_int) MEDjointLire( *fid, (char *)fn1, (char *) fn2, (med_int *) corrtab, (med_int) *n,
			       (med_entite_maillage) *typ_ent_local,   (med_geometrie_element) *typ_geo_local,
			       (med_entite_maillage) *typ_ent_distant, (med_geometrie_element) *typ_geo_distant);

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

/* _MEDobjetsOuverts(*fid); */

  return(ret);   
}




/* Lecture du nombre de correspondances pour deux types en regard dans   un joint */


#ifdef PPRO_NT
med_int 
 EDFJNCO (med_idt *fid,
                   char *maa_lcl,  unsigned int bidon1, med_int *lon1,
                   char *jn,   unsigned int bidon2, med_int *lon2, 
		   med_int * typ_ent_local, med_int * typ_geo_local,
		   med_int * typ_ent_distant, med_int * typ_geo_distant
		   )
#else
med_int 
nedfjnco (med_idt *fid, 
	  char *maa_lcl,med_int *lon1,
	  char *jn, med_int *lon2,
	  med_int * typ_ent_local, med_int * typ_geo_local,
	  med_int * typ_ent_distant, med_int * typ_geo_distant
	  )
#endif
{
  med_int ret;
  char *fn1;
  char *fn2;                   /* nom joint IN */ 


  /* nom maillage IN */
  fn1 = _MED2cstring(maa_lcl, (int) * lon1);

  /* nom joint IN */
  fn2 = _MED2cstring(jn, (int) * lon2);

  if (!fn1)
    return(-1); 

  ret = (med_int) MEDjointnCorres( *fid,
  			          (char *)   fn1,     /* maillage local IN */
			          (char *)   fn2,    /* nom du joint IN */
				  (med_entite_maillage)   *typ_ent_local,
				  (med_geometrie_element) *typ_geo_local,
				  (med_entite_maillage)   *typ_ent_distant,
				  (med_geometrie_element) *typ_geo_distant
				  );      
		


  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);


  return(ret);   
}







/* Lecture du type des elements en regard dans un joint */


#ifdef PPRO_NT
med_int 
 EDFJTCO (med_idt *fid,
                   char *maa_lcl,  unsigned int bidon1, med_int *lon1,
                   char *jn,   unsigned int bidon2, med_int *lon2, 
		   int *ind,
		   med_int * typ_ent_local, med_int * typ_geo_local,
		   med_int * typ_ent_distant, med_int * typ_geo_distant
		   )
#else
med_int 
nedfjtco (med_idt *fid, 
	  char *maa_lcl,med_int *lon1,
	  char *jn, med_int *lon2,
	  int *ind,
	  med_int * typ_ent_local,   med_int * typ_geo_local,
	  med_int * typ_ent_distant, med_int * typ_geo_distant
	  )
#endif
{
  med_int ret;
  char *fn1;
  char *fn2;            /* nom joint IN */
  med_entite_maillage   _typ_ent_local;
  med_geometrie_element _typ_geo_local;
  med_entite_maillage   _typ_ent_distant;
  med_geometrie_element _typ_geo_distant;

  /* nom maillage IN */
  fn1 = _MED2cstring(maa_lcl, (int) * lon1);

  /* nom joint IN */
  fn2 = _MED2cstring(jn, (int) * lon2);

  if (!fn1) return(-1);

  ret = (med_int) MEDjointTypeCorres( *fid,
                                     (char *)   fn1,    /* maillage local IN */
                                     (char *)   fn2,    /* nom du joint IN */
                                     (int) *ind,        /* numero de la correspondance  IN*/
                                     &_typ_ent_local,   /* type entite local OUT */
                                     &_typ_geo_local,   /* type geometrie local OUT */
                                     &_typ_ent_distant, /* type entite distant OUT */
                                     &_typ_geo_distant  /* type geometrie distant OUT */
                                  );
   *typ_ent_local = (med_int) _typ_ent_local;
   *typ_geo_local = (med_int) _typ_geo_local;
   *typ_ent_distant = (med_int) _typ_ent_distant;
   *typ_geo_distant = (med_int) _typ_geo_distant;

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);


  return(ret);
   
}


/* ecriture d'une numerotation globale */

#ifdef PPRO_NT
med_int 
 EDFGNME (med_idt *fid, 
		   char *maa, unsigned int bidon1, med_int *lon1,
		   med_int *numtab, med_int *n,
		   med_int * typ_ent, med_int * typ_geo)
#else
med_int 
nedfgnme (med_idt *fid, 
	  char *maa,med_int *lon1,
	  med_int *numtab, med_int *n,
	  med_int * typ_ent, med_int * typ_geo)

#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring(maa, (int) * lon1);

  if (!fn1 )
    return(-1); 

  ret = (med_int) MEDglobalNumEcr( *fid, (char *)fn1, 
				  (med_int *) numtab, (med_int) *n,
				  (med_entite_maillage) *typ_ent,   (med_geometrie_element) *typ_geo);

  _MEDcstringFree(fn1);

  return(ret);   
}



/* lecture d'une numerotation globale */

#ifdef PPRO_NT
med_int 
 EDFGNML (med_idt *fid, 
		   char *maa, unsigned int bidon1, med_int *lon1,
		   med_int *numtab, med_int *n,
		   med_int * typ_ent, med_int * typ_geo)
#else
med_int 
nedfgnml (med_idt *fid, 
	  char *maa,med_int *lon1,
	  med_int *numtab, med_int *n,
	  med_int * typ_ent, med_int * typ_geo)

#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring(maa, (int) * lon1);

  if (!fn1 )
    return(-1); 

  ret = (med_int) MEDglobalNumLire( *fid, (char *)fn1, 
				  (med_int *) numtab, (med_int) *n,
				  (med_entite_maillage) *typ_ent,   (med_geometrie_element) *typ_geo);

  _MEDcstringFree(fn1);

  return(ret);   
}





