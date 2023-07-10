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

#define nedfcone F77_FUNC(edfcone,EDFCONE)
#define nedfconl F77_FUNC(edfconl,EDFCONL)
#define nedfcooe F77_FUNC(edfcooe,EDFCOOE)
#define nedfcool F77_FUNC(edfcool,EDFCOOL)
#define nedfnema F77_FUNC(edfnema,EDFNEMA)
#define nedfnome F77_FUNC(edfnome,EDFNOME)
#define nedfnoml F77_FUNC(edfnoml,EDFNOML)
#define nedfnume F77_FUNC(edfnume,EDFNUME)
#define nedfnuml F77_FUNC(edfnuml,EDFNUML)
#define nedfpgce F77_FUNC(edfpgce,EDFPGCE)
#define nedfpgcl F77_FUNC(edfpgcl,EDFPGCL)
#define nedfpygi F77_FUNC(edfpygi,EDFPYGI)
#define nedfpece F77_FUNC(edfpece,EDFPECE)
#define nedfpecl F77_FUNC(edfpecl,EDFPECL)
#define nedfpyei F77_FUNC(edfpyei,EDFPYEI)
#define nedficoe F77_FUNC(edficoe,EDFICOE)
#define nedficol F77_FUNC(edficol,EDFICOL)
#define nedfscoe F77_FUNC(edfscoe,EDFSCOE)
#define nedfscol F77_FUNC(edfscol,EDFSCOL)


#ifdef PPRO_NT
med_int 
 EDFCONE(med_idt *fid, char *maa, unsigned int bidon,
                  med_int *lon, med_int *mdim, med_int *con,med_int *mode_switch,
		  med_int *nbre, 
	          med_int *type_ent,
                  med_int *type_geo,med_int *type_con )
#else
med_int
nedfcone(med_idt *fid, char *maa, med_int *lon,med_int *mdim, 
	 med_int *con, med_int *mode_switch, med_int *nbre, 
	 med_int *type_ent,med_int *type_geo,med_int *type_con)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);
  
  if (!fn)
    return(-1); 
  
  ret = (med_int) MEDconnEcr (*fid,fn,(med_int) *mdim,
			      (med_int *)con,(med_mode_switch) *mode_switch,(med_int)*nbre, 
			      (med_entite_maillage)*type_ent,
			      (med_geometrie_element) *type_geo,
			      (med_connectivite) *type_con);

  _MEDcstringFree(fn); 

  return(ret); 
    
}


#ifdef PPRO_NT
med_int 
 EDFCONL(med_idt *fid, char *maa, unsigned int bidon, 
                  med_int *lon, med_int *mdim, med_int *con, med_int *mode_switch,
		  med_int * pfltabtmp, med_int *psizetmp,
                  med_int *type_ent,med_int *type_geo,
                  med_int *type_con )
#else
med_int 
nedfconl(med_idt *fid, char *maa, med_int *lon,med_int *mdim,med_int *con, 
	 med_int *mode_switch,
	 med_int * pfltabtmp, med_int *psizetmp,
	 med_int *type_ent,med_int *type_geo,med_int *type_con )
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);
  
  if (!fn)
    return(-1); 

  ret = (med_int) MEDconnLire(*fid, fn,(med_int) *mdim,(med_int *) con, 
			      (med_mode_switch) *mode_switch,
			      (med_int *) pfltabtmp, (med_size) *psizetmp,
			      (med_entite_maillage) *type_ent,
			      (med_geometrie_element) *type_geo,
			      (med_connectivite) *type_con); 

  _MEDcstringFree(fn);

  return(ret); 
}


#ifdef PPRO_NT
med_int
 EDFCOOE(med_idt *fid,char *maa,unsigned int bidon1,med_int *lon1,
		  med_int *mdim, med_float * coo,med_int * modcoo, med_int *n, 
	          med_int *type_rep, char *nom, 
                  unsigned int bidon2, med_int *lon2, 
	          char *unit, unsigned int bidon3, med_int *lon3)
#else
med_int
nedfcooe(med_idt *fid, char *maa, med_int *lon1, med_int *mdim,
	 med_float* coo,med_int *modcoo, med_int *n, 
	 med_int *type_rep, char *nom, med_int *lon2, 
	 char *unit, med_int *lon3)
#endif
{
  med_int ret;
  char *  fn1, *fn2, *fn3;
  med_repere type_rep_c;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED1cstring(nom, (int) * lon2,(int) *mdim*MED_TAILLE_PNOM);
  fn3 = _MED1cstring(unit, (int) * lon3,(int) *mdim*MED_TAILLE_PNOM);

  if (!fn1 || !fn2 || !fn3)
    return(-1);

  type_rep_c = (med_repere) *type_rep; 
  ret = (med_int) MEDcoordEcr(*fid, fn1, (med_int) *mdim,
			      (med_float *) coo,(med_mode_switch)*modcoo,
			      (med_int) *n,type_rep_c,
			      fn2,fn3);
  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2); 
  _MEDcstringFree(fn3);

  return(ret); 
}



#ifdef PPRO_NT
med_int
 EDFCOOL(med_idt *fid, char *maa, unsigned int bidon1, 
                  med_int *lon1, med_int *mdim, med_float * coo, med_int * modcoo, 
		  med_int *numco, med_int * pfltabtmp, med_int * psize, 
		  med_int *type_rep, char *nom, unsigned int bidon2, char *unit, 
                  unsigned int bidon3)
#else
med_int
nedfcool(med_idt *fid, char *maa,med_int *lon1,med_int *mdim,
	 med_float * coo,med_int * modcoo,
	 med_int * numco,med_int * pfltabtmp, med_int * psize, 
	 med_int *type_rep, char *nom, 
         char *unit)
#endif
{
  med_int ret;
  char * fn1,* fs1,* fs2;
  med_repere type_rep_c;
  med_size lpsize;

  lpsize = (med_size) *psize;	

  fn1 = _MED2cstring(maa, (int) * lon1);
  fs1 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*(*mdim)+1);
  fs2 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*(*mdim)+1);

  if (!(fn1&&fs1&&fs2))
    return(-1);

  ret = (med_int) MEDcoordLire(*fid, fn1, (med_int ) *mdim,
			       (med_float *) coo,(med_mode_switch)*modcoo,
			       (med_int) *numco, (med_int *) pfltabtmp, lpsize, 
			       &type_rep_c, fs1, fs2);

  *type_rep = (med_int) type_rep_c;
  strncpy(nom,fs1,MED_TAILLE_PNOM*(*mdim));
  _MEDfstring(nom ,MED_TAILLE_PNOM*(*mdim));
  strncpy(unit,fs2,MED_TAILLE_PNOM*(*mdim));
  _MEDfstring(unit,MED_TAILLE_PNOM*(*mdim));

  _MEDcstringFree(fn1);
  free(fs1);
  free(fs2);

  return(ret); 
}


#ifdef PPRO_NT
int
 EDFNOME (med_idt *fid,char *maa, unsigned int bidon1, med_int *lon1,
                   char *nom, unsigned int bidon2, med_int *lon2, 
                   med_int *n, med_int *type_ent,
                   med_int *type_geo)

#else
med_int
nedfnome(med_idt *fid,char *maa,med_int *lon1, char *nom,med_int *lon2, 
	 med_int *n,med_int *type_ent,med_int *type_geo)
#endif
{
  med_int ret;
  char * fn1, *fn2;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED1cstring(nom, (int) * lon2,(int) *n*MED_TAILLE_PNOM);

  if (!fn1 || !fn2)
    return(-1);

  ret = (med_int) MEDnomEcr(*fid, fn1, fn2, (med_int) *n, 
			    (med_entite_maillage) *type_ent,
			    (med_geometrie_element) *type_geo); 

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2);

  return(ret); 
}

#ifdef PPRO_NT
med_int
 EDFNOML(med_idt *fid,char *maa, unsigned int bidon1, med_int *lon1,
                  char *nom , unsigned int bidon2, med_int *n, 
	          med_int *type_ent, med_int *type_geo)

#else
med_int
nedfnoml(med_idt *fid,char *maa,med_int *lon1, char *nom,med_int *n, 
	 med_int *type_ent,med_int *type_geo)
#endif
{
  med_int ret;
  char * fn1,* fs1;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fs1 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*(*n)+1);

  if ( !(fn1&&fs1) )
    return(-1);

  ret = (med_int) MEDnomLire(*fid, fn1, (char *) fs1, (med_int) *n, 
			     (med_entite_maillage) *type_ent,
			     (med_geometrie_element) *type_geo); 

  strncpy(nom,fs1,MED_TAILLE_PNOM*(*n));
  _MEDfstring(nom,MED_TAILLE_PNOM*(*n));

  _MEDcstringFree(fn1);
  free(fs1);

  return(ret); 
}


#ifdef PPRO_NT
med_int
 EDFNUME(med_idt *fid,char *maa, unsigned int bidon1, med_int *lon1,
                  med_int *num, med_int *n, med_int *type_ent,
                  med_int *type_geo)
#else
med_int
nedfnume(med_idt *fid,char *maa, med_int *lon1, med_int *num, med_int *n, 
	 med_int *type_ent,med_int *type_geo)
#endif
{
  med_int ret;
  char * fn1;

  fn1 = _MED2cstring(maa, (int) * lon1);

  if (!fn1 )
    return(-1);

  ret = (med_int) MEDnumEcr(*fid, fn1, (med_int *) num, (med_int)*n, 
			    (med_entite_maillage) *type_ent,
			    (med_geometrie_element) *type_geo);

  _MEDcstringFree(fn1);

  return(ret); 
}


#ifdef PPRO_NT
int
 EDFNUML(med_idt *fid,char *maa, unsigned int bidon1, med_int *lon1,
                  med_int *num , med_int *n, med_int *type_ent,
                  med_int *type_geo)
#else
med_int
nedfnuml(med_idt *fid,char *maa, med_int *lon1, med_int *num , med_int *n, 
	 med_int *type_ent,med_int *type_geo)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring(maa, (int) * lon1);

  if (!fn1 )
    return(-1);

  ret = (med_int) MEDnumLire(*fid, fn1, (med_int *) num,(med_int)*n, 
			     (med_entite_maillage) *type_ent,
			     (med_geometrie_element) *type_geo); 

  _MEDcstringFree(fn1);

  return(ret);
}

#ifdef PPRO_NT
med_int 
 EDFNEMA (med_idt *fid, char *maa, unsigned int bidon, med_int *lon,
                   med_int *quoi, med_int *typ_ent, med_int *typ_geo,
                   med_int *typ_conn)
#else
med_int 
nedfnema(med_idt *fid, char *maa, med_int *lon, med_int *quoi, 
	 med_int *typ_ent, med_int *typ_geo,med_int *typ_conn)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring(maa, (int) * lon);

  if (!fn1 )
    return(-1);

  ret = (med_int) MEDnEntMaa(*fid, fn1, (med_table) *quoi, 
			     (med_entite_maillage) *typ_ent, 
			     (med_geometrie_element) *typ_geo,
			     (med_connectivite) *typ_conn); 

  _MEDcstringFree(fn1);

  return(ret); 
}


#ifdef PPRO_NT
med_int 
 EDFPGCE(med_idt *fid, char *maa, unsigned int bidon,med_int *lon, 
		  med_int *index, med_int *ni, med_int *con,
	          med_int *type_ent,med_int *type_con)
#else
med_int
nedfpgce(med_idt *fid, char *maa, med_int *lon,
	 med_int *index, med_int *ni, med_int *con,
	 med_int *type_ent,med_int *type_con)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);
  
  if (!fn)
    return(-1); 
  
  ret = (med_int) MEDpolygoneConnEcr(*fid,fn,(med_int *) index,(med_int) *ni,
				     (med_int *)con,(med_entite_maillage)*type_ent,
				     (med_connectivite) *type_con);

  _MEDcstringFree(fn); 

  return(ret); 
}


#ifdef PPRO_NT
med_int 
 EDFPGCL(med_idt *fid, char *maa, unsigned int bidon,med_int *lon, 
		  med_int *index, med_int *ni, med_int *con,
	          med_int *type_ent,med_int *type_con)
#else
med_int
nedfpgcl(med_idt *fid, char *maa, med_int *lon,
	 med_int *index, med_int *ni, med_int *con,
	 med_int *type_ent,med_int *type_con)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);
  
  if (!fn)
    return(-1); 

  ret = (med_int) MEDpolygoneConnLire(*fid, fn,
				      (med_int *) index,(med_int) *ni,(med_int *) con, 
				      (med_entite_maillage) *type_ent,
				      (med_connectivite) *type_con); 

  _MEDcstringFree(fn);

  return(ret); 
}

#ifdef PPRO_NT
med_int 
 EDFPYGI(med_idt *fid, char *maa, unsigned int bidon,med_int *lon, 
	          med_int *type_ent,med_int *type_con,med_int *consize)
#else
med_int
nedfpygi(med_idt *fid, char *maa, med_int *lon,
	 med_int *type_ent,med_int *type_con,med_int *consize)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);
  
  if (!fn)
    return(-1); 

  ret = (med_int) MEDpolygoneInfo(*fid, fn,
				  (med_entite_maillage) *type_ent,
				  (med_connectivite) *type_con,
				  (med_int *) consize); 

  _MEDcstringFree(fn);

  return(ret); 
}


#ifdef PPRO_NT
med_int 
 EDFPECE(med_idt *fid, char *maa, unsigned int bidon,med_int *lon, 
		  med_int *indexp, med_int *np,med_int *indexf, med_int *nf, 
		  med_int *con,med_int *type_con)
#else
med_int
nedfpece(med_idt *fid, char *maa, med_int *lon,
	 med_int *indexp, med_int *np,med_int *indexf, med_int *nf, 
	 med_int *con,med_int *type_con)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);
  
  if (!fn)
    return(-1); 
  
  ret = (med_int) MEDpolyedreConnEcr(*fid,fn,(med_int *)indexp,(med_int) *np,
				     (med_int *)indexf,(med_int) *nf,(med_int *)con,
				     (med_connectivite) *type_con);

  _MEDcstringFree(fn); 

  return(ret); 
}


#ifdef PPRO_NT
med_int 
 EDFPECL(med_idt *fid, char *maa, unsigned int bidon,med_int *lon, 
		  med_int *indexp, med_int *np, 
		  med_int * indexf,med_int *nf,
		  med_int *con,med_int *type_con)
#else
med_int
nedfpecl(med_idt *fid, char *maa, med_int *lon,
	 med_int *indexp, med_int *np, 
	 med_int *indexf,med_int *nf,
	 med_int *con,med_int *type_con)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);
  
  if (!fn)
    return(-1); 

  ret = (med_int) MEDpolyedreConnLire(*fid, fn,
				      (med_int *) indexp,(med_int) *np,
				      (med_int *) indexf,(med_int) *nf,
				      (med_int *) con, 
				      (med_connectivite) *type_con); 

  _MEDcstringFree(fn);

  return(ret); 
}

#ifdef PPRO_NT
med_int 
 EDFPYEI(med_idt *fid, char *maa, unsigned int bidon,med_int *lon, 
	          med_int *type_con,med_int *nf,med_int *consize)
#else
med_int
nedfpyei(med_idt *fid, char *maa, med_int *lon,
	 med_int *type_con,med_int *nf,med_int *consize)
#endif
{
  char *fn;
  med_int ret;

  fn = _MED2cstring(maa, (int) * lon);
  
  if (!fn)
    return(-1); 

  ret = (med_int) MEDpolyedreInfo(*fid, fn,
				  (med_connectivite) *type_con,
				  (med_int *) nf,
				  (med_int *) consize); 

  _MEDcstringFree(fn);

  return(ret);  
}


#ifdef PPRO_NT
med_int
 EDFICOE(med_idt *fid,char *maa,unsigned int bidon1,med_int *lon1,
		  med_int *mdim, med_float * indices,med_int *n, 
	          med_int *axe, char *comp, 
                  unsigned int bidon2, med_int *lon2, 
	          char *unit, unsigned int bidon3, med_int *lon3)
#else
med_int
nedficoe(med_idt *fid, char *maa, med_int *lon1, med_int *mdim,
	 med_float* indices,med_int *n, 
	 med_int *axe, char *comp, med_int *lon2, 
	 char *unit, med_int *lon3)
#endif
{
  med_int ret;
  char *  fn1, *fn2, *fn3;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED1cstring(comp, (int) * lon2,(int) *mdim*MED_TAILLE_PNOM);
  fn3 = _MED1cstring(unit, (int) * lon3,(int) *mdim*MED_TAILLE_PNOM);

  if (!fn1 || !fn2 || !fn3)
    return(-1);
 
  ret = (med_int) MEDindicesCoordEcr(*fid, fn1, (med_int) *mdim,
				     (med_float *) indices,(med_int) *n,
				     (med_int) *axe,fn2,fn3);

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2); 
  _MEDcstringFree(fn3);

  return(ret); 
}

#ifdef PPRO_NT
med_int
 EDFICOL(med_idt *fid,char *maa,unsigned int bidon1,med_int *lon1,
		  med_int *mdim, med_float * indices,med_int *n, 
	          med_int *axe, char *comp, 
                  unsigned int bidon2, med_int *lon2, 
	          char *unit, unsigned int bidon3, med_int *lon3)
#else
med_int
nedficol(med_idt *fid, char *maa, med_int *lon1, med_int *mdim,
	 med_float* indices,med_int *n, 
	 med_int *axe, char *comp, med_int *lon2, 
	 char *unit, med_int *lon3)
#endif
{
  med_int ret;
  char *  fn1, *fs1, *fs2;

  fn1 = _MED2cstring(maa, (int) * lon1);
  fs1 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM+1);
  fs2 = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM+1);

  if (!(fn1 && fs1 && fs2))
    return(-1); 
 
  ret = (med_int) MEDindicesCoordLire(*fid, fn1, (med_int) *mdim,
				      (med_float *) indices,(med_int) *n,
				      (med_int) *axe,fs1,fs2);

  strncpy(comp,fs1,MED_TAILLE_PNOM);
  _MEDfstring(comp ,MED_TAILLE_PNOM);
  strncpy(unit,fs2,MED_TAILLE_PNOM);
  _MEDfstring(unit,MED_TAILLE_PNOM);

  _MEDcstringFree(fn1);
  free(fs1);
  free(fs2);

  return(ret); 
}

#ifdef PPRO_NT
med_int
 EDFSCOE(med_idt *fid,char *maa, unsigned int bidon1, med_int *lon1,
                  med_int * mdim,med_int *structure)
#else
med_int
nedfscoe(med_idt *fid,char *maa, med_int *lon1, med_int *mdim, med_int *structure)
#endif
{
  med_int ret;
  char * fn1;

  fn1 = _MED2cstring(maa, (int) * lon1);

  if (!fn1 )
    return(-1);

  ret = (med_int) MEDstructureCoordEcr(*fid, fn1, (med_int) *mdim, (med_int *) structure);

  _MEDcstringFree(fn1);

  return(ret); 
}


#ifdef PPRO_NT
med_int
 EDFSCOL(med_idt *fid,char *maa, unsigned int bidon1, med_int *lon1,
                  med_int *mdim,med_int *structure)
#else
med_int
nedfscol(med_idt *fid,char *maa, med_int *lon1, med_int *mdim,med_int *structure)
#endif
{
  med_int ret;
  char * fn1;

  fn1 = _MED2cstring(maa, (int) * lon1);

  if (!fn1 )
    return(-1);

  ret = (med_int) MEDstructureCoordLire(*fid, fn1, (med_int) *mdim, (med_int *) structure);

  _MEDcstringFree(fn1);

  return(ret); 
}
