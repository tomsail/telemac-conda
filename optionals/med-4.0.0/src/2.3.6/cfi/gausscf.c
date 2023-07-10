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


#define nedfngau F77_FUNC(edfngau,EDFNGAU)
#define nedfgaui F77_FUNC(edfgaui,EDFGAUI)
#define nedfgaue F77_FUNC(edfgaue,EDFGAUE)
#define nedfgaul F77_FUNC(edfgaul,EDFGAUL)

#ifdef PPRO_NT
med_int 
 EDFGAUI(med_idt *fid, med_int *indice, char * locname, unsigned int bidon,
		  med_int *typ_geo, med_int *ngauss)
#else
med_int
nedfgaui(med_idt *fid,med_int *indice, char * locname,
	 med_int *typ_geo, med_int *ngauss)
#endif
{
  med_int ret;
  char fs1[MED_TAILLE_NOM+1];
  med_geometrie_element type;

  ret = (med_int) MEDgaussInfo((med_idt)*fid,(med_int) *indice, (char *) fs1, 
			       &type, (med_int *) ngauss); 
  
  strncpy(locname,fs1,MED_TAILLE_NOM);
  _MEDfstring(locname,MED_TAILLE_NOM);
  *typ_geo = (med_int) type;

  return(ret); 
}


#ifdef PPRO_NT
med_int 
 EDFNGAU(med_idt *fid)
#else
med_int 
nedfngau(med_idt *fid)
#endif
{
  med_int ret; 
  
  ret = (med_int) MEDnGauss(*fid); 

  return(ret); 
}


#ifdef PPRO_NT
med_int
 EDFGAUE(med_idt *fid,med_int *typ_geo,
		  med_float *refcoo, med_int *mode_coo,
		  med_int *ngauss, med_float *gscoo, med_float *wg,
		  char *locname, unsigned int bidon1, med_int *lon1)
#else
med_int 
nedfgaue(med_idt *fid,med_int *typ_geo,
		  med_float *refcoo, med_int *mode_coo,
		  med_int *ngauss, med_float *gscoo, med_float *wg,
		  char *locname, med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;
  med_geometrie_element type = (med_geometrie_element) *typ_geo;
  med_mode_switch mode = (med_mode_switch) *mode_coo;

  fn1 = _MED2cstring(locname, (int) * lon1);

  if (!fn1) return(-1); 
  
  ret = (med_int) MEDgaussEcr( *fid, type,
			       (med_float *) refcoo, mode,
			       (med_int) *ngauss, (med_float *) gscoo, (med_float *) wg,
			       (char *) fn1);

  _MEDcstringFree(fn1);

  return (ret);
}


#ifdef PPRO_NT
med_int
 EDFGAUL(med_idt * fid, med_float *refcoo, med_float *gscoo, med_float * wg,
		  med_int *mode_coo, char *locname, unsigned int bidon1, med_int *lon1)
#else
med_int 
nedfgaul(med_idt * fid, med_float *refcoo, med_float *gscoo, med_float * wg,
	 med_int *mode_coo, char *locname, med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;
  med_mode_switch mode = (med_mode_switch) *mode_coo;

  fn1 = _MED2cstring(locname, (int) * lon1);

  if (!fn1) return(-1); 
  
  ret = (med_int) MEDgaussLire( *fid, (med_float *) refcoo, (med_float *)gscoo,
				(med_float *) wg, mode,
				(char *) fn1);

  _MEDcstringFree(fn1);

  return (ret);
}

