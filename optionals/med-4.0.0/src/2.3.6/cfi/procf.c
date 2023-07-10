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


#define nedfnpro F77_FUNC(edfnpro,EDFNPRO)
#define nedfproi F77_FUNC(edfproi,EDFPROI)
#define nedfpfle F77_FUNC(edfpfle,EDFPFLE)
#define nedfnpfl F77_FUNC(edfnpfl,EDFNPFL)
#define nedfpfll F77_FUNC(edfpfll,EDFPFLL)


#ifdef PPRO_NT
med_int 
EDFPROI(med_idt *fid, med_int *indice, char *pro, 
                  unsigned int bidon, med_int *n)
#else
med_int
nedfproi(med_idt *fid,med_int *indice, char *pro,med_int *n)
#endif
{
  med_int ret;
  char fs1[MED_TAILLE_NOM+1];

  ret = (med_int) MEDprofilInfo(*fid,(med_int) *indice, (char *) fs1, 
			     (med_int *) n); 
  
  strncpy(pro,fs1,MED_TAILLE_NOM);
  _MEDfstring(pro,MED_TAILLE_NOM);

  return(ret); 
}


#ifdef PPRO_NT
med_int 
 EDFNPRO(med_idt *fid)
#else
med_int 
nedfnpro(med_idt *fid)
#endif
{
  med_int ret; 
  
  ret = (med_int) MEDnProfil( *fid); 

  return(ret); 
}


#ifdef PPRO_NT
med_int
 EDFPFLE(med_idt *fid, med_int *pflval, med_int *n, char *nom, 
		  unsigned int bidon1, med_int *lon1)
#else
med_int 
nedfpfle(med_idt *fid, med_int *pflval, med_int *n, 
	 char *nom, med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring(nom, (int) * lon1);

  if (!fn1)
    return(-1); 
  
  ret = (med_int) MEDprofilEcr( *fid,(med_int *) pflval,
			       (med_int) *n,(char *)fn1);

  _MEDcstringFree(fn1);

  return (ret);
}




#ifdef PPRO_NT
med_int
 EDFNPFL(med_idt *fid,char *nom, unsigned int bidon1, med_int *lon1)
#else
med_int 
nedfnpfl(med_idt *fid,char *nom, med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;
  
  fn1 = _MED2cstring(nom, (int) * lon1);

  if (!fn1)
    return(-1); 
  
  ret = (med_int) MEDnValProfil( *fid,(char *)fn1);

  _MEDcstringFree(fn1);

  return (ret);
}


#ifdef PPRO_NT
med_int
 EDFPFLL(med_idt *fid, med_int *pflval,char *nom, 
		  unsigned int bidon1, med_int *lon1)
#else
med_int 
nedfpfll(med_idt *fid, med_int *pflval,
	 char *nom, med_int *lon1)
#endif
{
  med_int ret;
  char *fn1;

  fn1 = _MED2cstring(nom, (int) * lon1);

  if (!fn1)
    return(-1); 
  
  ret = (med_int) MEDprofilLire( *fid,(med_int *) pflval,
				(char *)fn1);

  _MEDcstringFree(fn1);

  return (ret);
}

