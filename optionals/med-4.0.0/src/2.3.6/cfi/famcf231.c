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
#include <med_versioned.h>
#include <stdlib.h>
#include <string.h>

#define nedffamc231 F77_FUNC(edffamc231,EDFFAMC231)


#ifdef PPRO_NT
void
 EDFFAMC231(int dummy,...)
#else
void
nedffamc231(int dummy,...)
#endif
{

  med_idt *fid;
  char    *maa;
  med_int *lon1;
  char    *fam;
  med_int *lon2; 
  med_int *num;
  med_int *attr_ident;
  med_int *attr_val;
  char    *attr_desc; 
  med_int *lon3;
  med_int *n_attr;
  char    *groupe ;
  med_int *lon4; 
  med_int *n_groupe;
  med_err *fret;

  med_int ret=-1;
  char *  fn1, *fn2, *fn3, *fn4;

  va_list params;
  va_start(params,dummy);

  fid        = va_arg(params,  med_idt* );
  maa        = va_arg(params,  char*    );
  lon1       = va_arg(params,  med_int* );
  fam        = va_arg(params,  char*    );
  lon2       = va_arg(params,  med_int* ); 
  num        = va_arg(params,  med_int* );
  attr_ident = va_arg(params,  med_int* );
  attr_val   = va_arg(params,  med_int* );
  attr_desc  = va_arg(params,  char*    ); 
  lon3       = va_arg(params,  med_int* );
  n_attr     = va_arg(params,  med_int* );
  groupe     = va_arg(params,  char*    );
  lon4       = va_arg(params,  med_int* ); 
  n_groupe   = va_arg(params,  med_int* );
  fret       = va_arg(params,  med_err* );


  fn1 = _MED2cstring(maa, (int) * lon1);
  fn2 = _MED1cstring(fam, (int) * lon2,MED_TAILLE_NOM);
  fn3 = _MED1cstring(attr_desc,(int) * lon3,
		     (int) *n_attr*MED_TAILLE_DESC);
  fn4 = _MED1cstring(groupe, (int) * lon4,
		     (int) *n_groupe*MED_TAILLE_LNOM);
  
  if (!fn1 || !fn2 || !fn3 || !fn4)
    goto ERROR; 

  if( MEDfamCr(*fid,fn1,fn2,(med_int) *num, 
	       (med_int *) attr_ident,
	       (med_int *) attr_val, fn3, (med_int) *n_attr, fn4, 
	       (med_int) *n_groupe) < 0 )
    goto ERROR;

  _MEDcstringFree(fn1);
  _MEDcstringFree(fn2); 
  _MEDcstringFree(fn3);
  _MEDcstringFree(fn4);

  ret=0;
 ERROR:
  va_end(params);
  *fret = ret;
  return;
 
}






