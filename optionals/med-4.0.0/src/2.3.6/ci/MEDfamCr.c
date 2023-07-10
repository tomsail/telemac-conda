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

#include <string.h>
#include <stdlib.h>

med_err 
MEDfamCr(med_idt fid,char* maa,char *famille,med_int numero, 
	 med_int *attr_ident, med_int *attr_val, char *attr_desc, 
	 med_int n_attr,char *groupe, med_int n_groupe)
{
  char *  name = "MEDfamCr";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDversionLire(fid, &majeur, &mineur, &release);
  
  func = _MEDversionedApi(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy, fid, maa, famille, numero, 
	  attr_ident, attr_val, attr_desc, 
	  n_attr, groupe, n_groupe , &fret);


  return fret;
}
