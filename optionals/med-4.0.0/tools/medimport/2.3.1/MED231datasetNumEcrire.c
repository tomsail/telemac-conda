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



#include "med_config.h"
#include "med_outils.h"

/* #ifdef __cplusplus */
/* } */
/* #endif */

#include "MAJ_231_232.h"


med_err _MED231datasetNumEcrire(med_idt pere,char *nom, med_type_champ type,
			     med_mode_switch interlace, med_size nbdim, med_size fixdim, 
			     med_size psize, med_mode_profil pflmod, med_size * pfltab,
			     med_int ngauss, med_size *size,  unsigned char *val)
{
  char *  name = "_MEDdatasetNumEcrire";
  int     dummy;
  med_err fret=-1;
  med_int majeur=2, mineur=3, release=1;
  MedFuncType func;

  func = _MEDversionedApi(name,majeur,mineur,release);

  if ( func != (MedFuncType) NULL )
    func  (dummy, pere, nom,  type,
	    interlace, nbdim, fixdim,
	    psize, pflmod, 0, pfltab,
	    ngauss, size,val, &fret);

  return fret;
}
