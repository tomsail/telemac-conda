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
#include <string.h>
#include <stdlib.h>
#include <med_outils.h>


med_int
_MEDfieldnValue (const med_idt fid, const char * const fieldname,const med_int numdt,const med_int numit,
		 const med_entity_type entitytype, const med_geometry_type geotype,
		 char * const profilename,const int profileit,
		 const med_storage_mode storagemode,med_int * const profilesize,
		 char * const localizationname, med_int * const nintegrationpoint )
{
  char *  name = "_MEDfieldnValue";
  int     dummy=0;
  med_int fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDfileNumVersionRd(fid, &majeur, &mineur, &release);
  func = _MEDversionedApi3(name,majeur,mineur,release);

  if ( func != (MedFuncType) NULL )
    func (dummy,
	  fid,
	  fieldname,
	  numdt,
	  numit,
	  entitytype,
	  geotype,
	  profilename,
	   profileit,
	  storagemode,
	  profilesize,
	  localizationname,
	  nintegrationpoint,
	  &fret);

  return fret;
}
