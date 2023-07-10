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

/*
 * ATTENTION : LES NOMS RENVOYES SONT LES CHAINES UTILISEES EN INTERNE
 * ELLES N'ONT PAS LA FORME MED_...
 */

med_err _MEDgetInternalGeometryTypeName(med_idt fid, char * const geotypename,med_geometry_type geotype)
{
  char *  name = "_MEDgetInternalGeometryTypeName";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur=MED_MAJOR_NUM, mineur=MED_MINOR_NUM, release=MED_RELEASE_NUM;
  MedFuncType func;

  /* Si fid == 0, on cherche l'API :
     MED_MAJOR_NUM.MED_MINOR_NUM, release.MED_RELEASE_NUM */
  if (fid != 0)
    MEDfileNumVersionRd(fid, &majeur, &mineur, &release);

  func = _MEDversionedApi3(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy,
	  geotypename,
	  geotype,
	  &fret);

  return fret;

}
