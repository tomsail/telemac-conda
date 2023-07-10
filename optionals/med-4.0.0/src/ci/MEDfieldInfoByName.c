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
#include <med.h>
#include "med_outils.h"
#include <string.h>

/**\ingroup MEDfield
  \brief \MEDfieldInfoByNameBrief
  \param fid \fid
  \param fieldname \fieldname
  \param meshname \meshname
  \param localmesh \localmesh
  \param fieldtype \fieldtype
  \param componentunit \componentunit
  \param componentname \componentname
  \param dtunit \dtunit
  \param ncstp \ncstp
  \retval med_err \error
  \details \MEDfieldInfoByNameDetails
 */

med_err MEDfieldInfoByName(const med_idt fid,
			   const char * const fieldname,
			   char * const meshname,
			   med_bool * const localmesh,
			   med_field_type * const fieldtype,
			   char * const componentname,
			   char * const componentunit,
			   char * const dtunit,
			   med_int * const ncstp)
{
  char *  name = "_MEDfieldInfoByName";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDfileNumVersionRd(fid, &majeur, &mineur, &release);
  func = _MEDversionedApi3(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy,
	  fid,
	  fieldname,
	  meshname,
	  localmesh,
	  fieldtype,
	  componentname,
	  componentunit,
	  dtunit,
	  ncstp,
	  &fret);

  return fret;
}
