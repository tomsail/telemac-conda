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

/**\ingroup MEDfield
  \brief \MEDfieldCrBrief
  \param fid \fid
  \param fieldname \fieldname
  \param fieldtype \fieldtype
  \param ncomponent \ncomponent
  \param componentname \componentname
  \param componentunit \componentunit
  \param dtunit \dtunit
  \param meshname \meshname
  \retval med_err \error
  \details \MEDfieldCrDetails

  \remarks
  \MEDfieldCrRem
 */

med_err
MEDfieldCr( const med_idt fid,
	    const char * const fieldname,
	    const med_field_type fieldtype,
	    const med_int ncomponent,
	    const char * const componentname,
	    const char * const componentunit,
	    const char * const dtunit,
	    const char * const meshname)
{
  char *  name = "_MEDfieldCr";
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
	  fieldtype,
	  ncomponent,
	  componentname,
	  componentunit,
	  dtunit,
	  meshname,
	  &fret);

  return fret;
}
