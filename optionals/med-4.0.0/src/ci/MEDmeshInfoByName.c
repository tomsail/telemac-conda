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

/**\ingroup MEDmesh
   \brief \MEDmeshInfoByNameBrief
   \param fid \fid
   \param meshname \meshname
   \param spacedim \spacedim
   \param meshdim \meshdim
   \param meshtype \meshtype
   \param description \description
   \param dtunit \dtunit
   \param sortingtype \sortingtype
   \param nstep \nstep
   \param axistype \axistype
   \param axisname \axisname
   \param axisunit \axisunit
   \retval med_err \error
   \details \MEDmeshInfoByNameDetails
*/

med_err MEDmeshInfoByName  (const med_idt            fid,
			    const char *             const meshname,
			          med_int *          const spacedim,
			          med_int *          const meshdim,
			          med_mesh_type *    const meshtype,
			          char *             const description,
			          char *             const dtunit,
			          med_sorting_type * const sortingtype,
			          med_int *          const nstep,
			          med_axis_type *    const axistype,
			          char *             const axisname,
			          char *             const axisunit)
{
  char *  name = "_MEDmeshInfoByName";
  int     dummy=0;
  med_err fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDfileNumVersionRd(fid, &majeur, &mineur, &release);

  func = _MEDversionedApi3(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy,
	  fid,
	  meshname,
	  spacedim,
	  meshdim,
	  meshtype,
	  description,
	  dtunit,
	  sortingtype,
	  nstep,
	  axistype,
	  axisname,
	  axisunit
	  , &fret);

  return fret;
}
