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

/**\ingroup MEDmesh
  \brief \MEDmeshCrBrief
  \param fid \fid
  \param meshname \meshname
  \param spacedim \spacedim
  \param meshdim \meshdim
  \param meshtype \meshtype
  \param description \description
  \param dtunit \dtunit
  \param sortingtype \sortingtype
  \param axistype \axistype
  \param axisname \axisname
  \param axisunit \axisunit
  \retval med_err \error
  \details \MEDmeshCrDetails
 */


med_err
MEDmeshCr(const med_idt fid,
	  const char * const meshname,
	  const med_int spacedim,
	  const med_int meshdim,
	  const med_mesh_type meshtype,
	  const char * const description,
	  const char * const dtunit,
	  const med_sorting_type sortingtype,
	  const med_axis_type axistype,
	  const char * const axisname,
	  const char * const axisunit)
{

  return  _MEDmeshCr(fid, MED_MESH_GRP , meshname, spacedim, meshdim,  meshtype,
		     description, dtunit, sortingtype, axistype, axisname, axisunit);

}
