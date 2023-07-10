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

/**\ingroup MEDsupportMesh
  \brief \MEDsupportMeshCrBrief
  \param fid         \fid
  \param supportmeshname    \supportmeshname
  \param spacedim    \spacedim
  \param meshdim     \meshdim
  \param description \description
  \param axistype    \axistype
  \param axisname    \axisname
  \param axisunit    \axisunit
  \retval med_err    \error
  \details \MEDsupportMeshCrDetails
  \see MEDstructElementCr
  \see MEDmeshElementConnectivityWr
  \see MEDmeshEntityFamilyNumberWr
  \see MEDmeshEntityNumberWr
  \see MEDmeshEntityNameWr
 */

med_err
MEDsupportMeshCr(const med_idt        fid,
		 const char*    const supportmeshname,
		 const med_int        spacedim,
		 const med_int        meshdim,
		 const char*   const  description,
		 const med_axis_type  axistype,
		 const char*   const  axisname,
		 const char*   const  axisunit
		 )
{
  return  _MEDmeshCr(fid, MED_MESH_SUPPORT_GRP , supportmeshname, spacedim, meshdim,  MED_UNSTRUCTURED_MESH,
		     description, "", MED_SORT_DTIT, axistype, axisname, axisunit);

}
