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
  \brief \MEDmeshElementConnectivityAdvancedRdBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param geotype \geotype
  \param cmode \cmode
  \param filter \filter
  \param connectivity \connectivity
  \retval med_err \error
  \details \MEDmeshElementConnectivityAdvancedRdDetails
 */

med_err MEDmeshElementConnectivityAdvancedRd(const med_idt               fid,
					     const char*  const          meshname,
					     const med_int               numdt,
					     const med_int               numit,
					     const med_entity_type       entitype,
					     const med_geometry_type     geotype,
					     const med_connectivity_mode cmode,
					     const med_filter * const    filter,
					     med_int* const  connectivity)
{

 return _MEDmeshAdvancedRd(fid,
			   meshname,
			   MED_CONNECTIVITY,
			   MED_NO_NAME,
			   MED_INTERNAL_UNDEF,
			   numdt,
			   numit,
			   entitype,
			   geotype,
			   cmode,
			   MED_UNDEF_STMODE,
			   MED_NO_PROFILE,
			   MED_UNDEF_INTERLACE,
			   MED_ALL_CONSTITUENT,
			   filter,
			   (unsigned char * const) connectivity);

}
