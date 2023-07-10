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
  \brief \MEDmeshEntityNumberRdBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param geotype \geotype
  \param number \number
  \retval med_err \error
  \details \MEDmeshEntityNumberRdDetails
 */

med_err MEDmeshEntityNumberRd(const med_idt               fid,
			      const char*  const          meshname,
			      const med_int               numdt,
			      const med_int               numit,
			      const med_entity_type       entitype,
			      const med_geometry_type     geotype,
			      med_int * const             number)
{
  med_err _ret = -1;
  med_entity_type _entitytype=entitype;

  if ( entitype == MED_NODE_ELEMENT ) _entitytype=MED_NODE ;

  _ret= _MEDmeshAdvancedRd(fid,
			   meshname,
			   MED_NUMBER,
			   MED_NO_NAME,
			   MED_INTERNAL_UNDEF,
			   numdt,
			   numit,
			   _entitytype,
			   geotype,
			   MED_NODAL,
			   MED_UNDEF_STMODE,
			   MED_NO_PROFILE,
			   MED_FULL_INTERLACE,  /*Pas d'utilité ici, car nconstituent==1*/
			   MED_ALL_CONSTITUENT,
			   NULL,
			   (unsigned char *const) number);
  return _ret;

}
