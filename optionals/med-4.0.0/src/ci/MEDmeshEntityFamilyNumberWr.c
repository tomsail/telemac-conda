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
  \brief \MEDmeshEntityFamilyNumberWrBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param geotype \geotype
  \param nentity \nentity
  \param number \number
  \retval med_err \error
  \details \MEDmeshEntityFamilyNumberWrDetails
 */

med_err MEDmeshEntityFamilyNumberWr(const med_idt               fid,
				    const char*  const          meshname,
				    const med_int               numdt,
				    const med_int               numit,
				    const med_entity_type       entitype,
				    const med_geometry_type     geotype,
				    const med_int               nentity,
				    const med_int * const       number)
{
  med_err _ret = -1;
  med_entity_type _entitytype=entitype;

  if ( entitype == MED_NODE_ELEMENT ) _entitytype=MED_NODE ;

  _ret= _MEDmeshAdvancedWr(fid,
			   meshname,
			   MED_FAMILY_NUMBER,
			   MED_NO_NAME,
			   MED_INTERNAL_UNDEF,
			   numdt,
			   numit,
			   MED_UNDEF_DT, /*Si la s�quence de calcul n'existe pas, ce champ sera initialisé correctement par d'autres appels.*/
			   _entitytype,
			   geotype,
			   MED_NODAL,
			   MED_UNDEF_STMODE,
			   MED_SAME_PROFILE_INTERNAL,
			   MED_FULL_INTERLACE,  /*Pas d'utilit� ici, car nconstituent==1*/
			   MED_ALL_CONSTITUENT,
			   NULL,
			   nentity,
			   number);
  return _ret;

}
