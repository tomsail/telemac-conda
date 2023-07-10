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
  \brief \MEDmeshEntityAttributeAdvancedBrief{de lire}
  \param fid \fid
  \param meshname \meshname
  \param datatype \datatype
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param geotype \geotype
  \param filter \filter
  \param attval \attributevalue
  \retval med_err \error
  \details \MEDmeshEntityAttributeAdvancedDetails{de lire}
  \remarks
  \MEDmeshEntityAttributeAdvancedRem
 */

med_err MEDmeshEntityAttributeAdvancedRd(const med_idt               fid,
					 const char*  const          meshname,
					 const med_data_type         datatype,
					 const med_int               numdt,
					 const med_int               numit,
					 const med_entity_type       entitype,
					 const med_geometry_type     geotype,
					 const med_filter * const    filter,
					 void * const                attval)
{

  med_err _ret=-1;

  switch (datatype) {
  case MED_NAME:
  case MED_NUMBER:
  case MED_FAMILY_NUMBER:
    break;
  default:
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_PARAMETER,"");
    ISCRUTE_int(datatype);
    goto ERROR;
  }

  _ret= _MEDmeshAdvancedRd(fid,
			   meshname,
			   datatype,
			   MED_NO_NAME,
			   MED_INTERNAL_UNDEF,
			   numdt,
			   numit,
			   entitype,
			   geotype,
			   MED_NODAL,
			   MED_UNDEF_STMODE,
			   MED_NO_PROFILE,
			   MED_UNDEF_INTERLACE,
			   MED_ALL_CONSTITUENT,
			   filter,
			   (unsigned char * const) attval);
 ERROR:
  return _ret;

}
