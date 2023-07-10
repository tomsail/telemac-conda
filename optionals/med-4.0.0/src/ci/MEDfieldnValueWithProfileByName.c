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
#include <string.h>
#include <stdlib.h>
#include <med_outils.h>

/**\ingroup MEDfield
  \brief \MEDfieldnValueWithProfileByNameBrief
  \param fid \fid
  \param fieldname \fieldname
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param geotype \geotype
  \param profilename \profilename
  \param storagemode \storagemode
  \param profilesize \profilesize
  \param localizationname \localizationname
  \param nintegrationpoint \nintegrationpoint
  \retval med_int \nvalue
  \details \MEDfieldnValueWithProfileByNameDetails
  \par Remarques
 */

med_int
MEDfieldnValueWithProfileByName(const med_idt fid,
				const char * const fieldname,
				const med_int numdt,
				const med_int numit,
				const med_entity_type entitype, 
				const med_geometry_type geotype,
				const char * const profilename,
				const med_storage_mode storagemode,
				med_int * const profilesize,
				char * const localizationname, 
				med_int * const nintegrationpoint)
{
  med_int  _ret=-1;

  if ( (_ret = _MEDfieldnValue(fid,  fieldname, numdt, numit,
			      entitype,  geotype,
			      (char * const) profilename,-1,
			      storagemode, profilesize,
			      localizationname, nintegrationpoint)) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE("_MEDfieldnValue");
    goto ERROR;
  }

 ERROR:

  return _ret;
}




