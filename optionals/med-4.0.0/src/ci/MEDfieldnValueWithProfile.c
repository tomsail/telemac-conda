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
  \brief \MEDfieldnValueWithProfileBrief
  \param fid \fid
  \param fieldname \fieldname
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param geotype \geotype
  \param profileit \profileit
  \param storagemode \storagemode
  \param profilename \profilename
  \param profilesize \profilesize
  \param localizationname \localizationname
  \param nintegrationpoint \nintegrationpoint
  \retval med_int \nvalue
  \details \MEDfieldnValueWithProfileDetails
  \par Remarques
 */

med_int
MEDfieldnValueWithProfile(const med_idt fid, 
			  const char * const fieldname,
			  const med_int numdt,
			  const med_int numit,
			  const med_entity_type entitype, 
			  const med_geometry_type geotype, 
			  const int profileit,
			  const med_storage_mode storagemode, 
			  char * const profilename, 
			  med_int * const profilesize,
			  char * const localizationname, 
			  med_int * const nintegrationpoint)
{
 med_int  _ret=-1;
 char     _profilename[MED_NAME_SIZE+1]="";
 if ( (_ret = _MEDfieldnValue(fid,  fieldname, numdt, numit,
			      entitype,  geotype,
			      _profilename,profileit,
			      storagemode, profilesize,
			      localizationname, nintegrationpoint)) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE("_MEDfieldnValue");
    goto ERROR;
  }

 strncpy(profilename,_profilename,MED_NAME_SIZE+1);
 profilename[MED_NAME_SIZE]='\0';

 ERROR:

  return _ret;

}



