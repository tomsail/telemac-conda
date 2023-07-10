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

/**\ingroup MEDprofile
   \brief \MEDprofileInfoBrief
   \param fid \fid
   \param profileit \profileit
   \retval profilename \profilename
   \retval profilesize \profilesize
   \return \error
   \details
   \MEDprofileInfoDetails
   \par Remarques
   \MEDprofileDef
*/

med_err
MEDprofileInfo(const med_idt          fid,
	       const int              profileit,
	       char    *     const    profilename,
	       med_int *     const    profilesize )
{
  med_idt        _pfid=0;
  med_err        _ret=-1;
  char           _path[MED_PROFILE_GRP_SIZE+MED_NAME_SIZE+1]=MED_PROFILE_GRP;
  int            _num=profileit-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * ouverture du groupe /PROFILS/"nom"
   */
  if ( _MEDobjectGetName(fid, _path ,_num, profilename) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);
    ISCRUTE_int(profileit);
    goto ERROR;
  }

  strcat(_path,profilename);
  if ((_pfid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_PROFILE_MSG);
    SSCRUTE(_path);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_NBR
   */
  if (_MEDattrEntierLire(_pfid,MED_NOM_NBR,profilesize) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_PROFILE_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_NBR);ISCRUTE(*profilesize);
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  if ( _pfid > 0 ) if ( _MEDdatagroupFermer(_pfid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_PROFILE_GRP);
    ISCRUTE_id(_pfid);
  }

  return _ret;
}
