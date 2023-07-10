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
   \brief \MEDprofileSizeByNameBrief
   \param fid \fid
   \param profilename \profilename
   \return \li med_int \profilesize \li med_int \error
   \details
   \MEDprofileSizeByNameDetails
   \par Remarques
   \MEDprofileDef
*/
med_int
MEDprofileSizeByName(const med_idt fid, const char * const profilename)
{
  med_idt _pflid=0;
  med_err _ret=-1;
  char _path[MED_PROFILE_GRP_SIZE+MED_NAME_SIZE+1]=MED_PROFILE_GRP;
  med_int _n=0;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

  /*
   * Renvoie une taille nulle, s'il n'y a pas de profil
   *
   */
  if (!strlen(profilename)) return 0;

  /*
   * ouverture du groupe /PROFILS/<nom>
   */
  strcat(_path,profilename);
  if ((_pflid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_PROFILE_GRP);
    SSCRUTE(_path); goto ERROR;
  }

  if (_MEDattrEntierLire(_pflid,MED_NOM_NBR,&_n) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NBR);
    ISCRUTE(_n);SSCRUTE(_path); goto ERROR;
  }

  _ret = _n;

 ERROR:
  if ( _pflid > 0 ) if (_MEDdatagroupFermer(_pflid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_pflid);
  }

  return _ret;
}


