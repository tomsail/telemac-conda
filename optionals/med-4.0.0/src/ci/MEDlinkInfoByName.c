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


/**\ingroup MEDlink
  \brief \MEDlinkInfoByNameBrief
  \param fid \fid
  \param meshname \meshname
  \retval med_int  \linksize
  \details \MEDlinkInfoByNameDetails
 */

med_int
MEDlinkInfoByName(const med_idt             fid,
		  const char        * const meshname )
{
  med_idt _lid=0;
  med_int _ret=-1;
  char    _path[MED_TAILLE_LIENS+MED_NAME_SIZE+1]=MED_LIENS;
  med_int _n = 0;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * ouverture du groupe /LIENS/"meshname"
   */
  strcat(_path,meshname);
  if ((_lid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_LINK_MSG);
    SSCRUTE(_path);
    goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_NBR
   */
  if (_MEDattrEntierLire(_lid,MED_NOM_NBR,&_n) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LINK_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_NBR);ISCRUTE(_n);
    goto ERROR;
  }


  _ret = _n;
 ERROR:

  if ( _lid > 0 ) if ( _MEDdatagroupFermer(_lid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_PROFILE_GRP);
    ISCRUTE_id(_lid);
  }

  return _ret;
}
