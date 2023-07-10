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

/**\ingroup MEDstructElement
  \brief \MEDstructElementNameBrief
  \param fid \fid
  \param mgeotype \mgeotype
  \param modelname \modelname
  \return \error
  \details \MEDstructElementNameDetails
  \see  MEDstructElementGeotype
 */

med_err
MEDstructElementName(const med_idt                 fid,
		     const med_geometry_type       mgeotype,
		     char *                  const modelname) {

  med_err  _ret=-1;
  char     _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1]="/"MED_ELSTRUCT_NAME;
  int      _num = mgeotype-MED_STRUCT_GEO_INTERNAL-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On recupere le nom de l'attribut
   */
  if ( _MEDobjectCrOrderGetName(fid, _path ,_num, modelname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);
    ISCRUTE_int(_num);SSCRUTE(modelname);
    goto ERROR;
  }
  _ret = 0;

 ERROR:

  return _ret;
}

