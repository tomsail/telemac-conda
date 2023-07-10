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
   \brief \MEDnProfileBrief
   \param fid \fid
   \return \li med_int \nProfile \li med_int \error
   \details
   \MEDnProfileDetails
   \par Remarques
   \MEDprofileDef
*/

med_int
MEDnProfile(const med_idt fid )
{
  med_int        _ret=-1,_err=-1;
  char           _path[MED_PROFILE_GRP_SIZE+1]=MED_PROFILE_GRP;
  med_size       _tmpn=0;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   *  nombre de profils
   */
  if ((_err=_MEDnObjects(fid,_path,&_tmpn)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_PROFILE,_path);
      goto ERROR;
    }

  _ret = (med_int) _tmpn;
 ERROR:
  return _ret;
}


