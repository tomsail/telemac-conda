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
  \brief \MEDlinkInfoBrief
  \param fid \fid
  \param linkit \linkit
  \param meshname \meshname
  \param linksize \linksize
  \retval med_err  \error
  \details \MEDlinkInfoDetails
 */


med_err
MEDlinkInfo(const med_idt             fid,
	    const int                 linkit,
	    char              * const meshname,
	    med_int           * const linksize )
{
  med_err _ret=-1;
  char    _path[MED_TAILLE_LIENS+MED_NAME_SIZE+1]=MED_LIENS;
  int     _num=linkit-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * ouverture du groupe /LIENS/"nom"
   */
  if ( _MEDobjectGetName(fid, _path ,_num, meshname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);
    ISCRUTE_int(linkit);
    goto ERROR;
  }
  *linksize =  MEDlinkInfoByName(fid,meshname );

  if (*linksize <0) _ret=*linksize; else _ret = 0;
 ERROR:

  return _ret;
}
