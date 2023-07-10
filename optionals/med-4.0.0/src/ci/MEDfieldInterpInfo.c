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
#include <hdf5.h>

/**\ingroup MEDfield
  \brief \MEDfieldInterpInfoBrief
  \param fid           \fid
  \param fieldname     \fieldname
  \param interpit      \interpit
  \param interpname    \interpname
  \return \error
  \details \MEDfieldInterpInfoDetails
 */


med_err
MEDfieldInterpInfo(const med_idt       fid,
		   const char*   const fieldname,
		   const int           interpit,
		         char*   const interpname
		   ) {


  med_err  _ret=-1;
  med_idt  _fieldinterpid=0;
  char     _path[MED_TAILLE_CHA_INTERP+MED_NAME_SIZE+1]=MED_CHA_INTERP;
  int      _num = interpit -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  strcat(_path,fieldname);

  /*
   * Si le DataGroup /MED_CHA_INTERP/<fieldname> n'existe pas -> erreur
   */
  if ((_fieldinterpid = _MEDdatagroupOpen(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

 /*
   * On recupere le nom de l'interpolation
   */
  if ( _MEDobjectGetName(_fieldinterpid, "." ,_num, interpname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(interpit);
    goto ERROR;
  }

  _ret=0;

 ERROR:

  if (_fieldinterpid>0)            if (_MEDdatagroupFermer(_fieldinterpid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_fieldinterpid);
  }

  return _ret;
}
