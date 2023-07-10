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
#include <hdf5.h>
#include <string.h>

/**\ingroup MEDfield
  \brief \MEDfieldInterpWrBrief
  \param fid           \fid
  \param fieldname     \fieldname
  \param interpname    \interpname

  \return \error
  \details \MEDfieldInterpWrDetails
 */

med_err
MEDfieldInterpWr(const med_idt fid,
		 const char* const fieldname,
		 const char* const interpname
		 )
{
  med_err  _ret=-1;
  med_idt  _fieldinterpid=0,_fieldid=0,_interpid=0;
  char     _path[MED_TAILLE_CHA_INTERP+MED_NAME_SIZE+1]=MED_CHA_INTERP;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
 if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  /*
   * On ouvre le champ /MED_CHA_INTERP
   */
 if ((_fieldinterpid = _MEDdatagroupOpen(fid,MED_CHA_INTERP)) < 0)
   if ((_fieldinterpid = _MEDdatagroupCreer(fid,MED_CHA_INTERP)) < 0) {
     MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_CHA_INTERP);
      goto ERROR;
    }

 NOFINALBLANK(interpname,ERROR);
 strcat(_path,fieldname);

  /*
   * On ouvre le champ /MED_CHA_INTERP/<fieldname>
   */
 if ((_fieldid = _MEDdatagroupOuvrir(_fieldinterpid,fieldname)) < 0)
   if ((_fieldid = _MEDdatagroupCreer(_fieldinterpid,fieldname)) < 0) {
     MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_path);
     goto ERROR;
   }

 /*
  * Si le DataGroup /MED_CHA_INTERP/<fieldname>/<interpname> n'existe pas, on le cree
  */
 if ((_interpid = _MEDdatagroupOpen(_fieldid,interpname)) < 0)
   if ((_interpid = _MEDdatagroupCreer(_fieldid,interpname)) < 0) {
     MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,interpname);
     SSCRUTE(_path);
     goto ERROR;
   }

 _ret = 0;

 ERROR:

  if (_interpid>0)            if (_MEDdatagroupFermer(_interpid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,interpname);
    SSCRUTE(_path); ISCRUTE_id(_interpid);
  }

  if (_fieldid>0)            if (_MEDdatagroupFermer(_fieldid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,fieldname);
    SSCRUTE(_path);ISCRUTE_id(_fieldid);
  }

  if (_fieldinterpid>0)            if (_MEDdatagroupFermer(_fieldinterpid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_CHA_INTERP);
    SSCRUTE(_path); ISCRUTE_id(_fieldinterpid);
  }


  return _ret;
}

