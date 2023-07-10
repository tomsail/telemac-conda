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
  \brief \MEDfieldnInterpBrief
  \param fid           \fid
  \param fieldname     \fieldname
  \retval med_int \ninterp
  \return \error
  \details \MEDfieldnInterpDetails
 */

med_int
MEDfieldnInterp(const med_idt fid,
		const char* const fieldname
		)
{

  med_int  _ret=-1,_err=-1;
  med_idt  _fieldinterpid=0,_fieldid=0;
  char     _interppath[MED_FIELD_GRP_SIZE+MED_NAME_SIZE+MED_INTERPOLATION_GRP_SIZE+1]=MED_FIELD_GRP;
  char     _path[MED_TAILLE_CHA_INTERP+MED_NAME_SIZE+1]=MED_CHA_INTERP;
  med_size _tmpn=0;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();


 /*
   * On ouvre le champ /MED_CHA_INTERP
   */
 if ((_fieldinterpid = _MEDdatagroupOpen(fid,MED_CHA_INTERP)) < 0)
   goto SORTIE;

 strcat(_interppath,fieldname);

 /*
  * On ouvre le champ /MED_CHA_INTERP/<fieldname>
  */
 if ((_fieldid = _MEDdatagroupOuvrir(_fieldinterpid,fieldname)) < 0)
   goto SORTIE;

 /*
  *  Lecture du nombre d'interpolations
  */
 if ((_err=_MEDnObjects(_fieldinterpid,".",&_tmpn)) <0)
   if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
     MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_INTERP,_interppath);
     goto ERROR;
   }

 SORTIE:
  _ret = (med_int) _tmpn;

 ERROR:

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



