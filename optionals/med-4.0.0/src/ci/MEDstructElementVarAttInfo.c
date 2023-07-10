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

#include <stdlib.h>
#include <string.h>

/**\ingroup MEDstructElement
  \brief \MEDstructElementVarAttInfoBrief
  \param fid                 \fid
  \param modelname           \modelname
  \param attit               \attit
  \param varattname          \varattname
  \param varatttype          \varatttype
  \param ncomponent       \ncomponent

  \return \error

  \details \MEDstructElementVarAttInfoDetails
 */

med_err
MEDstructElementVarAttInfo(const med_idt                   fid,
			   const char*               const modelname,
			   const int                       attit,
			         char*               const varattname,
			         med_attribute_type* const varatttype,
			         med_int*            const ncomponent
			   )

{

  med_err  _ret=-1;
  char     _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1+MED_TAILLE_VARATR+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  int      _num = attit -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  strcat(_path,modelname);
  strcat(_path,MED_VARATR);

  /*
   * On recupere le nom de l'attribut
   */
  if ( _MEDobjectGetName(fid, _path ,_num, varattname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(attit);
    goto ERROR;
  }
  strcat(_path,varattname);

  if ( MEDstructElementVarAttInfoByName(fid,
					modelname,
					varattname,
					varatttype,
					ncomponent
					) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_STRUCT_MSG);
    SSCRUTE(modelname);SSCRUTE(_path);SSCRUTE("MEDstructElementVarAttInfoByName");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;
}
