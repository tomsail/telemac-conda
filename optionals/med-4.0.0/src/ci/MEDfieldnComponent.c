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
#include <string.h>
#include <stdlib.h>
#include <med_outils.h>

/**\ingroup MEDfield
  \brief \MEDfieldnComponentBrief
  \param fid \fid
  \param ind \ind
  \retval med_int \ncomponent
  \details \MEDfieldnComponentDetails
 */

med_int
MEDfieldnComponent(const med_idt fid, 
		   const int ind)
{
  med_err _ret       = -1;
  char    _fieldname[MED_NAME_SIZE+1]="";
  int     _num       = ind-1;
  char    _path        [MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1]=MED_FIELD_GRP;
  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();


  /*
   * On recupere le nom du champ
   */
  if ( _MEDobjectGetName(fid, _path ,_num, _fieldname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(_num);
    goto ERROR;
  }

  if ( (_ret=MEDfieldnComponentByName( fid, _fieldname )) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_FIELD_MSG);
    SSCRUTE(_fieldname);SSCRUTE(_path);SSCRUTE("MEDfieldnComponentbyName");
    goto ERROR;
  }

 ERROR:
  return _ret;
}

