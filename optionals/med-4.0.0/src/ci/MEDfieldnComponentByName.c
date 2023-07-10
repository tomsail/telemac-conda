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
  \brief \MEDfieldnComponentByNameBrief
  \param fid \fid
  \param fieldname \fieldname
  \retval med_int \ncomponent
  \details \MEDfieldnComponentByNameDetails
 */

med_int
MEDfieldnComponentByName(const med_idt fid, const char * const fieldname)
{

  med_err _ret =-1;
  med_idt _datagroup=0;
  char    _datagroupname[MED_NAME_SIZE+1]="";
  char    _path        [MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1]=MED_FIELD_GRP;
  med_int _n;

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();


  strcat(_path,fieldname);
  /*
   * On recupere le nombre de composants
   */
  if ((_datagroup = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }
  if ( _MEDattrEntierLire(_datagroup,MED_NOM_NCO,&_n) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NCO);
    SSCRUTE(_path);goto ERROR;
  }
  if (_MEDdatagroupFermer(_datagroup) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  _ret = (med_int) _n;
 ERROR:
  return _ret;
}

