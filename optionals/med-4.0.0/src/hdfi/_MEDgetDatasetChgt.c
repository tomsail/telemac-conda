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

med_err _MEDgetDatasetChgt(const med_idt                       gid,
			   const med_data_type                 meddatatype,
			   const med_connectivity_mode         cmode,
			   med_bool *                  const   isasoftlink,
			   med_bool *                  const   chgt )
{
  med_err        _ret=-1;
  char           _datasetname[3+1+3+1]="";
  med_int        _changement=0;

  if (  _MEDgetDatasetName(_datasetname,meddatatype,cmode) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetDatasetName");
    SSCRUTE(_datasetname);ISCRUTE_int(meddatatype);goto ERROR;
  }

  if ( _MEDisasoftlink(gid, _datasetname, MED_TRUE, isasoftlink ) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_LINK,_datasetname);
    ISCRUTE_int(meddatatype);goto ERROR;
  }

  if (*isasoftlink)
    *chgt = MED_FALSE;
  else {

    strcat(_datasetname,"/"MED_NOM_CGT);

    if ( _MEDattrEntierLire(gid,MED_NOM_CGT,&_changement) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(_datasetname);ISCRUTE_int(meddatatype);
      SSCRUTE(MED_NOM_CGT);goto ERROR;
    }
    *chgt = (med_bool) _changement;

  }

  _ret = 0;
 ERROR:

  return _ret;
}
