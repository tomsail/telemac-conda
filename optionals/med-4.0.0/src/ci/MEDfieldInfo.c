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


#include "med_config.h"
#include <med.h>
#include "med_outils.h"
#include <string.h>

/**\ingroup MEDfield
  \brief \MEDfieldInfoBrief
  \param fid \fid
  \param ind \ind
  \param fieldname \fieldname
  \param meshname \meshname
  \param localmesh \localmesh
  \param fieldtype \fieldtype
  \param componentunit \componentunit
  \param componentname \componentname
  \param dtunit \dtunit
  \param ncstp \ncstp
  \retval med_err \error
  \details \MEDfieldInfoDetails
 */


med_err
MEDfieldInfo(const med_idt fid,
	     const int ind,
	     char * const fieldname,
	     char * const meshname,
	     med_bool * const localmesh,
	     med_field_type * const fieldtype,
	     char * const componentname,
	     char * const componentunit,
	     char * const dtunit,
	     med_int *const ncstp)
{
  med_err  _ret=-1;
  char     _fieldpath[MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1]=MED_FIELD_GRP;
  int      _num = ind -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On recupere le nom du champ
   */
  if ( _MEDobjectGetName(fid, _fieldpath ,_num, fieldname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_fieldpath);ISCRUTE_int(ind);
    goto ERROR;
  }
  strcat(_fieldpath,fieldname);

  if ( MEDfieldInfoByName(fid, fieldname, meshname,localmesh,
			  fieldtype, componentname, componentunit, dtunit, ncstp) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_fieldpath);SSCRUTE("MEDfieldInfoByName");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;
}
