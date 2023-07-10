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

med_err _MEDobjectGetRank(const med_idt        gid,
			  const char * const   linkname,
			  med_size *    const  rank )
{
  med_err        _ret=-1;
  H5L_info_t      _linkinfo;

  if ( H5Lget_info( gid, linkname,  &_linkinfo, H5P_DEFAULT ) >= 0 ) {
    if ( _linkinfo.type == H5L_TYPE_HARD ) {
      if ( _linkinfo.corder_valid) {
	*rank = (med_size) _linkinfo.corder;
      } else {
	MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,linkname);
	ISCRUTE_int(_linkinfo.corder_valid);
      goto ERROR;

      }
    }else{
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_DATATYPE,MED_ERR_DATAGROUP_MSG);
      SSCRUTE(linkname);
      goto ERROR;
    }
  } else  {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,linkname);
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  return _ret;
}
