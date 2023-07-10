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
#include "med_utils.h"

med_err _MEDisasoftlink(const med_idt               id,
			const char * const          linkname,
			med_bool                    linkmustexist,
			med_bool *   const          isasoftlink
			) {

  med_err         _ret=-1;
  H5L_info_t      _linkinfo;

  *isasoftlink=MED_FALSE;
  if ( H5Lget_info( id, linkname,  &_linkinfo, H5P_DEFAULT ) >= 0 ) {
    if ( _linkinfo.type == H5L_TYPE_SOFT )
      *isasoftlink=MED_TRUE;
  } else  {
    if (linkmustexist) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_LINK,linkname);
      goto ERROR;
    }
  }

  _ret=0;
 ERROR:
  return _ret;
}
