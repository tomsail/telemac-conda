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


/* 	  herr_t (*func)( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) ) */

med_err
_MEDiterate(const med_idt fid,  herr_t (*func)(), void * itdatas )
{

  med_err    _ret=-1;
  med_size   _n;
  hsize_t    _idx=0;

  if ( H5Literate( fid, H5_INDEX_NAME, H5_ITER_NATIVE, &_idx, func, itdatas ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_VISIT,MED_ERR_DATAGROUP,"");
    ISCRUTE_size(_idx);
    goto ERROR;
  }


  _ret = 0;

 ERROR:

  return _ret;
}
