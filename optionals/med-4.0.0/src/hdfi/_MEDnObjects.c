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


med_err
_MEDnObjects(const med_idt fid,const char * const path,med_size * const n)
{

  med_err    _ret=-1;
  med_idt    _gid=0;
  H5G_info_t _group_info;

  if ( (_gid = H5Gopen(fid,path)) < 0 ) {
    _ret= MED_ERR_OPEN+MED_ERR_DATAGROUP;
    *n=0;
    goto ERROR;
  }

/*   if ( H5Gget_num_objs(_gid,n)  < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,path); */
/*     goto ERROR; */
/*   } */

  if (  H5Gget_info( _gid, &_group_info ) < 0) {
    MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,path);
    goto ERROR;
  }

  *n=(med_int) _group_info.nlinks;

  _ret = 0;

 ERROR:

  if (_gid>0)            if ( H5Gclose(_gid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,path);
    ISCRUTE_id(_gid);
  }

  return _ret;
}
