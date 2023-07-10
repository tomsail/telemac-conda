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
_MEDobjectGetName(const med_idt fid,const char * const path,const med_size ind,char *name) {

  med_err _ret=-1;

  if ( H5Literate_by_name(fid,path, H5_INDEX_NAME, H5_ITER_INC,
			  (hsize_t *) &ind, _MEDcopyName, name,H5P_DEFAULT ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_VISIT,MED_ERR_DATAGROUP,path);
    /* H5Eprint1(stderr); */
    goto ERROR;
  }

/*   return __MEDobjectGetName(fid, path, ind, name, H5_INDEX_NAME, H5_ITER_NATIVE); */
/*   return __MEDobjectGetName(fid, path, ind, name, H5_INDEX_CRT_ORDER, H5_ITER_NATIVE); */

  _ret = 0;
 ERROR:
  return _ret;
}

