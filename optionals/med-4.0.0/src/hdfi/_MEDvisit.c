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
_MEDvisit(const med_idt fid, const char * const srcpath, const char * const dstpath,
	  herr_t (*func)() )
{

  med_err    _ret=-1;
  med_idt    _gid1=0,_gid2=0;
  med_size   _n;

  visitordatas _data;

  med_access_mode MED_ACCESS_MODE;

  _data.srcpath=(char*)srcpath;
  _data.dstpath=(char*)dstpath;

  if ( (MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MESSAGE("Impossible de déterminer le mode d'acces au fichier ");
    return -1;
  }

  /*TESTER LE MODE D'ACCES: A T'ON LE DROIT D'ECRIRE*/


  if ( (_gid1 = H5Gopen(fid,srcpath)) < 0 ) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,srcpath);
    goto ERROR;
  }
  _data.gid1=_gid1;

  if ( (_gid2 = H5Gopen(fid,dstpath)) < 0 ) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,dstpath);
    goto ERROR;
  }
  _data.gid2=_gid2;


  if ( H5Lvisit( _gid1,H5_INDEX_NAME, H5_ITER_NATIVE, func, &_data ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_VISIT,MED_ERR_DATAGROUP,srcpath);
    goto ERROR;
  }


  _ret = 0;

 ERROR:

  if (_gid1>0)            if ( H5Gclose(_gid1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,srcpath);
    ISCRUTE_id(_gid1);
  }

  if (_gid2>0)            if ( H5Gclose(_gid2) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,dstpath);
    ISCRUTE_id(_gid2);
  }

  return _ret;
}
