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

#include <mpi.h>

med_idt _MEDparFileOpen(const char * const filename,const med_access_mode accessmode,
		     const MPI_Comm comm, const MPI_Info info)
{
  med_idt _fid     = -1;
  hid_t   _fapl    = H5P_DEFAULT;
  int     _hdf_mode= -1;

  switch(accessmode)
    {
    case MED_ACC_RDWR :
    case MED_ACC_RDEXT    :
      _hdf_mode = MED_ACC_RDWR;
      break;

    case MED_ACC_RDONLY :
      _hdf_mode = H5F_ACC_RDONLY;
      break;

    default :
      MED_ERR_(_fid,MED_ERR_RANGE,MED_ERR_ACCESS,filename);
      goto ERROR;
    }

  if ( (_fapl = H5Pcreate (H5P_FILE_ACCESS)) < 0 ) {
    MED_ERR_(_fid,MED_ERR_CREATE,MED_ERR_PROPERTY,MED_ERR_PARALLEL_MSG);
    goto ERROR;
  }

  if ( H5Pset_fapl_mpio(_fapl, comm, info) < 0 ) {
    MED_ERR_(_fid,MED_ERR_INIT,MED_ERR_PROPERTY,MED_ERR_PARALLEL_MSG);
    goto ERROR;
  }
#if H5_VERS_MINOR > 10
#error "Don't forget to change the compatibility version of the library !"
#endif
  if ( H5Pset_libver_bounds( _fapl, H5F_LIBVER_18, H5F_LIBVER_18 ) ) {
    MED_ERR_(_fid,MED_ERR_INIT,MED_ERR_PROPERTY,MED_ERR_FILEVERSION_MSG);
    goto ERROR;
  }

  if ((_fid = H5Fopen(filename,_hdf_mode,_fapl)) < 0) {
    MED_ERR_(_fid,MED_ERR_OPEN,MED_ERR_FILE,"");
    ISCRUTE_int(accessmode);
    goto ERROR;
  }

  if ( H5Pclose(_fapl) < 0 ) {
    MED_ERR_(_fid,MED_ERR_CLOSE,MED_ERR_PROPERTY,"");
    _fid=-1;goto ERROR;
  }


/* Adjust the size of metadata cache */
/* config.version = H5AC__CURR_CACHE_CONFIG_VERSION; */
/* H5Fget_mdc_config(_fid, &config); */
/* config.set_initial_size = 1; */
/* config.initial_size = 8*1024*1024; */
/* config.max_size = 16*1024*1024; */
/* H5Fset_mdc_config(_fid, &config); */

  _MEDsetModeAcces(_fid,accessmode);
  _MEDfileVersion(_fid);

 ERROR:

  return _fid;
}
