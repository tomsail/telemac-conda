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

med_idt
_MEDdatagroupCrOrderCr(const med_idt pid, const char * const name)
{
  med_idt         _id=0;
  hid_t           _gcpl_id=0;
  med_idt         _ret=-1;
  med_access_mode _MED_ACCESS_MODE;

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(pid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    ISCRUTE_int(_MED_ACCESS_MODE);
    goto ERROR;
  }

  if ( _MED_ACCESS_MODE == MED_ACC_RDONLY) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    ISCRUTE_int(_MED_ACCESS_MODE);
    goto ERROR;
  };


  _id = _MEDdatagroupOpen(pid,name);
  if (_id > 0)
    if (_MED_ACCESS_MODE == MED_ACC_RDEXT) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,name);
      ISCRUTE_int(MED_ACC_RDEXT);
      goto ERROR;
    }


  if ( (_gcpl_id =H5Pcreate(H5P_GROUP_CREATE) ) < 0) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_PROPERTY,MED_ERR_DATAGROUP_MSG);
    SSCRUTE(name);
    goto ERROR;
  }

  /* HDF-5 : UG
    Groups will be initially created in the compact‐or‐indexed format only when one or more of the following 
    conditions is met:
   •    The low version bound value of the library version bounds property has been set to Release 1.8.0 
        or later in the file access property list (see H5Pset_libver_bounds). Currently, that would 
        require an H5Pset_libver_bounds call with the low parameter set to H5F_LIBVER_LATEST.
        When this property is set for an HDF5 file, all objects in the file will be created using the latest 
        available format; no effort will be made to create a file that can be read by older libraries.
	
   •   The creation order tracking property, H5P_CRT_ORDER_TRACKED, has been set in the group creation property list (see H5Pset_link_creation_order).
  */
  if ( H5Pset_link_creation_order( _gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) <0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_PROPERTY,MED_ERR_DATAGROUP_MSG);
    SSCRUTE(name);
    goto ERROR;
  }


  /* sinon on le crée */
  if (_id <= 0)
    if ((_id = H5Gcreate2( pid, name, H5P_DEFAULT, _gcpl_id, H5P_DEFAULT ) ) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,name);
      goto ERROR;
    }

  _ret = _id;
 ERROR:
  if ( H5Pclose(_gcpl_id) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_PROPERTY,MED_ERR_DATAGROUP_MSG);
    SSCRUTE(name);
  }

  return _id;

}
