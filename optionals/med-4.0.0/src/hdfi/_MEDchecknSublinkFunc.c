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
#include <string.h>

med_err _MEDchecknSublinkFunc(med_idt id,const char *lname, const H5L_info_t *linfo, med_bool  *data) {

  med_err  _ret=-1;
  H5O_info_t oinfo;
  H5G_info_t _group_info;


#ifdef _DEBUG_
  SSCRUTE(lname);
#endif

  if (!strcmp(lname,".")) return 0;

  switch ( (*linfo).type ) {

  case H5L_TYPE_SOFT:
    oinfo.type=H5G_LINK;
    break;
  case H5L_TYPE_HARD:
    if ( H5Oget_info_by_name( id, lname, &oinfo, H5P_DEFAULT ) <0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"H5Oget_info_by_name");
      SSCRUTE(lname);
    }
    break;
  case H5L_TYPE_EXTERNAL:
  case H5L_TYPE_ERROR:
  default:
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_HDFTYPE,lname);
    ISCRUTE_int((*linfo).type);
    goto ERROR;
    break;
 }

  switch ( oinfo.type ) {

  case H5G_GROUP:

    if ( H5Gget_info_by_name(id, lname, &_group_info, H5P_DEFAULT  ) < 0 ){
      MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_GROUP, MED_ERR_NAME_MSG );
      SSCRUTE(lname);goto ERROR;
    }

    if ( _group_info.nlinks > 1 ) {
      *data=MED_TRUE;
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_GROUP,lname);
      ISCRUTE_size(_group_info.nlinks);goto ERROR;
    }

  /*   SSCRUTE(lname); */
/*     ISCRUTE(*data); */
/*     ISCRUTE(_group_info.nlinks); */

    break;

  case H5G_DATASET:
  case H5G_LINK:
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_HDFTYPE,lname);
    goto ERROR;

    break;
  case H5G_TYPE:
  default:
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_HDFTYPE,lname);
    goto ERROR;
  }
  _ret = 0;

 ERROR:
  return _ret;
}

