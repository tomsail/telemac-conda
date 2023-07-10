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

med_err _MEDattributeNumWrByName(med_idt pid,
				 const char * const path ,
				 const char * const attname,
				 const med_internal_type type,
				 const unsigned char * const  val)

{
  med_idt _attid=0,aid=0;
  med_err _ret=-1;
  med_idt  type_hdf;
  med_access_mode MED_ACCESS_MODE;
  H5O_info_t      _oinfo;

  if ( (MED_ACCESS_MODE = _MEDmodeAcces(pid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE, "MED_ACC_UNDEF" );
    SSCRUTE(attname); SSCRUTE(path);goto ERROR;
  }

  switch(type)
    {
    case MED_INTERNAL_FLOAT64 :
      type_hdf = H5T_NATIVE_DOUBLE;
      break;
      
    case MED_INTERNAL_INT :
#if defined(HAVE_F77INT64)
      type_hdf = H5T_NATIVE_LONG;
#else
      type_hdf = H5T_NATIVE_INT;
#endif
      break;

    default :
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_DATATYPE, MED_ERR_VALUE_MSG );
      ISCRUTE_int(type); SSCRUTE(attname); SSCRUTE(path);goto ERROR;
    }

  if ((aid = H5Screate(H5S_SCALAR)) < 0){
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASPACE, attname );
    ISCRUTE_id(aid);
  }

  if  ( (_attid=H5Aopen_by_name( pid, path, attname, H5P_DEFAULT, H5P_DEFAULT )) >= 0 ) {

    if ( H5Oget_info( pid, &_oinfo ) <0) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"H5Oget_info");
	goto ERROR;
    }

    if ( MED_ACCESS_MODE == MED_ACC_RDEXT ) {
      if ( ( _oinfo.type != H5O_TYPE_GROUP) ||
	   ( (_oinfo.type == H5O_TYPE_GROUP) &&
	     ( strcmp(attname,MED_NOM_CGT) &&
	       strcmp(attname,MED_NOM_CGS) &&
	       strcmp(attname,MED_NOM_NXT) &&
	       strcmp(attname,MED_NOM_NXI) &&
	       strcmp(attname,MED_NOM_PVI) &&
	       strcmp(attname,MED_NOM_PVT) ) )
	   ) {      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE, "MED_ACC_RDEXT" );
	SSCRUTE(attname); SSCRUTE(path);goto ERROR;
      }
    }

  } else {
    if ( (_attid=H5Acreate_by_name( pid, path, attname, type_hdf, aid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT )) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_ATTRIBUTE, attname );
      SSCRUTE(path);goto ERROR;
    }
  }

  if ( H5Awrite(_attid,type_hdf,val) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE, attname );
    SSCRUTE(path);H5Eprint1(stderr);goto ERROR;
  }

  _ret=0;

 ERROR:

  if (aid > 0 ) if ( H5Sclose(aid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASPACE, MED_ERR_ID_MSG );
    ISCRUTE_id(aid);
  }

  if (_attid > 0 ) if ( H5Aclose(_attid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_ATTRIBUTE, MED_ERR_ID_MSG );
    ISCRUTE_id(_attid);
  }

  return _ret;

}
