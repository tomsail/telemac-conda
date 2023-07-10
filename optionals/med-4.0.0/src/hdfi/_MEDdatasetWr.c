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

med_err _MEDdatasetWr(const med_idt               id,
		      const char * const          datasetname,
		      const med_internal_type     datatype,
		      const med_filter* const     filter,
		      const void * const value) {

  med_idt         _dataset=0, _dataspace=0,_datadiskspace=0;
  med_size        _datasetsize[1]={0};
  med_err         _ret=-1;
  med_idt         _hdftype=0;
  int             _datasetsizeEqualTosizespace = 0;
  med_access_mode _MED_ACCESS_MODE;
  med_int         _nvaluesperentity=0,_nconstituentpervalue=0;
  int             _i=0;
  H5L_info_t      _linkinfo;
  hsize_t         _dim=0;
  const void      *_value= value;

 /*  ISCRUTE((*filter).nentity              ); */
/*   ISCRUTE((*filter).nvaluesperentity     ); */
/*   ISCRUTE((*filter).nconstituentpervalue ); */
/*   ISCRUTE((*filter).constituentselect       ); */
/*   ISCRUTE((*filter).switchmode              ); */
/*   ISCRUTE((*filter).filterarraysize         ); */
/*   ISCRUTE((*filter).profilearraysize        ); */
/*   ISCRUTE((*filter).storagemode             ); */
/*   SSCRUTE((*filter).profilename             ); */


  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(id) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  if ( _MED_ACCESS_MODE == MED_ACC_RDONLY) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    ISCRUTE_int(_MED_ACCESS_MODE);
    goto ERROR;
  }

  switch(datatype)
    {
    case MED_INTERNAL_FLOAT64 :
      _hdftype = H5T_NATIVE_DOUBLE;
      break;

    case MED_INTERNAL_FLOAT32 :
      _hdftype = H5T_NATIVE_FLOAT;
      break;

    case MED_INT :
#if defined(HAVE_F77INT64)
      _hdftype = H5T_NATIVE_LONG;
#else
      _hdftype = H5T_NATIVE_INT;
#endif
      break;

    case MED_INTERNAL_INT32 :
      _hdftype = H5T_NATIVE_INT;
      break;

    case MED_INTERNAL_INT64 :
      /* _hdftype = H5T_NATIVE_LONG; */
      /* _hdftype = H5T_NATIVE_LLONG; */
      _hdftype = MED_H5T_INT64;
      break;

    case MED_INTERNAL_CHAR:
      _hdftype = H5T_NATIVE_CHAR;
      _dim=1;
      break;

    case MED_INTERNAL_SNAME:
      if (!_dim) _dim= MED_SNAME_SIZE;
    case MED_INTERNAL_NAME:
      if (!_dim) _dim = MED_NAME_SIZE;
    case MED_INTERNAL_LNAME:
      if (!_dim) _dim = MED_LNAME_SIZE;

/*       ISCRUTE(_dim); */
      if( (_hdftype =  H5Tarray_create1( H5T_NATIVE_CHAR, 1, &_dim, 0 )) < 0) {
	MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATATYPE,"");goto ERROR;
      }
      break;

    default :
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,MED_ERR_TYPEOF_MSG);
      ISCRUTE_int(datatype);
      goto ERROR;
    }

  /* Calculate dataset size*/
  _nvaluesperentity     = (*filter).nvaluesperentity;
  _nconstituentpervalue = (*filter).nconstituentpervalue;
  if ( (!_nvaluesperentity)  || (!_nconstituentpervalue) ) {
    MED_ERR_(_ret,MED_ERR_NOTNULL,MED_ERR_FILTER,"");
    ISCRUTE((*filter).nvaluesperentity);
    ISCRUTE((*filter).nconstituentpervalue);
    goto ERROR;
  }
  _datasetsize[0] = (*filter).nvaluesperentity * (*filter).nconstituentpervalue;
  if ( (*filter).profilearraysize == MED_UNDEF_SIZE ) {
/*     if ( ! (*filter).nentity )  { */
/*       MED_ERR_(_ret,MED_ERR_NOTNULL,MED_ERR_FILTER,""); */
/*       ISCRUTE((*filter).nentity); */
/*       goto ERROR; */
/*     } */
    _datasetsize[0]*= (*filter).nentity;
  }  else
    _datasetsize[0]*= (*filter).profilearraysize;

  if ( ! _datasetsize[0] || ! _value) {
    _dataspace = H5Screate( H5S_NULL );
    _value=NULL;
  }

  if (!_dataspace)
    if ((_dataspace = H5Screate_simple(1,_datasetsize,NULL)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATASPACE,MED_ERR_SIZE_MSG);
      ISCRUTE_size(_datasetsize[0]);
      goto ERROR;
    }

  if ( H5Lget_info( id, datasetname,  &_linkinfo, H5P_DEFAULT ) >= 0 ) {
    if ( _linkinfo.type == H5L_TYPE_SOFT )
      if ( H5Ldelete(id,datasetname,H5P_DEFAULT) < 0 ) {
	MED_ERR_(_ret,MED_ERR_DELETE,MED_ERR_LINK,datasetname);
	goto ERROR;
      }
  }

  /* On s'assure de l'existence d'un dataset.
     S'il n'existe pas, il est crée.
     S'il existe, le mode MED_ACC_RDEXT génère une erreur.
     Sinon, sa taille est lue et comparée à la taille du dataspace préalablement crée.
     Si les tailles sont différente le dataset est supprimé et recrée à la bonne taille.
  */

  if ( (_dataset = H5Dopen(id,datasetname)) < 0) {

    if ((_dataset = H5Dcreate(id,datasetname,_hdftype,_dataspace, H5P_DEFAULT)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATASET,datasetname);
      goto ERROR;
    }

  } else {

    if ( _MED_ACCESS_MODE == MED_ACC_RDEXT )  {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_DATASET_MSG);
      ISCRUTE_int(_MED_ACCESS_MODE);
      SSCRUTE(datasetname);
      goto ERROR;
    }

    if ( (_datadiskspace = H5Dget_space(_dataset)) <0 ) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASPACE,MED_ERR_DATASET_MSG MED_ERR_NAME_MSG );
      SSCRUTE(datasetname);
      goto ERROR;
    }

    {
      hsize_t   _sizespace   [H5S_MAX_RANK];
      hsize_t   _maxsizespace[H5S_MAX_RANK];

      H5Sget_simple_extent_dims(_datadiskspace, _sizespace, _maxsizespace);
      _datasetsizeEqualTosizespace = ( (_sizespace[0]) == _datasetsize[0] );
    }

    if ( !_datasetsizeEqualTosizespace ) {

      if ( H5Dclose(_dataset) < 0 ) {
	MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASET, datasetname );
	goto ERROR;
      }

      if ( H5Ldelete(id,datasetname,H5P_DEFAULT)  < 0) {
	MED_ERR_(_ret,MED_ERR_DELETE,MED_ERR_DATASET, datasetname );
	goto ERROR;
      }
      if ( (_dataset = H5Dcreate(id,datasetname,_hdftype,_dataspace,H5P_DEFAULT)) < 0){
	MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATASET, datasetname );
	goto ERROR;
      }

    }
  } /*Fin de traitement d'un dataset existant */


  if (_value)
    for (_i=0; _i < (*filter).nspaces; ++_i) {
      if ( H5Dwrite(_dataset,_hdftype,(*filter).memspace[_i],
		    (*filter).diskspace[_i],H5P_DEFAULT, _value) < 0 ) {
	MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET, datasetname );
	ISCRUTE_id(_dataset);
	ISCRUTE_int(_i);
	ISCRUTE_id((*filter).memspace[_i]);
	ISCRUTE_id((*filter).diskspace[_i]);
	goto ERROR;
      }
    }

    _ret = 0;

  ERROR:

    if ( _dataspace > 0 ) if ( H5Sclose(_dataspace) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASPACE, MED_ERR_ID_MSG );
      ISCRUTE_id(_dataspace);
    }

    if ( _datadiskspace > 0 ) if ( H5Sclose(_datadiskspace) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASPACE, MED_ERR_ID_MSG );
      ISCRUTE_id(_datadiskspace);
    }

    if ( _dataset > 0 ) if ( H5Dclose(_dataset) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASET, MED_ERR_ID_MSG );
      ISCRUTE_id(_dataset);
    }

    if ( _dim > 1 ) if ( H5Tclose(_hdftype) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATATYPE, MED_ERR_ID_MSG );
      ISCRUTE_id(_hdftype);
    }

    return _ret;
  }
