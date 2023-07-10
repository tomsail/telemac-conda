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

#include <assert.h>

med_err _MEDdatasetRd(const med_idt               id,
		      const char * const          datasetname,
		      const med_internal_type     datatype,
		      const med_filter* const     filter,
		      unsigned char * const value) {

  med_idt         _dataset=0, _dataspace=0;
  med_size        _datasetsize[1]={0};
  med_err         _ret=-1;
  med_idt         _hdftype=0;
  int             _datasetsizeEqualTosizespace = 0;
  med_access_mode _MED_ACCESS_MODE;
  med_int         _nvaluesperentity=0,_nconstituentpervalue=0;
  int             _i=1;

  hsize_t   _sizespace   [H5S_MAX_RANK];
  hsize_t   _maxsizespace[H5S_MAX_RANK];
  hsize_t   _memspacesizetmp[H5S_MAX_RANK];
  hsize_t   _maxmemspacesize[H5S_MAX_RANK];
  hsize_t   _memspacesize=0;

  hsize_t   _dim=0;


  switch(datatype)
    {
    case MED_FLOAT64 :
      _hdftype = H5T_NATIVE_DOUBLE;
      break;

    case MED_FLOAT32 :
      _hdftype = H5T_NATIVE_FLOAT;
      break;

    case MED_INT :
#if defined(HAVE_F77INT64)
      _hdftype = H5T_NATIVE_LONG;
#else
      _hdftype = H5T_NATIVE_INT;
#endif
      break;

    case MED_INT32 :
      _hdftype = H5T_NATIVE_INT;
      break;

    case MED_INT64 :
      /* _hdftype = H5T_NATIVE_LONG; */
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
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,MED_ERR_TYPEOF_MSG MED_ERR_FIELD_MSG);
      ISCRUTE_int(datatype);
      goto ERROR;
    }


  /* Calculate dataset size for checking */
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
    if ( ! (*filter).nentity )  {
      MED_ERR_(_ret,MED_ERR_NOTNULL,MED_ERR_FILTER,"");
      ISCRUTE((*filter).nentity);
      goto ERROR;
    }
    _datasetsize[0]*= (*filter).nentity;
  }  else
    _datasetsize[0]*= (*filter).profilearraysize;




 if ( (_dataset = H5Dopen(id,datasetname)) < 0) {
   MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATASET,datasetname);
   goto ERROR;
 }

 if ( (_dataspace = H5Dget_space(_dataset)) <0 ) {
   MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASPACE,MED_ERR_DATASET_MSG MED_ERR_NAME_MSG );
   SSCRUTE(datasetname);
   goto ERROR;
 }

 H5Sget_simple_extent_dims(_dataspace, _sizespace, _maxsizespace);
 _datasetsizeEqualTosizespace = ( (_sizespace[0]) == _datasetsize[0] );


 if ( !_datasetsizeEqualTosizespace ) {
   MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_DATASPACE,MED_ERR_SIZE_MSG);
   ISCRUTE_size(_sizespace[0]);ISCRUTE_size(_datasetsize[0]);
   goto ERROR;
 }

 for (_i=0; _i < (*filter).nspaces; ++_i) {
   if ( H5Dread(_dataset,_hdftype,(*filter).memspace[_i],
		(*filter).diskspace[_i],H5P_DEFAULT, value) < 0 ) {
     MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET, datasetname );
/*      ISCRUTE((void*)value); */
     H5Eprint1(stderr);
     ISCRUTE_id(_dataset);
     ISCRUTE_int(_i);
     ISCRUTE_id((*filter).memspace[_i]);
     ISCRUTE_id((*filter).diskspace[_i]);
     goto ERROR;
   }
 }

#ifdef _DEBUG_
 med_int *_ptr=(med_int*)value;
 printf("\n");
 if (datatype == MED_INT32)
   for (_i=0; _i < _datasetsize[0]; ++_i)
     printf("%d ",((int*) _ptr)[_i]);
 if ( (datatype == MED_FLOAT32) )
   for (_i=0; _i < _datasetsize[0]; ++_i)
     printf("%f ",((float*) _ptr)[_i]);
 if ( (datatype == MED_FLOAT64) )
   for (_i=0; _i < _datasetsize[0]; ++_i)
     printf("%f ",((double*) _ptr)[_i]);
 printf("\n");
#endif

 switch (datatype) {
 case MED_INTERNAL_CHAR:
 case MED_INTERNAL_SNAME:
 case MED_INTERNAL_NAME:
 case MED_INTERNAL_LNAME:
   /* for (_i=0; _i < (*filter).nspaces; ++_i) { */
   /*   H5Sget_simple_extent_dims((*filter).memspace[_i], _memspacesizetmp, _maxmemspacesize); */
   /*   if (_i > 0) assert( _memspacesize == _memspacesizetmp[0]); */
   /*   _memspacesize = _memspacesizetmp[0]; */
   /* } */
   H5Sget_simple_extent_dims((*filter).memspace[0],  &_memspacesize, _maxmemspacesize);

   /* ISCRUTE_size(_memspacesize); */
   /* ISCRUTE_size((*filter).nspaces); */
   /* ISCRUTE_size(_memspacesize*(*filter).nspaces*_dim); */
   /* Le calcul : _memspacesize*(*filter).nspaces*_dim est faux
      car seule une composante est selectionnée par memspace de taille memspacesize comprenant 
      les n composantes */

   value[_memspacesize*_dim]='\0';
   break;
 default:
   break;
 }

 _ret=0;
 ERROR:

  if ( _dataspace > 0 ) if ( H5Sclose(_dataspace) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASPACE, MED_ERR_ID_MSG );
    ISCRUTE_id(_dataspace);
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
