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

/**\ingroup MEDfilter
   \MEDfilterBrief
   \param fid \fid
   \param nentity \nentity \nentityMEDfilterCm
   \param nvaluesperentity \nvaluesperentity \nvaluesperentityMEDfilterCm \nvaluesperentityMEDfilterEx
   \param nconstituentpervalue \nconstituentpervalue \nconstituentpervalueMEDfilterEx
   \param constituentselect \constituentselect \constituentselectMEDfilterEx
   \param storagemode \storagemode
   \param profilename \profilename
   \param filterarraysize \filterarraysize \filterarraysizeMEDfilterCm
   \param filterarray \filterarray
   \retval \filter \filterMEDfilterCm
   \return \error

   \details
   \MEDfilterDetails

   \par Remarques
   \MEDfilterNoIGNote
*/

static  med_int  _identity        (const med_int * const filterarray, int i)  { return i; }
static  med_int  _withfilterarray (const med_int * const filterarray, int i)  { return (filterarray[i]-1); }
static  med_int  _withprofilearray(const med_int * const profilearray,int i)  { return (profilearray[i]-1); }

med_err _MEDfilterEntityNoIGlobalCr(const med_idt          fid,
				    const med_int          nentity,
				    const med_int          nvaluesperentity,
				    const med_int          nconstituentpervalue,
				    const med_int          constituentselect,
				    const med_storage_mode storagemode,
				    const char * const     profilename,
				    const med_int          filterarraysize,
				    const med_int* const   filterarray,
				    med_filter* const      filter) {

  med_idt    _memspace[1]={0},_diskspace[1]={0};
  med_size   _memspacesize[1]={0},_diskspacesize[1]={0};
  med_int    profilearraysize=0;
  med_int    _profilearraysize=0, *_profilearray=0,(*_profilearrayfunc)(const med_int * const, int)=0;
  med_int    _filterarraysize=0,(*_filterarrayfunc)(const med_int * const, int)=0;
  med_size   *_fltmem=NULL,*_pfldisk=NULL;
  med_size   _fltmemsize[1],_pfldisksize[1];
  med_size   _onedimallvaluesmemoffset=0,_onedimallvaluesdiskoffset=0;
  med_err    _ret=-1;
  int        _i=0,_j=0,_index=0;
  int        _dim=0, _firstdim=0, _dimutil=0, _lastdim=0 ;

  if ( constituentselect != MED_ALL_CONSTITUENT) {
    _firstdim = constituentselect-1;
    _lastdim  = constituentselect;
    _dimutil  = 1;
  } else {
    _firstdim = 0;
    _lastdim  = nconstituentpervalue;
    _dimutil  = nconstituentpervalue;
  }

  /* Conditionne les traitements à l'existence d'un profil */
  if ( strlen(profilename) ) {
    profilearraysize = MEDprofileSizeByName(fid,profilename);
    _profilearraysize=profilearraysize;
    _profilearray     = (med_int *) malloc (sizeof(med_int)*_profilearraysize);
    if ( MEDprofileRd(fid,profilename, _profilearray) <0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_PROFILE,profilename);
      goto ERROR;
    }
    _profilearrayfunc = _withprofilearray;
  } else {
    _profilearrayfunc = _identity;
    _profilearraysize = nentity;
  }


  if ( (filterarraysize <= 0) ) {
    _filterarrayfunc = _identity;
    _filterarraysize = _profilearraysize;
  } else {
    _filterarrayfunc = _withfilterarray;
    _filterarraysize = filterarraysize;
  }

  /*Dimensionement filtre interne et memspace */
  _fltmemsize[0] = _filterarraysize*nvaluesperentity*_dimutil;
  _fltmem        = (med_size *) malloc (sizeof(med_size)*_fltmemsize[0]);

  _onedimallvaluesmemoffset  = nentity*nvaluesperentity;
  _memspacesize[0]           = _onedimallvaluesmemoffset*nconstituentpervalue;

  if ( (_memspace[0] = H5Screate_simple (1,_memspacesize, NULL)) <0) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_MEMSPACE,MED_ERR_SIZE_MSG);
    ISCRUTE_size(*_memspacesize);
    goto ERROR;
  }


  /*Dimensionement profil interne et diskspace */
  _pfldisksize[0] = _fltmemsize[0];
  _pfldisk        = (med_size *) malloc (sizeof(med_size)*_pfldisksize[0]);

  _onedimallvaluesdiskoffset = _profilearraysize*nvaluesperentity;
  _diskspacesize[0]          = _onedimallvaluesdiskoffset*nconstituentpervalue;

  if ( (_diskspace[0] = H5Screate_simple (1,_diskspacesize, NULL)) <0) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DISKSPACE,MED_ERR_SIZE_MSG);
    ISCRUTE_size(*_diskspacesize);
    goto ERROR;
  }

  _index=0;
  for (_dim=_firstdim; _dim < _lastdim; ++_dim) {
    for (_i=0; _i < _filterarraysize; ++_i) {
      for (_j=0; _j < nvaluesperentity; ++_j) {
	_fltmem[_index] = _dim*_onedimallvaluesmemoffset +
	  _profilearrayfunc(_profilearray,_filterarrayfunc(filterarray,_i))*nvaluesperentity+_j  ;
	_pfldisk[_index] = _dim*_onedimallvaluesdiskoffset + _filterarrayfunc(filterarray,_i)*nvaluesperentity+_j;
#ifdef _DEBUG_
	printf("NoGlb :_fltmem[%d]=%llu -- _pfldisk[%d]=%llu \n",_index,_fltmem[_index],_index,_pfldisk[_index]);
#endif
	_index++;
      }
    }
  }

  /*Ce type de sélection n'est pas utilisable en parallélisme*/
  if ( H5Sselect_elements(_memspace[0] ,H5S_SELECT_SET,_fltmemsize[0], HDF5_SELECT_BUG _fltmem ) <0) {
    MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
    ISCRUTE_id(_memspace[0]);
    goto ERROR;
  }

  /*Ce type de sélection n'est pas utilisable en parallélisme*/
  if ( H5Sselect_elements(_diskspace[0] ,H5S_SELECT_SET,_pfldisksize[0], HDF5_SELECT_BUG _pfldisk ) <0) {
    MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_DISKSPACE,MED_ERR_ID_MSG);
    ISCRUTE_id(_diskspace[0]);
    goto ERROR;
  }


  free(_fltmem);       _fltmem=NULL;
  free(_pfldisk);      _pfldisk=NULL;
  free(_profilearray); _profilearray=NULL;

  if ( _MEDsetFilter(1,_memspace, _diskspace, nentity,
		     nvaluesperentity, nconstituentpervalue,
		     constituentselect, MED_NO_INTERLACE,
		     filterarraysize,profilearraysize,
		     storagemode, profilename, filter ) <0) {
    MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILTER,"");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  if (_fltmem)       free(_fltmem);
  if (_pfldisk)      free(_pfldisk);
  if (_profilearray) free(_profilearray);

  return _ret;
}
