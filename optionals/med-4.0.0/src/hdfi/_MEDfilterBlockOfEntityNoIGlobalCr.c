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
   \MEDfilterBlockOfEntityBrief
   \param fid \fid
   \param nentity \nentity \nentityMEDfilterCm
   \param nvaluesperentity \nvaluesperentity \nvaluesperentityMEDfilterCm \nvaluesperentityMEDfilterEx
   \param nconstituentpervalue \nconstituentpervalue \nconstituentpervalueMEDfilterEx
   \param constituentselect \constituentselect \constituentselectMEDfilterEx
   \param storagemode \storagemode
   \param profilename \profilename
   \param start \start
   \param stride \stride
   \param count \count
   \param blocksize \blocksize
   \retval \filter \filterMEDfilterCm
   \return \error

   \details
   \MEDfilterBlockOfEntityDetails
   \par Remarques
   \MEDfilterBlockOfEntityNoIGNote
*/

med_err _MEDfilterBlockOfEntityNoIGlobalCr(const med_idt          fid,
					   const med_int          nentity,
					   const med_int          nvaluesperentity,
					   const med_int          nconstituentpervalue,
					   const med_int          constituentselect,
					   const med_storage_mode storagemode,
					   const char * const     profilename,
					   const med_size  start,
					   const med_size  stride,
					   const med_size  count,
					   const med_size  blocksize,
					   const med_size  lastblocksize,
					   med_filter*    const   filter) {

  med_size   _start=start-1,_count=count;
  med_size   _1[1]={1};
  med_idt    _memspace [MED_MAX_FILTER_SPACES]= MED_MAX_FILTER_SPACES_INIT;
  med_idt    _filespace[MED_MAX_FILTER_SPACES]= MED_MAX_FILTER_SPACES_INIT;
  med_size   _memspacesize[1]={0},_filespacesize[1]={0};
  med_size   _startmem[1]={0},_stride[1]={0},_startmem_lastblock[1]={0};
  med_size   _blocksize[1]={0},_memlastblocksize[1]={0};
  med_size   _onedimallvaluesfileoffset=0;
  med_err    _ret=-1;
  int        _dim=0, _firstdim=0, _dimutil=0, _lastdim=0,_index=0 ;
  med_size   _nconstituentpervalue[1]={0},_anylastblock[1]={0};

  /*   if ( constituentselect != MED_ALL_CONSTITUENT) { */
  /*       MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_PARAMETER,MED_ERR_VALUE_MSG); */
  /*       ISCRUTE(constituentselect); */
  /*       SSCRUTE(MED_ERR_FORBIDDEN_MSG); */
  /*       goto ERROR; */
  /*   } */

  if ( constituentselect != MED_ALL_CONSTITUENT) {
    _firstdim = constituentselect-1;
    _lastdim  = constituentselect;
    _dimutil  = 1;
  } else {
    _firstdim = 0;
    _lastdim  = nconstituentpervalue;
    _dimutil  = nconstituentpervalue;
  }

  /*Pas de gestion de profil possible dans ce mode GLOBAL */
  if ( strlen(profilename) ) {
    MED_ERR_(_ret,MED_ERR_NULL,MED_ERR_PARAMETER,profilename);
    goto ERROR;
  }

  if ( (count < 1 ) || ( blocksize < 1 ) ) {
    _index=0;
    for (_dim=_firstdim; _dim < _lastdim; _dim++) {
      if ( (_memspace[_index]=H5Screate(H5S_NULL)) < 0 ) {
	MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
	ISCRUTE_id(_memspace[_index]);
	goto ERROR;
      }
      if ( (_filespace[_index]=H5Screate(H5S_NULL)) < 0 ) {
	MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILESPACE,MED_ERR_ID_MSG);
	ISCRUTE_int(_filespace[_index]);
	goto ERROR;
      }
      ++_index;
    }
    goto SAVEFILTER;
  }


  _nconstituentpervalue[0] = nconstituentpervalue;
  /*Trigger the specific lastblock selection */
  if ( (count > 1) && (lastblocksize != blocksize) && lastblocksize ) /*Tester ou non si lastblocksize est bien > blocksize */
    {--_count;_anylastblock[0]=1;}
/*   ISCRUTE(_start); */
/*   ISCRUTE(_count); */
/*   ISCRUTE(_anylastblock[0]); */

  _onedimallvaluesfileoffset = nentity*nvaluesperentity;
  _memspacesize [0]  = _onedimallvaluesfileoffset*nconstituentpervalue;
  _filespacesize[0] = _memspacesize[0];
  _blocksize    [0] = blocksize*nvaluesperentity;
  _stride       [0] = stride*nvaluesperentity;
  _memlastblocksize   [0] = lastblocksize*nvaluesperentity;

/* ISCRUTE(_blocksize    [0] ); */
/* ISCRUTE(_stride       [0] ); */

  _index=0;
  for (_dim=_firstdim; _dim < _lastdim; _dim++) {

    if ( (_memspace[_index] = H5Screate_simple (1, _memspacesize, NULL)) <0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_MEMSPACE,MED_ERR_SIZE_MSG);
      ISCRUTE_size(*_memspacesize);
      ISCRUTE_int(_memspace[_index]);
      goto ERROR;
    }

    _startmem [0] = _start*nvaluesperentity + _dim*_onedimallvaluesfileoffset;
/*     ISCRUTE(_startmem [0] ); */
    if ( H5Sselect_hyperslab (_memspace[_index], H5S_SELECT_SET, _startmem, _stride,
			      &_count, _blocksize) <0 ) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      goto ERROR;
    }

    /*Mem: Conditionally (_anylastblock) add a lastblock selection */
    _startmem_lastblock [0] = _startmem[0]+_stride[0]*_count;
    if ( H5Sselect_hyperslab (_memspace[_index], H5S_SELECT_OR, _startmem_lastblock,_1,
			      _anylastblock, _memlastblocksize) <0 ) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      goto ERROR;
    }

    if ( (_filespace[_index] = H5Scopy(_memspace[_index])) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILESPACE,MED_ERR_SIZE_MSG);
      ISCRUTE_size(*_memspacesize);
      ISCRUTE_id(_memspace[_index]);
      ISCRUTE_id(_filespace[_index]);
      goto ERROR;
    }

/*     ISCRUTE_int(_filespace[_index]); */

    if (_index > MED_MAX_FILTER_SPACES) {
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_PARAMETER,"");
      ISCRUTE_int(_index);
      ISCRUTE(nconstituentpervalue);
      ISCRUTE_int(MED_MAX_FILTER_SPACES);
      goto ERROR;
    } else
      ++_index;
  }

 SAVEFILTER:
  if ( _MEDsetFilter(_dimutil,_memspace, _filespace, nentity,
		     nvaluesperentity, nconstituentpervalue,
		     constituentselect, MED_NO_INTERLACE,
		     MED_NO_FILTER_SIZE, MED_NO_PROFILE_SIZE,
		     MED_UNDEF_STMODE, MED_NO_PROFILE, filter ) <0) {
    MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILTER,"");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;
}
