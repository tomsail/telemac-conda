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


#include <med_config.h>
#include <med.h>
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
   \MEDfilterBlockOfEntityFullIGNote
*/

med_err _MEDfilterBlockOfEntityFullIGlobalCr(const med_idt          fid,
					     const med_int          nentity,
					     const med_int          nvaluesperentity,
					     const med_int          nconstituentpervalue,
					     const med_int          constituentselect,
					     const med_storage_mode storagemode,
					     const char * const     profilename,
					     const med_size         start,
					     const med_size         stride,
					     const med_size         count,
					     const med_size         blocksize,
					     const med_size         lastblocksize,
					     med_filter*  const     filter) {

  med_size   _start=start-1,_count=count;
  med_size   _1[1]={1};
  med_idt    _memspace [MED_MAX_FILTER_SPACES]= MED_MAX_FILTER_SPACES_INIT;
  med_idt    _filespace[MED_MAX_FILTER_SPACES]= MED_MAX_FILTER_SPACES_INIT;
  med_size   _memspacesize[1]={0},_filespacesize[1]={0};
  med_size   _startmem[1]={0}, _startmem_adim[1]={0}, _startmem_lastblock[1]={0};
  med_size   _startfile[1]={0}, _startfile_lastblock[1]={0};
  med_size   _stridemem[1]={0}, _stridefile[1]  ={0};
  med_size   _countmem [1]={0}, _anylastblock[1] ={0};
  med_size   _memblocksize[1] ={0}, _memlastblocksize[1]={0}, _memadimblocksize[1]={0};
  med_size   _fileblocksize[1]={0}, _filelastblocksize[1]={0};
  med_size   _onedimallvaluesfileoffset=0,_oneentitymemoffset=0;
  med_size   _nconstituentpervalue[1]={0};
  med_err    _ret=-1;
  int        _dim=0, _firstdim=0, _dimutil=0, _lastdim=0,_index=0 ;


  if ( constituentselect != MED_ALL_CONSTITUENT) {
    _firstdim = constituentselect-1;
    _lastdim  = constituentselect;
    _dimutil  = 1;
  } else {
    _firstdim = 0;
    _lastdim  = nconstituentpervalue;
    _dimutil  = nconstituentpervalue;
  }

  /*Pas de gestion de profil possible dans ce mode
    car on n'obtiendrait pas une hyperslab selection */
  if ( strlen(profilename) ) {
    MED_ERR_(_ret,MED_ERR_NULL,MED_ERR_PROFILE,profilename);
    goto ERROR;
  }

  /*No selection*/
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
	ISCRUTE_id(_filespace[_index]);
	goto ERROR;
      }
      ++_index;
    }
    goto SAVEFILTER;
  }

  _nconstituentpervalue[0] = nconstituentpervalue;
  /*Trigger the specific lastblock selection */
  if ( (count > 1) && (lastblocksize != blocksize) && lastblocksize )
    {--_count;_anylastblock[0]=1;}
/*   ISCRUTE(_start); */
/*   ISCRUTE(_count); */
/*   ISCRUTE(_anylastblock[0]); */

  _oneentitymemoffset   = nvaluesperentity*_nconstituentpervalue[0];
  _startmem         [0] = _start*_oneentitymemoffset;
  _memspacesize     [0] = nentity*_oneentitymemoffset;
  _stridemem        [0] = stride*_oneentitymemoffset;
  _memblocksize     [0] = blocksize*_oneentitymemoffset;
  _memadimblocksize   [0] = 1;
  _countmem           [0] = nentity*nvaluesperentity;
  _startmem_lastblock [0] = _startmem[0]+_stridemem[0]*_count;
  _memlastblocksize   [0] = lastblocksize*_oneentitymemoffset;

  _onedimallvaluesfileoffset = nentity*nvaluesperentity;
  _filespacesize    [0] = _onedimallvaluesfileoffset*_nconstituentpervalue[0];
  _fileblocksize    [0] = blocksize*nvaluesperentity;
  _stridefile       [0] = stride*nvaluesperentity;
  _filelastblocksize[0] = lastblocksize*nvaluesperentity;

/*   ISCRUTE(_oneentitymemoffset    ); */
/*   ISCRUTE(_memspacesize      [0] ); */
/*   ISCRUTE(_startmem          [0] ); */
/*   ISCRUTE(_stridemem     [0] ); */
/*   ISCRUTE(_count                 ); */
/*   ISCRUTE(_memblocksize     [0] ); */
/*   ISCRUTE(_startmem_lastblock[0] ); */
/*   ISCRUTE(_memlastblocksize  [0] ); */


/*   ISCRUTE(_nconstituentpervalue [0] ); */
/*   ISCRUTE(_memadimblocksize           [0] ); */
/*   ISCRUTE(_countmem            [0] ); */

/*   ISCRUTE(_onedimallvaluesfileoffset ); */
/*   ISCRUTE(_filespacesize    [0] ); */
/*   ISCRUTE(_startfile       [0] ); */
/*   ISCRUTE(_stridefile      [0] ); */
/*   ISCRUTE(_count                 ); */
/*   ISCRUTE(_fileblocksize    [0] ); */
/*   ISCRUTE(_filelastblocksize[0] ); */

  _index=0;
  for (_dim=_firstdim; _dim < _lastdim; _dim++) {

    if ( (_memspace[_index] = H5Screate_simple (1, _memspacesize, NULL)) <0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      MESSAGE(MED_ERR_SIZE_MSG);
      ISCRUTE_size(_memspacesize[0]);
      goto ERROR;
    }

    /*Mem: Blocks selection with all constituents*/
    if ( H5Sselect_hyperslab (_memspace[_index], H5S_SELECT_SET, _startmem, _stridemem,
			      &_count, _memblocksize) <0 ) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      goto ERROR;
    }

    /*Mem: Conditionally (_anylastblock) add a lastblock selection */
   if ( H5Sselect_hyperslab (_memspace[_index], H5S_SELECT_OR, _startmem_lastblock,_1,
			      _anylastblock, _memlastblocksize) <0 ) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      goto ERROR;
    }

    /*Mem: One constituent selection*/
    _startmem_adim[0] = _startmem[0] + _dim;
/*     ISCRUTE(_startmem_adim[0]); */
    if ( H5Sselect_hyperslab (_memspace[_index], H5S_SELECT_AND, _startmem_adim, _nconstituentpervalue,
			      _countmem, _memadimblocksize) <0 ) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      goto ERROR;
    }


    if ( (_filespace[_index] = H5Screate_simple(1,_filespacesize,NULL)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILESPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_filespace[_index]);
      MESSAGE(MED_ERR_SIZE_MSG);
      ISCRUTE_size(_filespacesize[0]);
      goto ERROR;
    }

    /*File: Blocks selection of one constituent*/
    _startfile[0] = _dim*_onedimallvaluesfileoffset+_start*nvaluesperentity;
/*     ISCRUTE(_startfile[0]); */
    if ( H5Sselect_hyperslab (_filespace[_index], H5S_SELECT_SET, _startfile,_stridefile ,
			      &_count, _fileblocksize ) <0) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILESPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_filespace[_index]);
      goto ERROR;
    }

    /*File: Conditionally (_count) add a specific lastblock selection */
    _startfile_lastblock[0] = _startfile[0]+_stridefile[0]*_count;
/*     ISCRUTE(_startfile_lastblock[0]); */
    if ( H5Sselect_hyperslab (_filespace[_index], H5S_SELECT_OR, _startfile_lastblock,_1 ,
			      _anylastblock, _filelastblocksize ) <0) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILESPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_filespace[_index]);
      goto ERROR;
    }

    if (_index > MED_MAX_FILTER_SPACES) {
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_PARAMETER,"");
      ISCRUTE_int(_index);
      ISCRUTE_size(_nconstituentpervalue[0]);
      ISCRUTE_int(MED_MAX_FILTER_SPACES);
      goto ERROR;
    } else
      ++_index;
  }

 SAVEFILTER:
  if (  _MEDsetFilter(_dimutil,_memspace, _filespace, nentity,
		      nvaluesperentity, nconstituentpervalue,
		      constituentselect, MED_FULL_INTERLACE,
		      MED_NO_FILTER_SIZE,MED_NO_PROFILE_SIZE,
		      storagemode, MED_NO_PROFILE, filter ) <0) {
    MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILTER,"");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;
}
