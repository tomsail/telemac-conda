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
   \MEDfilterBlockOfEntityFullICNote
*/

med_err _MEDfilterBlockOfEntityFullICompactCr(const med_idt          fid,
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
  med_size   _startmem[1]={0}, _startmem_adim[1]={0}, _startmem_lastblock[1]={0};
  med_size   _startfile[1]={0}, _startfile_lastblock[1]={0};
  med_size   _stridemem[1]={0}, _stridefile[1]  ={0};
  med_size   _countmem [1]={0}, _countmemlastblock[1] ={0}, _countfilelastblock[1]={0};
  med_size   _memblocksize[1] ={0}, _memlastblocksize[1]={0}, _blocksize[1]={0} ;
  med_size   _fileblocksize[1]={0}, _filelastblocksize[1]={0};
  med_size   _onedimallvaluesfileoffset=0,_oneentitymemoffset=0;
  med_size   _nconstituentpervalue[1]={0};
  med_err    _ret=-1;
  int        _dim=0, _firstdim=0, _dimutil=0, _lastdim=0,_index=0 ;
  int        _anyprofil=0,_anylastblock=0;
  med_int    _profilearraysize=0,profilearraysize=0;

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
  if ( (_anyprofil=(strlen(profilename)) ) ) {
    profilearraysize = MEDprofileSizeByName(fid,profilename);
    _profilearraysize=profilearraysize;
  } else {
    _profilearraysize = nentity;
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
  if ( (count > 1) && (lastblocksize != blocksize) && lastblocksize ) /*Tester ou non si lastblocksize est bien > blocksize */
    {--_count;_anylastblock=1;_countmemlastblock[0]=1;_countfilelastblock[0]=1;}
/*   ISCRUTE(_start); */
/*   ISCRUTE(_count); */
/*   ISCRUTE(_countmemlastblock[0]); */
/*   ISCRUTE(_countfilelastblock[0]); */


  _oneentitymemoffset    = nvaluesperentity*nconstituentpervalue;
  _blocksize         [0] = blocksize*nvaluesperentity; /*!*nconstituentpervalue car on sélectionne par constituent*/
  _memblocksize      [0] = 1;
  _memspacesize      [0] = blocksize*_oneentitymemoffset*_count + _anylastblock*_oneentitymemoffset*lastblocksize;
  _stridemem         [0] = nconstituentpervalue;
  _countmem          [0] = _count*_blocksize[0];
  _countmemlastblock [0] *= lastblocksize*nvaluesperentity;

  _onedimallvaluesfileoffset = _profilearraysize*nvaluesperentity;
  _filespacesize     [0]     = _onedimallvaluesfileoffset*nconstituentpervalue;
  _stridefile        [0]     = stride*nvaluesperentity;
  _filelastblocksize [0]     = _countmemlastblock[0];

  /* ISCRUTE(_oneentitymemoffset    ); */
  /* ISCRUTE(_blocksize         [0] ); */
  /* ISCRUTE(_memblocksize      [0] ); */
  /* ISCRUTE_id(_memspacesize      [0] ); */
  /* ISCRUTE(_stridemem         [0] ); */
  /* ISCRUTE(_countmem          [0] ); */
  /* ISCRUTE(_countmemlastblock [0] ); */

  /* ISCRUTE(_onedimallvaluesfileoffset ); */
  /* ISCRUTE_int(_filespacesize     [0]     ); */
  /* ISCRUTE(_stridefile        [0]     ); */
  /* ISCRUTE(_filelastblocksize [0]     ); */

  if ( _memspacesize[0] > _filespacesize[0] ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_MEMSPACE,MED_ERR_SIZE_MSG);
    ISCRUTE_size(_memspacesize[0]);
    ISCRUTE_size(_filespacesize[0]);
    goto ERROR;
  }

  _index=0;
  for (_dim=_firstdim; _dim < _lastdim; _dim++) {

    if ( (_memspace[_index] = H5Screate_simple (1, _memspacesize, NULL)) <0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      MESSAGE(MED_ERR_SIZE_MSG);
      ISCRUTE_size(_memspacesize[0]);
      goto ERROR;
    }

     /*Mem: selection of one constituent for all entities (compact)*/
    _startmem      [0] = _dim;
    if ( H5Sselect_hyperslab (_memspace[_index], H5S_SELECT_SET, _startmem, _stridemem,
			      _countmem, _memblocksize) <0 ) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      goto ERROR;
    }
/*     ISCRUTE_id(_memspace[_index]); */


    /*Mem: Conditionally (_countmemlastblock) add a lastblock selection */
    _startmem_lastblock[0] = _startmem[0]+_stridemem[0]*_countmem[0];
/*     ISCRUTE(_startmem_lastblock[0]); */
    if ( H5Sselect_hyperslab (_memspace[_index], H5S_SELECT_OR, _startmem_lastblock, _stridemem,
			      _countmemlastblock, _memblocksize) <0 ) {
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
    if ( H5Sselect_hyperslab (_filespace[_index], H5S_SELECT_SET, _startfile,_stridefile ,
			      &_count, _blocksize ) <0) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILESPACE,MED_ERR_ID_MSG);
      ISCRUTE_int(_filespace[_index]);
      goto ERROR;
    }

    /*File: Conditionally (_count) add a specific lastblock selection */
    _startfile_lastblock[0] = _startfile[0]+_stridefile[0]*_count;
/*     ISCRUTE(_startfile_lastblock[0]); */
    if ( H5Sselect_hyperslab (_filespace[_index], H5S_SELECT_OR, _startfile_lastblock,_1 ,
			      _countfilelastblock, _filelastblocksize ) <0) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILESPACE,MED_ERR_ID_MSG);
      ISCRUTE_int(_filespace[_index]);
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
  if (  _MEDsetFilter(_dimutil,_memspace, _filespace, nentity,
		      nvaluesperentity, nconstituentpervalue,
		      constituentselect, MED_FULL_INTERLACE,
		      MED_NO_FILTER_SIZE,profilearraysize,
		      storagemode, profilename, filter ) <0) {
    MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILTER,"");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;
}
