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


/**\ingroup MEDfilter
   \MEDselectAllEntitiesBrief
   \param fid \fid
   \param nentity \nentity \nentityMEDfilterCm
   \param nvaluesperentity \nvaluesperentity \nvaluesperentityMEDfilterCm \nvaluesperentityMEDfilterEx
   \param nconstituentpervalue \nconstituentpervalue \nconstituentpervalueMEDfilterEx
   \param constituentselect \constituentselect \constituentselectMEDfilterEx
   \retval \filter \filterMEDfilterCm
   \return \error

   \details
   \MEDselectAllEntitiesDetails
   \par Remarques
*/

med_err _MEDselectAllEntitiesFullI(const med_idt          fid,
			      const med_int          nentity,
			      const med_int          nvaluesperentity,
			      const med_int          nconstituentpervalue,
			      const med_int          constituentselect,
			      med_filter*    const   filter) {

  med_idt    _memspace[MED_MAX_FILTER_SPACES]= MED_MAX_FILTER_SPACES_INIT;
  med_idt    _diskspace[MED_MAX_FILTER_SPACES]= MED_MAX_FILTER_SPACES_INIT;
  med_size   _memspacesize[1]={0},_diskspacesize[1]={0};
  med_size   _stride[1]={0};
  med_size   _start_mem[1]={0},_start_disk[1]={0};
  med_err    _ret=-1;
  int        _dim=0, _firstdim=0, _dimutil=0, _lastdim=0, _index=0 ;
  med_size   _onedimallvaluesdiskoffset[1]={0};

  /* Crée un filtre vide valide */
  if ( ! nentity) {
    _memspace[0]  = H5Screate(H5S_NULL);
    _diskspace[0] = H5Screate(H5S_NULL);
    goto _CREATE_FILTER;
  }

  if ( constituentselect != MED_ALL_CONSTITUENT) {
    _firstdim = constituentselect-1;
    _lastdim  = constituentselect;
    _dimutil  = 1;
  } else {
    _firstdim = 0;
    _lastdim  = nconstituentpervalue;
    _dimutil  = nconstituentpervalue;
  }

  _onedimallvaluesdiskoffset[0] = nentity*nvaluesperentity;
  _memspacesize[0]              = _onedimallvaluesdiskoffset[0]*nconstituentpervalue;
  _diskspacesize[0]             = _memspacesize[0];
  _stride[0]                    = nconstituentpervalue;

  /*On réalise les selections par composantes pour placer  */
  for (_dim=_firstdim; _dim < _lastdim; _dim++) {

    /*On crée un espace qui contient l'ensemble des composantes*/
    if ( (_memspace[_index] = H5Screate_simple (1, _memspacesize, NULL)) <0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      MESSAGE(MED_ERR_SIZE_MSG);
      ISCRUTE_size(_memspacesize[0]);
      goto ERROR;
    }

    /*Mais on selectionne uniquement la composante a placer dans l'espace mémoire*/
    _start_mem[0] = _dim;
    if ( H5Sselect_hyperslab (_memspace[_index], H5S_SELECT_SET, _start_mem, _stride,
			      _onedimallvaluesdiskoffset, NULL) <0 ) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_memspace[_index]);
      goto ERROR;
    }
/*     ISCRUTE_id(_memspace[_index]); */

    if ( (_diskspace[_index] = H5Screate_simple(1,_diskspacesize,NULL)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DISKSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_diskspace[_index]);
      MESSAGE(MED_ERR_SIZE_MSG);
      ISCRUTE_size(_diskspacesize[0]);
      goto ERROR;
    }
    _start_disk[0] = _dim*_onedimallvaluesdiskoffset[0];
    if ( H5Sselect_hyperslab (_diskspace[_index], H5S_SELECT_SET, _start_disk, NULL,
			      _onedimallvaluesdiskoffset, NULL) <0) {
      MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_DISKSPACE,MED_ERR_ID_MSG);
      ISCRUTE_id(_diskspace[_index]);
      goto ERROR;
    }

/*     ISCRUTE(_diskspace[_index]); */

    if (_index > MED_MAX_FILTER_SPACES) {
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_PARAMETER,"");
      ISCRUTE_int(_index);
      ISCRUTE(nconstituentpervalue);
      ISCRUTE_int(MED_MAX_FILTER_SPACES);
      goto ERROR;
    } else
      ++_index;
  }

 _CREATE_FILTER:
  if ( _MEDsetFilter(_dimutil,_memspace, _diskspace, nentity,
		     nvaluesperentity, nconstituentpervalue,
		     constituentselect, MED_FULL_INTERLACE,
		     MED_NO_FILTER_SIZE, MED_NO_PROFILE_SIZE,
		     MED_UNDEF_STMODE, MED_NO_PROFILE, filter ) <0) {
    MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILTER,"");
    goto ERROR;
  }

  /* ISCRUTE_size(_memspacesize[0]); */
  /* ISCRUTE((*filter).nentity              ); */
  /* ISCRUTE((*filter).nvaluesperentity     ); */
  /* ISCRUTE((*filter).nconstituentpervalue ); */
  /* ISCRUTE((*filter).constituentselect       ); */
  /* ISCRUTE((*filter).switchmode              ); */
  /* ISCRUTE((*filter).filterarraysize         ); */
  /* ISCRUTE((*filter).profilearraysize        ); */
  /* ISCRUTE((*filter).storagemode             ); */
  /* SSCRUTE((*filter).profilename             ); */

  _ret = 0;

 ERROR:

  return _ret;
}
