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
   \MEDselectAllEntitiesNoIBrief
   \param fid \fid
   \param nentity \nentity \nentityMEDfilterCm
   \param nvaluesperentity \nvaluesperentity \nvaluesperentityMEDfilterCm \nvaluesperentityMEDfilterEx
   \param nconstituentpervalue \nconstituentpervalue \nconstituentpervalueMEDfilterEx
   \param constituentselect \constituentselect \constituentselectMEDfilterEx \relativenumbering
   \retval \filter \filterMEDfilterCm
   \return \error

   \details
   \MEDselectAllEntitiesNoIDetails
   \par Remarques
*/

med_err _MEDselectAllEntitiesNoI(const med_idt          fid,
				 const med_int          nentity,
				 const med_int          nvaluesperentity,
				 const med_int          nconstituentpervalue,
				 const med_int          constituentselect,
				 med_filter*    const   filter) {

  med_idt    _memspace[1]={0},_diskspace[1]={0};
  med_size   _memspacesize[1],_diskspacesize[1];
  med_size   _start_mem[1]={0};
  med_err    _ret=-1;
  med_size   _onedimallvaluesdiskoffset[1]={0};
  med_size   _profilearraysize[1]={0};

  /*TODO : propager cette modif aux autres filtres*/
  if ( ! nentity) {
    _memspace[0]  = H5Screate(H5S_NULL);
    _diskspace[0] = H5Screate(H5S_NULL);
    goto _CREATE_FILTER;
  }

  _onedimallvaluesdiskoffset[0] = nentity*nvaluesperentity;
  _memspacesize[0]  = _onedimallvaluesdiskoffset[0]*nconstituentpervalue;
  _diskspacesize[0] = _memspacesize[0];

  if ( (_memspace[0] = H5Screate_simple (1, _memspacesize, NULL)) <0) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_MEMSPACE,MED_ERR_SIZE_MSG);
    ISCRUTE_size(*_memspacesize);
    ISCRUTE_int(_memspace[0]);
    goto ERROR;
  }


  if ( constituentselect != MED_ALL_CONSTITUENT) {
    _start_mem[0] = (constituentselect-1)*_onedimallvaluesdiskoffset[0];
    _profilearraysize[0]=_onedimallvaluesdiskoffset[0];
  } else {
    _start_mem[0]       =  0;
    _profilearraysize[0]=_memspacesize[0];
  };

  if ( H5Sselect_hyperslab (_memspace[0], H5S_SELECT_SET, _start_mem, NULL,
			    _profilearraysize, NULL) <0 ) {
    MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_MEMSPACE,MED_ERR_ID_MSG);
    ISCRUTE_id(_memspace[0]);
    goto ERROR;
  }
/*   ISCRUTE_int(_memspace[0]); */


  if ( (_diskspace[0] = H5Scopy(_memspace[0])) < 0) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DISKSPACE,MED_ERR_SIZE_MSG);
    ISCRUTE_size(*_memspacesize);
    ISCRUTE_id(_memspace[0]);
    ISCRUTE_id(_diskspace[0]);
    goto ERROR;
  }

 _CREATE_FILTER:
  if ( _MEDsetFilter(1,_memspace, _diskspace, nentity,
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
