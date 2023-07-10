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
   \brief \MEDfilterBlockOfEntityBrief
   \param fid \fid
   \param nentity \nentity \nentityMEDfilterCm
   \param nvaluesperentity \nvaluesperentity \nvaluesperentityMEDfilterCm \nvaluesperentityMEDfilterEx
   \param nconstituentpervalue \nconstituentpervalue \nconstituentpervalueMEDfilterEx
   \param constituentselect \constituentselect \constituentselectMEDfilterEx
   \param switchmode \switchmode
   \param storagemode \storagemode
   \param profilename \profilename
   \param start \start
   \param stride \stride
   \param count \count
   \param blocksize \blocksize
   \param lastblocksize \lastblocksize
   \retval filter \filter \filterMEDfilterCm
   \return \error

   \details
   \MEDfilterBlockOfEntityDetails

   \par Remarques
   \MEDfilterBlockOfEntityNote
*/

med_err MEDfilterBlockOfEntityCr(const med_idt          fid,
				 const med_int          nentity,
				 const med_int          nvaluesperentity,
				 const med_int          nconstituentpervalue,
				 const med_int          constituentselect,
				 const med_switch_mode  switchmode,
				 const med_storage_mode storagemode,
				 const char * const     profilename,
				 const med_size         start,
				 const med_size         stride,
				 const med_size         count,
				 const med_size         blocksize,
				 const med_size         lastblocksize,
				 med_filter*    const   filter) {


  med_int    _profilearraysize=0,_maxentitynum=0;
  med_err    _ret=-1;
  med_size   _start=start-1;

  NOFINALBLANK(profilename,ERROR);

  if ( start == 0) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_FILTER,"");
    ISCRUTE_size(start);
    goto ERROR;
  }

  _maxentitynum=_start+(count-1)*(stride);

  if ( strlen(profilename) ) {
    _profilearraysize = MEDprofileSizeByName(fid,profilename);
    if ( _maxentitynum  > _profilearraysize  ) {
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_FILTER,MED_ERR_GSIZE_MSG);ISCRUTE(_maxentitynum);
      SSCRUTE(profilename);ISCRUTE(_profilearraysize);
      ISCRUTE_size(start);ISCRUTE_size(stride);ISCRUTE_size(count);ISCRUTE_size(blocksize);
      goto ERROR;
    }
  }

  /* Verify constituentselect is between [0, nconstituentpervalue] ( 0 is MED_ALL_CONSTITUENT ) */
  if ( constituentselect > nconstituentpervalue) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_FILTER,MED_ERR_VALUE_MSG);ISCRUTE(constituentselect);
    ISCRUTE(nconstituentpervalue);
    goto ERROR;
  }

  switch(switchmode) {
  case MED_FULL_INTERLACE :


    switch(storagemode) {  /* switch Interlace */
    case MED_GLOBAL_STMODE :

      if ( _MEDfilterBlockOfEntityFullIGlobalCr(fid,nentity,nvaluesperentity,nconstituentpervalue,constituentselect,
						storagemode,profilename,start,stride,count,blocksize,lastblocksize,filter) < 0 ) {
	MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
	MESSAGE("MED_FULL_INTERLACE, MED_GLOBAL_STMODE");
	goto ERROR;
      }
      break;
    case MED_COMPACT_STMODE :
      if ( _MEDfilterBlockOfEntityFullICompactCr(fid,nentity,nvaluesperentity,nconstituentpervalue,constituentselect,
						 storagemode,profilename,start,stride,count,blocksize,lastblocksize,filter) < 0 ) {
	MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
	MESSAGE("MED_FULL_INTERLACE, MED_COMPACT_STMODE");
	goto ERROR;
      }
      break;
    default:
      MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
      MESSAGE("MED_UNDEF_STMODE");
      MESSAGE("MED_FULL_INTERLACE");
      SSCRUTE(profilename);
      ISCRUTE_int(storagemode);
      ISCRUTE(_profilearraysize);
      goto ERROR;
      break;
    }

    break;
  case MED_NO_INTERLACE :

    switch(storagemode) {

    case MED_GLOBAL_STMODE :

      if ( _MEDfilterBlockOfEntityNoIGlobalCr(fid,nentity,nvaluesperentity,nconstituentpervalue,constituentselect,
					      storagemode,profilename,start,stride,count,blocksize,lastblocksize,filter) < 0 ) {
	MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
	MESSAGE("MED_NO_INTERLACE, MED_GLOBAL_STMODE");
	goto ERROR;
      }
      break;

    case MED_COMPACT_STMODE :
      if ( _MEDfilterBlockOfEntityNoICompactCr(fid,nentity,nvaluesperentity,nconstituentpervalue,constituentselect,
					       storagemode,profilename,start,stride,count,blocksize,lastblocksize,filter) < 0 ) {
	MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
	MESSAGE("MED_NO_INTERLACE, MED_COMPACT_STMODE");
	goto ERROR;
      }
      break;

    default:
      MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
      MESSAGE("MED_UNDEF_STMODE");
      MESSAGE("MED_NO_INTERLACE");
      SSCRUTE(profilename);
      ISCRUTE_int(storagemode);
      ISCRUTE(_profilearraysize);
      goto ERROR;
      break;
    }

    break;

  default:
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_INTERLACINGMODE,MED_ERR_VALUE_MSG);
    ISCRUTE_int(switchmode);
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  /*   if ( _memspace ) if ( (_ret = H5Sclose(_memspace)) < 0) { */
  /*     MESSAGE("Impossible de fermer le memspace : "); */
  /*     ISCRUTE(_memspace);  _ret = -1; */
  /*   } */

  return _ret;
}
