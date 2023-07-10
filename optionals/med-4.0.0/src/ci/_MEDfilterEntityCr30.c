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

void _MEDfilterEntityCr30(int dummy, ...)
{


  med_err    _ret=-1;
  med_int    _profilearraysize=0;
  med_storage_mode  _storagemode;

  MED_VARGS_DECL(const, med_idt           , , fid                     );
  MED_VARGS_DECL(const, med_int           , , nentity              );
  MED_VARGS_DECL(const, med_int           , , nvaluesperentity     );
  MED_VARGS_DECL(const, med_int           , , nconstituentpervalue );
  MED_VARGS_DECL(const, med_int           , , constituentselect       );
  MED_VARGS_DECL(const, med_switch_mode   , , switchmode              );
  MED_VARGS_DECL(const, med_storage_mode  , , storagemode             );
  MED_VARGS_DECL(const, char * , const      , profilename             );
  MED_VARGS_DECL(const, med_int           , , filterarraysize         );
  MED_VARGS_DECL(const, med_int * , const   , filterarray             );
  MED_VARGS_DECL(, med_filter*, const       , filter                  );
  MED_VARGS_DECL(, med_err *               ,, fret                    );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid                     );
  MED_VARGS_DEF(const, med_int           , , nentity              );
  MED_VARGS_DEF(const, med_int           , , nvaluesperentity     );
  MED_VARGS_DEF(const, med_int           , , nconstituentpervalue );
  MED_VARGS_DEF(const, med_int           , , constituentselect       );
  MED_VARGS_DEF(const, med_switch_mode   , , switchmode              );
  MED_VARGS_DEF(const, med_storage_mode  , , storagemode             );
  MED_VARGS_DEF(const, char * , const      , profilename             );
  MED_VARGS_DEF(const, med_int           , , filterarraysize         );
  MED_VARGS_DEF(const, med_int * , const  , filterarray             );
  MED_VARGS_DEF(, med_filter*, const       , filter                  );
  MED_VARGS_DEF(, med_err *               ,, fret                    );

  _storagemode=storagemode;

  /* TRUC BIZZARD AVEC LES ANCIENS PROFILS, A VERIFIER */
  NOFINALBLANK(profilename,ERROR);

  if ( (filterarraysize <= 0) && (filterarray != NULL ) ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_FILTER,MED_ERR_ARRAY_MSG);
    ISCRUTE(filterarraysize);
    MESSAGE(MED_ERR_NULL_MSG);ISCRUTE_long((long)filterarray);
    goto ERROR;
  }

  if ( strlen(profilename) ) {
    _profilearraysize = MEDprofileSizeByName(fid,profilename);
    if ( filterarraysize > _profilearraysize  ) {
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_FILTER,MED_ERR_GSIZE_MSG);ISCRUTE(filterarraysize);
      SSCRUTE(profilename);ISCRUTE(_profilearraysize);
      goto ERROR;
    }
  } else {
    /*Faux si un filterarray est présent !*/
/*     _storagemode = MED_GLOBAL_STMODE; */
  }

  /* Verify constituentselect is between [0, nconstituentpervalue] ( 0 is MED_ALL_CONSTITUENT ) */
  if ( constituentselect > nconstituentpervalue) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_FILTER,MED_ERR_VALUE_MSG);ISCRUTE(constituentselect);
    ISCRUTE(nconstituentpervalue);
    goto ERROR;
  }

  /* Verify nvaluesperentity > 0 */
  if ( nvaluesperentity  < 1 ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_FILTER,MED_ERR_VALUE_MSG);
    ISCRUTE(nvaluesperentity);
    goto ERROR;
  }

  switch(switchmode) {
  case MED_FULL_INTERLACE :

    if ( (!strlen(profilename)) && (filterarraysize == MED_NO_FILTER_SIZE)  ) {

      if ( _MEDselectAllEntitiesFullI(fid, nentity, nvaluesperentity, nconstituentpervalue,
				 constituentselect, filter) < 0) {
	MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
	MESSAGE("MED_FULL_INTERLACE, MED_NO_PROFILE, MED_NO_FILTER");
	goto ERROR;
      }

    } else {

      switch(_storagemode) {
      case MED_GLOBAL_STMODE :

	if ( _MEDfilterEntityFullIGlobalCr(fid,nentity,nvaluesperentity,nconstituentpervalue,constituentselect,
					   _storagemode,profilename,filterarraysize,filterarray,filter) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
	  MESSAGE("MED_FULL_INTERLACE, MED_GLOBAL_STMODE");
	  goto ERROR;
	}
	break;
      case MED_COMPACT_STMODE :
	if ( _MEDfilterEntityFullICompactCr(fid,nentity,nvaluesperentity,nconstituentpervalue,constituentselect,
					    _storagemode,profilename,filterarraysize,filterarray,filter) < 0 ) {
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
	ISCRUTE_int(_storagemode);
	ISCRUTE(_profilearraysize);
	goto ERROR;
	break;
      }
    }
    break;
  case MED_NO_INTERLACE :

    if ( (!strlen(profilename)) && (filterarraysize == MED_NO_FILTER_SIZE)  ) {

      if ( _MEDselectAllEntitiesNoI(fid, nentity, nvaluesperentity, nconstituentpervalue,
				      constituentselect, filter) < 0) {
	MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
	MESSAGE("MED_NO_INTERLACE, MED_NO_PROFILE, MED_NO_FILTER");
	goto ERROR;
      }

    } else {

      switch(_storagemode) {

      case MED_GLOBAL_STMODE :

	if ( _MEDfilterEntityNoIGlobalCr(fid,nentity,nvaluesperentity,nconstituentpervalue,constituentselect,
					   _storagemode,profilename,filterarraysize,filterarray,filter) < 0 ) {
	  MED_ERR_(_ret,MED_ERR_SELECT,MED_ERR_FILTER,MED_ERR_MODE_MSG);
	  MESSAGE("MED_NO_INTERLACE, MED_GLOBAL_STMODE");
	  goto ERROR;
	}
	break;

      case MED_COMPACT_STMODE :
	if ( _MEDfilterEntityNoICompactCr(fid,nentity,nvaluesperentity,nconstituentpervalue,constituentselect,
					    _storagemode,profilename,filterarraysize,filterarray,filter) < 0 ) {
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
	ISCRUTE_int(_storagemode);
	ISCRUTE(_profilearraysize);
	goto ERROR;
	break;
      }

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

  va_end(params);
  *fret = _ret;

  return;
}
