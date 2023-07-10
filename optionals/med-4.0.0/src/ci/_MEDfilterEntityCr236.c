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

void _MEDfilterEntityCr236(int dummy, ...)
{

  med_err           _ret=-1;
  med_int           _profilearraysize=0;
  med_storage_mode  _storagemode;
  int               _i = 0;

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
  MED_VARGS_DEF(const, med_int * , const   , filterarray             );
  MED_VARGS_DEF(, med_filter*, const       , filter                  );
  MED_VARGS_DEF(, med_err *               ,, fret                    );

  _storagemode=storagemode;

  NOFINALBLANK(profilename,ERROR);

  if ( (filterarraysize <= 0) && (filterarray != NULL ) ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_FILTER,MED_ERR_ARRAY_MSG);
    ISCRUTE(filterarraysize);
    MESSAGE(MED_ERR_NULL_MSG);ISCRUTE_long((long int) filterarray);
    goto ERROR;
  }

  if ( strlen(profilename) ) {
    if ( filterarraysize != 0 ) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_FILTER,"");
      SSCRUTE(profilename);ISCRUTE(filterarraysize);
      goto ERROR;
    } else {
      _profilearraysize = MEDprofileSizeByName(fid,profilename);
      if ( filterarraysize > _profilearraysize  ) {
	MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_FILTER,MED_ERR_GSIZE_MSG);ISCRUTE(filterarraysize);
	SSCRUTE(profilename);ISCRUTE(_profilearraysize);
	goto ERROR;
      }
    }
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

  if (  _MEDsetFilter(0,
		      NULL,
		      NULL,
		      nentity,
		      nvaluesperentity,
		      nconstituentpervalue,
		      constituentselect,
		      switchmode,
		      filterarraysize,
		      _profilearraysize,
		      storagemode,
		      profilename,
		      filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDsetFilter");
    goto ERROR;
  }

  /*TODO : Typer filterarray en med_size*/
  if ( filterarray && ( filterarraysize > 0 ) ) {
    (*filter).filterarray23v30 = (med_size *) malloc (sizeof(med_size)*filterarraysize);
    for (_i=0;_i< filterarraysize;++_i)
      (*filter).filterarray23v30[_i] = (med_size) (filterarray[_i]);
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
