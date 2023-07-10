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

#include <string.h>
#include <stdlib.h>

void _MEDprofileRd30(int dummy, ...) {



  med_idt        _pfid=0;
  med_err        _ret=-1;
  char           _path[MED_PROFILE_GRP_SIZE+MED_NAME_SIZE+1]=MED_PROFILE_GRP;
  med_filter     _filter        = MED_FILTER_INIT;
  med_int        _nentity=0;


  MED_VARGS_DECL(const, med_idt         , , fid          );
  MED_VARGS_DECL(const, char    * , const , profilename  );
  MED_VARGS_DECL(, med_int *, const , profilearray       );
  MED_VARGS_DECL(, med_err *       ,, fret               );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt         , , fid          );
  MED_VARGS_DEF(const, char    * , const , profilename  );
  MED_VARGS_DEF(, med_int *, const , profilearray       );
  MED_VARGS_DEF(, med_err *       ,, fret               );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * ouverture du groupe /PROFILS/"nom"
   */
  strcat(_path,profilename);

  if ((_pfid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_PROFILE_GRP);
    SSCRUTE(_path); goto ERROR;
  }

  /*
   * Lecture de l'attribut MED_NOM_NBR
   */
  if (_MEDattrEntierLire(_pfid,MED_NOM_NBR,&_nentity) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_PROFILE_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_NBR);ISCRUTE(_nentity);
    goto ERROR;
  }


  /*
   * Lecture du profil
   */
  if ( MEDfilterEntityCr(fid, _nentity, 1, 1, MED_ALL_CONSTITUENT,
			 MED_NO_INTERLACE,MED_UNDEF_STMODE,
			 MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetRd(_pfid,MED_NOM_PFL,MED_INTERNAL_INT,&_filter, (unsigned char * const ) profilearray) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,MED_NOM_PFL);SSCRUTE(_path);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_PROFILE_MSG); SSCRUTE(_path);
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  if ( _pfid > 0 ) if ( _MEDdatagroupFermer(_pfid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_PROFILE_GRP);
    ISCRUTE_id(_pfid);
  }

  va_end(params);
  *fret = _ret;

  return;
}
