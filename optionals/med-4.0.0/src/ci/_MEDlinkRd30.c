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

void
_MEDlinkRd30(int dummy, ...)
{


  med_idt        _lid=0, _root=0;
  med_err        _ret=-1;
  char           _path[MED_TAILLE_LIENS+MED_NAME_SIZE+1]=MED_LIENS;
  med_int        _n=0;
  med_filter     _filter        = MED_FILTER_INIT;


  MED_VARGS_DECL(const, med_idt      , , fid        );
  MED_VARGS_DECL(const, char* , const  , meshname   );
  MED_VARGS_DECL(, char*, const  , link             );
  MED_VARGS_DECL(, med_err *          ,, fret       );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid        );
  MED_VARGS_DEF(const, char* , const  , meshname   );
  MED_VARGS_DEF(, char*, const  , link             );
  MED_VARGS_DEF(, med_err *          ,, fret       );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();


  /*
   * Ouverture du DataGroup /LIENS/
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_path)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }

  /*
   * Ouverture du /LIENS/<meshname>
   */
  if ((_lid = _MEDdatagroupOuvrir(_root,meshname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,meshname);
    SSCRUTE(_path);goto ERROR;
  }

  strcat(_path,meshname);


  /*
   * On lit <n> sous forme d'attribut
   */
  if (_MEDattrEntierLire(_lid,MED_NOM_NBR,&_n) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LINK_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_NBR);ISCRUTE(_n);
    goto ERROR;
  }

  /*
   * On lit le link dans un dataset
   */
  if ( MEDfilterEntityCr(fid,_n, 1, 1, MED_ALL_CONSTITUENT,
			 MED_NO_INTERLACE,MED_UNDEF_STMODE,
			 MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetRd(_lid,MED_NOM_LIE,MED_INTERNAL_CHAR,&_filter,(unsigned char *) link) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,MED_NOM_LIE);SSCRUTE(_path);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_LINK_MSG); SSCRUTE(_path);
    goto ERROR;
  }


  _ret = 0;

 ERROR:

  if (_lid>0)            if (_MEDdatagroupFermer(_lid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,meshname);
    ISCRUTE_id(_lid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_LIENS);
    ISCRUTE_id(_root);
  }


  va_end(params);
  *fret = _ret;
  return;
}
