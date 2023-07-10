/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2013  EDF R&D, CEA/DEN
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
_MEDfieldCr30(int dummy, ...) {

  med_err _ret=-1;
  med_idt _root=0,_datagroup1=0;
  med_int _fieldtype;
  char    _datagroupname1[MED_NAME_SIZE+1]="";
  med_access_mode _MED_ACCESS_MODE;


  MED_VARGS_DECL(const, med_idt           , , fid           );
  MED_VARGS_DECL(const, char * , const      , fieldname     );
  MED_VARGS_DECL(const, med_field_type    , , fieldtype     );
  MED_VARGS_DECL(const, med_int           , , ncomponent    );
  MED_VARGS_DECL(const, char *, const       , componentname );
  MED_VARGS_DECL(const, char *, const       , componentunit );
  MED_VARGS_DECL(const, char *, const       , dtunit        );
  MED_VARGS_DECL(const, char *, const       , meshname      );
  MED_VARGS_DECL(, med_err *              , , fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid           );
  MED_VARGS_DEF(const, char * , const      , fieldname     );
  MED_VARGS_DEF(const, med_field_type,     , fieldtype     );
  MED_VARGS_DEF(const, med_int           , , ncomponent    );
  MED_VARGS_DEF(const, char *, const       , componentname );
  MED_VARGS_DEF(const, char *, const       , componentunit );
  MED_VARGS_DEF(const, char *, const       , dtunit        );
  MED_VARGS_DEF(const, char *, const       , meshname      );
  MED_VARGS_DEF(, med_err *              , , fret          );


 _MEDmodeErreurVerrouiller();

 _fieldtype = (med_int) fieldtype;

 if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  if ( _MED_ACCESS_MODE == MED_ACC_RDONLY) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    ISCRUTE_int(_MED_ACCESS_MODE);
    goto ERROR;
  }

  /*
   * Si le DataGroup MED_FIELD_GRP n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOuvrir(fid,MED_FIELD_GRP)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,MED_FIELD_GRP)) < 0) {
     MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_FIELD_GRP);
     goto ERROR;
    }

  NOFINALBLANK(fieldname,ERROR);
  /*
   * Si le Data Group "/CHA/<fieldname>" n'existe pas, on le cree
   */
  if ((_datagroup1 = _MEDdatagroupOuvrir(_root,fieldname)) < 0)
    if ((_datagroup1 = _MEDdatagroupCreer(_root,fieldname)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_FIELD_GRP);
      SSCRUTE(fieldname);goto ERROR;
    }


  /*
   * Les infos sur les composantes du champ
   */
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_NCO,&ncomponent) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NCO);
    ISCRUTE(ncomponent);goto ERROR;
  }
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_TYP,&_fieldtype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_TYP);
    ISCRUTE(_fieldtype);goto ERROR;
  }
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_NOM,
			    MED_SNAME_SIZE*ncomponent,componentname) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NOM);
    SSCRUTE(componentname);goto ERROR;
  }
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_UNI,
			    MED_SNAME_SIZE*ncomponent,componentunit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_UNI);
    SSCRUTE(componentunit);goto ERROR;
  }

  /*MODEL : MED_NOM_UNI vient du niveau en dessous
    Cree ou ouvre l'attribut  MED_NOM_UNI pour écriture
  */
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_UNT,MED_SNAME_SIZE,dtunit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_UNT);
    SSCRUTE(dtunit);goto ERROR;
  }

  /*MODEL : MED_NOM_MAI est écrit dans l'API de création de champ
   * Si c'est la première référence à un maillage, initialise l'attribut MED_MAI à ce maillage
   */
  NOFINALBLANK(meshname,ERROR);

  if (_MEDattributeStringWr(_datagroup1,MED_NOM_MAI,MED_NAME_SIZE,meshname) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_MAI);
    SSCRUTE(meshname);
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,meshname);
    goto ERROR;
  }


  /*
   * On ferme tout
   */

  _ret=0;
 ERROR:

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname1);
    ISCRUTE_id(_datagroup1);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_FIELD_GRP);
    ISCRUTE_id(_root);
  }

  va_end(params);
  *fret = _ret;
  return;
}
