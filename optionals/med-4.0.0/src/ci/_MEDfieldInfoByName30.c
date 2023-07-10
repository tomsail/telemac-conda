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


#include "med_config.h"
#include <med.h>
#include "med_outils.h"
#include <string.h>

void
_MEDfieldInfoByName30(int dummy, ...) {


  med_err  _ret=-1,_err=-1;
  med_idt  _fieldgid=0,_meshgid=0,_linkgid=0;
  char     _fieldpath[MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1]=MED_FIELD_GRP;
  char     _meshpath [MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  char     _linkpath [MED_TAILLE_LIENS+MED_NAME_SIZE+1]=MED_LIENS;
  med_size _ncstp=0;
  med_int  _n=0;
  med_int  _intfieldtype=0;


  MED_VARGS_DECL(const, med_idt           , , fid           );
  MED_VARGS_DECL(const, char * , const     , fieldname     );
  MED_VARGS_DECL(, char *, const           , meshname      );
  MED_VARGS_DECL(, med_bool *, const       , localmesh     );
  MED_VARGS_DECL(, med_field_type *, const , fieldtype     );
  MED_VARGS_DECL(, char *, const           , componentname );
  MED_VARGS_DECL(, char *, const           , componentunit );
  MED_VARGS_DECL(, char *, const           , dtunit        );
  MED_VARGS_DECL(, med_int *, const        , ncstp      );
  MED_VARGS_DECL(, med_err *              ,, fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid           );
  MED_VARGS_DEF(const, char * , const     , fieldname     );
  MED_VARGS_DEF(, char *, const           , meshname      );
  MED_VARGS_DEF(, med_bool *, const       , localmesh     );
  MED_VARGS_DEF(, med_field_type *, const , fieldtype     );
  MED_VARGS_DEF(, char *, const           , componentname );
  MED_VARGS_DEF(, char *, const           , componentunit );
  MED_VARGS_DEF(, char *, const           , dtunit        );
  MED_VARGS_DEF(, med_int *, const        , ncstp      );
  MED_VARGS_DEF(, med_err *              ,, fret          );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  strcat(_fieldpath,fieldname);

  /*
   * On recupere le nombre de composants
   */
  if ((_fieldgid = _MEDdatagroupOuvrir(fid,_fieldpath)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_fieldpath);
    goto ERROR;
  }

  if ( _MEDattrEntierLire(_fieldgid,MED_NOM_NCO,&_n) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NCO);
    SSCRUTE(_fieldpath);goto ERROR;
  }

  /*
   * La liste des attributs
   */

  /* Lecture de l'attribut MED_NOM_TYP */
  if ( _MEDattrEntierLire(_fieldgid,MED_NOM_TYP,&_intfieldtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(MED_NOM_TYP);
    goto ERROR;
  }
  *fieldtype = (med_field_type) (_intfieldtype);


  /*
   * Les infos sur les composantes du champ
   */
  if (_MEDattrStringLire(_fieldgid,MED_NOM_NOM,_n*MED_SNAME_SIZE,
			 componentname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_fieldpath);SSCRUTE(MED_NOM_NOM);
    SSCRUTE(componentname);goto ERROR;
  }

  if (_MEDattrStringLire(_fieldgid,MED_NOM_UNI,_n*MED_SNAME_SIZE,
			 componentunit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_fieldpath);SSCRUTE(MED_NOM_UNI);
    SSCRUTE(componentunit);goto ERROR;
  }
  /*MODEL : MED_NOM_UNI vient du niveau en dessous
    Cree ou ouvre l'attribut  MED_NOM_UNI pour écriture
  */
  if ( _MEDattrStringLire(_fieldgid,MED_NOM_UNT,MED_SNAME_SIZE,dtunit) < 0) {
   MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_fieldpath);SSCRUTE(MED_NOM_UNT);
    SSCRUTE(dtunit);goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_MAI */
  if ( _MEDattrStringLire(_fieldgid,MED_NOM_MAI,MED_NAME_SIZE,meshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(MED_NOM_MAI);SSCRUTE(meshname);
    goto ERROR;
  }

 /* Maillage local ou distant */
  strcat(_meshpath,meshname);

  /* Le maillage est il distant */
  if ( (_meshgid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0)  {

    /* Verifie que le maillage est bien référencé comme distant */
    strcat(_linkpath,meshname);
    if ((_linkgid = _MEDdatagroupOuvrir(fid,_linkpath)) < 0) {
/*       MED_ERR_(_ret,MED_ERR_DOESNTEXIST,MED_ERR_MESH,MED_ERR_FIELD_MSG); */
/*       SSCRUTE(fieldname);SSCRUTE(_meshpath);SSCRUTE(_linkpath); */
/*       goto ERROR; */
      *localmesh = MED_FALSE;
    }
    *localmesh = MED_FALSE;
  } else
    *localmesh = MED_TRUE;

  /* Nombre d'étapes de calcul*/
  if ( (_err=_MEDnObjects(_fieldgid,".",&_ncstp)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_fieldpath);
      goto ERROR;
    }

  *ncstp = (med_int) _ncstp;

  _ret = 0;

 ERROR:


  if (_fieldgid>0)            if (_MEDdatagroupFermer(_fieldgid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_fieldpath);
    ISCRUTE_id(_fieldgid);
  }

  if (_meshgid>0)            if (_MEDdatagroupFermer(_meshgid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshgid);
  }

  if (_linkgid>0)            if (_MEDdatagroupFermer(_linkgid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_linkpath);
    ISCRUTE_id(_linkgid);
  }

  va_end(params);
  *fret = _ret;
  return;
}
