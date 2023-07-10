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

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

void
_MEDfieldInfoByName236(int dummy, ...) {


  med_err  _ret=-1,_err=-1;
  med_idt  _fieldgid=0,_meshgid=0,_linkgid=0;
  char     _fieldpath[MED_FIELD_GRP_SIZE+MED_TAILLE_NOM+1]=MED_FIELD_GRP;
  char     _meshpath [MED_MESH_GRP_SIZE+MED_TAILLE_NOM+1]=MED_MESH_GRP;
  char     _linkpath [MED_TAILLE_LIENS+MED_TAILLE_NOM+1]=MED_LIENS;
  med_size _ncstp=0;
  med_int  _n=0;
  med_int  _intfieldtype=0;
  char     _meshrefpath236[(2*MED_TAILLE_NOM_ENTITE+1)+1+ (2*MED_MAX_PARA)+1]="";


  MED_VARGS_DECL(const, med_idt          , , fid           );
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

  MED_VARGS_DEF(const, med_idt          , , fid           );
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

  if ((_fieldgid = _MEDdatagroupOuvrir(fid,_fieldpath)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_fieldpath);
    goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_NCO */
  if ( _MEDattrEntierLire(_fieldgid,MED_NOM_NCO,&_n) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NCO);
    SSCRUTE(_fieldpath);goto ERROR;
  }

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
  if (_MEDattrStringLire(_fieldgid,MED_NOM_NOM,_n*MED_TAILLE_PNOM,
			 componentname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_fieldpath);SSCRUTE(MED_NOM_NOM);
    SSCRUTE(componentname);goto ERROR;
  }

  if (_MEDattrStringLire(_fieldgid,MED_NOM_UNI,_n*MED_TAILLE_PNOM,
			 componentunit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_fieldpath);SSCRUTE(MED_NOM_UNI);
    SSCRUTE(componentunit);goto ERROR;
  }


  /*
   * On recupere le nom du premier couple (entitytype,geotype)
   */
  if ( _MEDobjectGetName(_fieldgid, "." , 0, _meshrefpath236) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,".");
    SSCRUTE(_meshrefpath236); goto ERROR;
  }

  /* Nombre d'étapes de calcul
   * La gestion d'une incohérence sur le nombre de séquences de calcul par couple (entitytype,geotype)
   * se fait à partir des appels :
   *  MEDfieldnProfile,MEDfieldnValue,MEDfieldnValueWithProfile
   */
  if ( (_err=_MEDnObjects(_fieldgid,_meshrefpath236,&_ncstp)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_fieldpath);
      goto ERROR;
    }
  *ncstp = (med_int) _ncstp;

  /*
   * On recupere le nom de maillage par défaut du premier couple (entitytype,geotype)
   * La gestion des erreurs multimaillage 2.3.6 se fait à partir des appels :
   * MEDfieldnProfile,MEDfieldnValue,MEDfieldnValueWithProfile
   */
  strcat(_meshrefpath236,"/");
  if ( _MEDobjectGetName(_fieldgid, _meshrefpath236 , 0, &_meshrefpath236[strlen(_meshrefpath236)]) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_meshrefpath236);
    goto ERROR;
  }


  /* Lecture de l'attribut MED_NOM_UNI */
  if ( _MEDattributeStringRdByName(_fieldgid,_meshrefpath236,MED_NOM_UNI,MED_TAILLE_PNOM,dtunit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_meshrefpath236);SSCRUTE(MED_NOM_UNI);
    SSCRUTE(dtunit);goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_MAI */
  if ( _MEDattributeStringRdByName(_fieldgid,_meshrefpath236,MED_NOM_MAI,MED_NAME_SIZE,meshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_meshrefpath236);SSCRUTE(MED_NOM_MAI);SSCRUTE(meshname);
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
