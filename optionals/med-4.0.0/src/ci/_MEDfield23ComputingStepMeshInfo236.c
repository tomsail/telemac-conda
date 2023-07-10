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

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

void
_MEDfield23ComputingStepMeshInfo236(int dummy,...)
{


  med_err   _ret=-1,_err=0;
  med_size  _num,_nmesh=0;

  med_idt   _datagroup1=0,_meshgid=0,_linkgid=0;
  char      _datagroupname1[2*MED_MAX_PARA+1]         ="";
  char      _ent_geo       [2*MED_TAILLE_NOM_ENTITE+2]="";
  char      _path          [(MED_FIELD_GRP_SIZE+MED_TAILLE_NOM+1)+
			    (2*MED_TAILLE_NOM_ENTITE+1)+1+(2*MED_MAX_PARA)+1]=MED_FIELD_GRP;
  char     _meshpath [MED_MESH_GRP_SIZE+MED_TAILLE_NOM+1]=MED_MESH_GRP;
  char     _linkpath [MED_TAILLE_LIENS+MED_TAILLE_NOM+1]=MED_LIENS;
  med_size _ncpst=0;
  med_bool _checkmultiplemesh=MED_TRUE, _multiplemesh       =MED_FALSE;
  med_bool _checkmeshname    =MED_TRUE, _samedefaultmeshname=MED_FALSE;


  MED_VARGS_DECL(const, med_idt       , , fid       );
  MED_VARGS_DECL(const, char * , const  , fieldname );
  MED_VARGS_DECL(const, int           , , csit      );
  MED_VARGS_DECL(, med_int *, const     , numdt     );
  MED_VARGS_DECL(, med_int *, const     , numit     );
  MED_VARGS_DECL(, med_float *, const   , dt        );
  MED_VARGS_DECL(, med_int *, const     , nmesh     );
  MED_VARGS_DECL(, char *, const        , meshname  );
  MED_VARGS_DECL(, med_bool *, const    , localmesh );
  MED_VARGS_DECL(, med_int *, const     , meshnumdt );
  MED_VARGS_DECL(, med_int *, const     , meshnumit );
  MED_VARGS_DECL(, med_err *           ,, fret      );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt       , , fid       );
  MED_VARGS_DEF(const, char * , const  , fieldname );
  MED_VARGS_DEF(const, int           , , csit      );
  MED_VARGS_DEF(, med_int *, const     , numdt     );
  MED_VARGS_DEF(, med_int *, const     , numit     );
  MED_VARGS_DEF(, med_float *, const   , dt        );
  MED_VARGS_DEF(, med_int *, const     , nmesh     );
  MED_VARGS_DEF(, char *, const        , meshname  );
  MED_VARGS_DEF(, med_bool *, const    , localmesh );
  MED_VARGS_DEF(, med_int *, const     , meshnumdt );
  MED_VARGS_DEF(, med_int *, const     , meshnumit );
  MED_VARGS_DEF(, med_err *           ,, fret      );

  _num=csit-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On construit le nom du datagroup
   */
  strcat(_path,fieldname);

/*   if ( _MEDfieldComputingStepCheck236(fid,  fieldname, */
/* 				      &_ncpst, */
/* 				      _checkmultiplemesh, &_multiplemesh, */
/* 				      _checkmeshname,     &_samedefaultmeshname) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_FIELD,fieldname); */
/*     goto ERROR; */
/*   } */

/*
 * On suppose que le modèle 2.3.6 est respecté :
 * Il y a les mêmes séquences de calcul pour tous les couples
 * (typent,typegeo). Autrement dit selon l'utilisation de l'API 2.3.6 :
 * A chaque séquence de calcul, on utilise le même ensemble de (typent,typegeo).
 * En conséquence si l'on fixe le (typeent,typegeo) les séquences
 * de calcul que l'on va trouver seront représentatives de celles
 * présentent dans tous les (typent,typegeo) utilisés.
 */
  if ( _MEDobjectGetName(fid, _path ,0, _ent_geo) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);
    SSCRUTE(_path); SSCRUTE(_ent_geo); goto ERROR;
  }

  strcat(_path,"/");
  strcat(_path,_ent_geo);

  if ( _MEDobjectGetName(fid, _path ,_num, _datagroupname1) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);
    SSCRUTE(_path); SSCRUTE(_ent_geo); goto ERROR;
  }

  strcat(_path,"/");
  strcat(_path,_datagroupname1);

  if ((_datagroup1 = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }


  /*
   * Lecture des attributs
   */

  if (_MEDattrEntierLire(_datagroup1,MED_NOM_NDT,(med_int*) numdt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NDT);
    SSCRUTE(_path);ISCRUTE(*numdt);goto ERROR;
  }

  if (_MEDattrFloatLire(_datagroup1,MED_NOM_PDT,(med_float*) dt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_PDT);
    SSCRUTE(_path);RSCRUTE(*dt);goto ERROR;
  }

  if (_MEDattrEntierLire(_datagroup1,MED_NOM_NOR,(med_int*) numit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NOR);
    SSCRUTE(_path);ISCRUTE(*numit);goto ERROR;
  }

  if (_MEDattrStringLire(_datagroup1,MED_NOM_MAI,MED_TAILLE_NOM,meshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_MAI);
    SSCRUTE(_path);SSCRUTE(meshname);goto ERROR;
  }


  /* Nombre de maillages
   * La gestion d'une incohérence sur le nombre de séquences de calcul par couple (entitytype,geotype)
   * se fait à partir des appels :
   *  MEDfieldnProfile,MEDfieldnValue*
   * La vérification complémentaire que l'ensemble des maillages utilisés sont les mêmes pour toutes les séquences
   * de calcul identiques est effectué dans MEDfield23nValue
   */
  if ( (_err=_MEDnObjects(_datagroup1,".",&_nmesh)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }
  *nmesh = (med_int) _nmesh;


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

  *meshnumdt=MED_NO_DT;
  *meshnumit=MED_NO_IT;

  _ret = 0;

 ERROR:

  if (_datagroup1>0)            if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_datagroup1);
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
