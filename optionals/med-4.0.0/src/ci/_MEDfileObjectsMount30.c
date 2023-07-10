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

#ifdef PPRO_NT_CALL
#define F_OK 0
#else
#include <unistd.h>
#endif

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

void _MEDfileObjectsMount30(int dummy, ...){


  med_idt _ret = -1;
  med_idt _id=0, _rootId=0,_linkId=0,_gchfid=0;
/*   char             _mountPath[MED_TAILLE_MNT+2*MED_NAME_SIZE+2]=MED_MNT; */
  char             _mountPath[MED_TAILLE_MNT+2*MED_NAME_SIZE+2+MED_MAX_CHFID_PATH+1]=MED_MNT;
  char             _link[MED_NAME_SIZE+1]="";
  med_access_mode _accessMode;
  med_int  majeur=0, mineur=0, release=0;
  med_int  rfileversionMMR,rfileversionMM,rfileversionM;
  med_int  lfileversionMMR,lfileversionMM,lfileversionM;
  med_bool _datagroupexist=MED_FALSE,_isasoftlink=MED_FALSE;

  MED_VARGS_DECL(const, med_idt         , , fid           );
  MED_VARGS_DECL(const, med_idt         , , chfid         );
  MED_VARGS_DECL(const, char*     , const , mountfilename );
  MED_VARGS_DECL(const, med_class       , , medclass      );
  MED_VARGS_DECL(, med_idt *             ,, fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt         , , fid           );
  MED_VARGS_DEF(const, med_idt         , , chfid         );
  MED_VARGS_DEF(const, char*     , const , mountfilename );
  MED_VARGS_DEF(const, med_class       , , medclass      );
  MED_VARGS_DEF(, med_idt *             ,, fret          );

  _MEDmodeErreurVerrouiller();

  if ( MEDfileNumVersionRd(fid, &majeur, &mineur, &release) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDfileNumVersionRd");
    ISCRUTE(majeur);ISCRUTE(mineur);ISCRUTE(release);
    goto ERROR;
  }
  lfileversionM   = 100*majeur;
  lfileversionMM  = 100*majeur+10*mineur;
  lfileversionMMR = lfileversionMM+release;


  if ( (_accessMode = (med_access_mode)_MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_FILE,"");
    ISCRUTE_int(_accessMode);
    goto ERROR;
  }

  /*Le montage se fait soit en utilisant le chfid soit le mountfilename */
  /*si un chfid est donné, il est utilisé et le mountfilename est le chemin est le
    chemin à partir duquel se trouve la structure au format MED à monter dans le fichier hôte (parent). 
  */
  if (chfid) {
    _id=chfid;
  } else {
    /* does the file exist ? */
    if (access(mountfilename,F_OK)) {
      MED_ERR_(_ret,MED_ERR_EXIST,MED_ERR_FILE,mountfilename);
      goto ERROR;
    }
    /* Open the file "mountfilename". */
    if ((_id = _MEDfileOpen(mountfilename,_accessMode)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_FILE,mountfilename);
      ISCRUTE_int(_accessMode);
      goto ERROR;
    }
  }


  if (chfid) {

    if( (_gchfid = _MEDdatagroupOpen(chfid,mountfilename)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_DOESNTEXIST,MED_ERR_DATAGROUP,mountfilename);
      goto ERROR;
    }

    if ( MEDfileNumVersionRd(_gchfid, &majeur, &mineur, &release) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDfileNumVersionRd");
      SSCRUTE(mountfilename);ISCRUTE(majeur);ISCRUTE(mineur); ISCRUTE(release);
      goto ERROR;
    }

    if (_MEDdatagroupFermer(_gchfid) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILE,MED_MNT);
      goto ERROR;
    }
    _gchfid = 0;

  } else {

    if ( MEDfileNumVersionRd(_id, &majeur, &mineur, &release) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDfileNumVersionRd");
      SSCRUTE(mountfilename);ISCRUTE(majeur);ISCRUTE(mineur); ISCRUTE(release);
      goto ERROR;
    }
  }

  rfileversionM   = 100*majeur;
  rfileversionMM  = 100*majeur+10*mineur;
  rfileversionMMR = rfileversionMM+release;

  if ( !( (rfileversionMM == lfileversionMM) && (rfileversionMMR <= lfileversionMM+9 ) ) ) {
	 MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_FILE,mountfilename);
	 ISCRUTE(rfileversionMMR);ISCRUTE(lfileversionMMR);
	 goto ERROR;
       }

  /*
   * Give access to the class object in the local file
   */
  switch(medclass) {

  case MED_MESH :
    strcpy(_link,MED_MESH_GRP);
    break;

  case MED_MESH_SUPPORT :
    strcpy(_link,MED_MESH_SUPPORT_GRP);
    break;

  case MED_ELSTRUCT :
    strcpy(_link,MED_ELSTRUCT_GRP);
    break;

  case MED_FAMILY :
    strcpy(_link,MED_FAMILY_GRP);
    break;

  case MED_EQUIVALENCE :
    strcpy(_link,MED_EQUIVALENCE_GRP);
    break;

  case MED_JOINT :
    strcpy(_link,MED_JOINT_GRP);
    break;

  case MED_FIELD :
    /* Ajouter aussi MED_CHA_INTERP*/
    //strcpy(_link,MED_CHA_INTERP);
    strcpy(_link,MED_FIELD_GRP);
    break;

  case MED_LOCALIZATION :
    strcpy(_link,MED_LOCALIZATION_GRP);
    break;

  case MED_PROFILE :
    strcpy(_link,MED_PROFILE_GRP);
    break;

  case MED_INTERPOLATION :
    strcpy(_link,MED_INTERPOLATION_GRP);
    break;

  case MED_NUMERICAL_DATA :
    strcpy(_link,MED_NUMERICAL_DATA_GRP);
    break;

  default :
    goto ERROR;
  }

  /*
   * Mount point creation in the local file
   * TODO : Ds un fichier mémoire
   */
  if ((_rootId = _MEDdatagroupOuvrir(fid,MED_MNT)) < 0 )
    if ((_rootId = _MEDdatagroupCreer(fid,MED_MNT)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_MNT);
      goto ERROR;
    }

  /*
   * Un datagroup spécifique à la <medclass> montée est crée
     Celà permet de monter simultanément plusieurs type de <medclass>
  */
  if ((_linkId = _MEDdatagroupOuvrir(_rootId,&_link[1])) <0)
    if ((_linkId = _MEDdatagroupCreer(_rootId,&_link[1])) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_MNT);
      SSCRUTE(&_link[1]);goto ERROR;
    }

  /* Etablit le chemin de montage en rapport à la <medclass> montée */
  strcpy(&_mountPath[strlen(_mountPath)-1],_link);
  /*   SSCRUTE(_mountPath); */
  /* Monte la racine du fichier d'identifiant <_id> au <_mountpath>
     (seule la racine d'un fichier peut être montée, il n'est pas possible
     de monter directement un datagroup )
   */
  if ( _MEDfichierMonter(fid,_mountPath,_id) < 0 ) {
    MED_ERR_(_ret,MED_ERR_MOUNT,MED_ERR_FILE,_mountPath);
    H5Eprint1(stderr);
    goto ERROR;
  }


  /*
   * Met à jour le mountpath pour servir de cible à un lien symbolique du datagroup /<link>
   */

  if ( chfid && strlen(mountfilename) ) {
    /* <mountfilename> doit être un chemin absolu (commencer par /) */
    /* <mountfilename> n'est pas obligé de finir par /              */
    strncpy(&_mountPath[strlen(_mountPath)-1],mountfilename,MED_MAX_CHFID_PATH);
    strcpy(&_mountPath[strlen(_mountPath)],_link);
  } else {
    strcpy(&_mountPath[strlen(_mountPath)-1],_link);
  }
  /*   SSCRUTE(_mountPath); */

  /* On s'assure que le datagroup <link> n'existe pas à la racine du fichier d'acceuil
     REM1 : Par cette technique, il n'est pas possible de monter un <medclass> distant s'il en
     existe un local dans le fichier d'acceuil
     REM2 : Puisque l'on ne peut monter que la racine d'un fichier dans un autre,
     on ne peut monter le fichier cible à la racine qui masquerait les autres <medclass>
     REM3 : Pour monter un fichier, il faut que le fichier d'acceuil soit ouvert en lecture/écriture
  */
  if( _MEDdatagroupExist(fid,_link,&_datagroupexist,&_isasoftlink) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDdatagroupExist");
    SSCRUTE(MED_NOM_NOE);goto ERROR;
  }
  if ( _datagroupexist && !_isasoftlink ) {
    MED_ERR_(_ret,MED_ERR_EXIST,MED_ERR_DATAGROUP,_link);
    SSCRUTE(_mountPath);
    goto ERROR;
  }

  /*Si le lien au datatagroup monté n'existe pas, on le crée */
  if ( (!_datagroupexist) )
    if (_MEDdatagroupLienCreer(fid,_mountPath,_link) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_LINK,_link);
      SSCRUTE(_mountPath);
      goto ERROR;
    }

  _ret = _id;
 ERROR:

  if (_gchfid > 0)
    if (_MEDdatagroupFermer(_gchfid) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILE,MED_MNT);
    }

  if (_linkId > 0)
    if (_MEDdatagroupFermer(_linkId) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILE,MED_MNT);
    }

  if (_rootId > 0)
    if (_MEDdatagroupFermer(_rootId) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILE,MED_MNT);
    }

  va_end(params);
  *fret = _ret;
  return;
}
