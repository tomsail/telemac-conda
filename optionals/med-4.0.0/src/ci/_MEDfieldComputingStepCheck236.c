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

/*
 * PARAMETRES D'ENTREE:
 *
 * - checkmeshname vérifie quelque soit  (entitytype,geotype) et seqnum que le maillage
 * par défaut est toujours le même.
 * - checkmultiplemesh vérifie quelque soit (entitytype,geotype) et seqnum qu'un seul
 * maillage est utilisé
 * En vérifiant checkmeshname && checkmultiplemesh on s'assure qu'il y a 1 et 1 seul même maillage
 * pour tous les (entitytype,geotype) et seqnum.
 *
 * PARAMETRES DE SORTIE :
 *
 * - npcst : indique le nombre de séquences de calcul utilisés par le champ <fieldname>
 *
 * ERREUR :
 * Une erreur est généré systématiquement si le nombre de séquence de calcul n'est pas le même
 * pour tous les (entitytype,geotype)
 * TODO : Il faut tester multiplemesh et samedefaultmeshname pour savoir si la demande correspondant
 * respectivement à checkmultiplemesh et checkmeshname est satisfaite
 */

med_err _MEDfieldComputingStepCheck236(med_idt fid, const char * const fieldname,
				       med_size * const ncpst,
				       med_bool checkmultiplemesh, med_bool * const multiplemesh,
				       med_bool checkmeshname, med_bool * const samedefaultmeshname)
{
  med_err   _ret=-1,_err=0;
  med_idt   _cstpid=0;
  char      _ent_geo       [2*MED_TAILLE_NOM_ENTITE+2]="";
  char      _path          [(MED_FIELD_GRP_SIZE+MED_TAILLE_NOM+1)+
			    (2*MED_TAILLE_NOM_ENTITE+1)+1+(2*MED_MAX_PARA)+1]=MED_FIELD_GRP;
  int       _pathlen=0;
  med_size  _i=0,_n=0;
  med_size  _prevnseq=0,_nseq =0;
  med_string_itdatas _itdatas;

  if( _MEDfieldChecked(fid,fieldname) ) goto SORTIE;

  /*
   * On construit le nom du datagroup
   */
  strcat(_path,fieldname);

  /*
   * On compte le nombre de couple (entitytype,geotype)
   */
  if ((_err=_MEDnObjects(fid,_path,&_n)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,MED_NOM_ENT "." MED_NOM_GEO);
      goto ERROR;
    }

  _pathlen =  strlen(_path);

  /*
   * Pour tous les couples (entitytype,geotype)
   * on vérifie qu'il y a le même nombre de séquences de calcul (sinon le fichier n'est pas conforme au modèle 2.3.6)
   * Ceci est une vérification partielle que le modèle 2.3.6 a été respecté :
   *  - Il faudrait normalement vérifier qu'il s'agit aussi des mêmes séquences de calcul
   * Etant donnée la faible utilisation du usecase multimaillages et le coût de traitement associé à cette
   * vérification complémentaire, celle-ci n'est pas faite.
   * Par contre, il est necessaire de savoir si plusieurs noms de maillages
     apparaissent ou si un seul est utilisé (ici on vérifie uniquement que le maillage par défaut est le même)
   */

  /*L'attribut dont il faut vérifier la valeur pour toutes les étapes de calcul de tous les
   couple (entitytype,geotype) est MED_NOM_MAI*/
  if (checkmeshname) _MEDcheckAttributeStringInit(&_itdatas,MED_NOM_MAI,MED_TAILLE_NOM);

  for (_i=0; _i <_n; ++_i ) {

    /*
     * On recupere le nom du couple (entitytype,geotype) n°_i
     */
    if ( _MEDobjectGetName(fid, _path ,_i, _ent_geo) < 0 ) {
      MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);
      SSCRUTE(_path); goto ERROR;
    }

/*     SSCRUTE(_path); */
    strcpy(&_path[_pathlen],"/");
    strcat(_path,_ent_geo);
/*     SSCRUTE(_path); */

/*     ISCRUTE(_prevnseq); */
/*     ISCRUTE(_nseq); */

    /*
     * On vérifie qu'il y a le même nombre de séquences de calcul
     * que pour le couple (entitytype,geotype) précédent
     * on le vérifie pas que ce sont bien les mêmes
     */
    if ( _prevnseq != _nseq ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_ent_geo);
      ISCRUTE_size(_prevnseq);ISCRUTE_size(_nseq);ISCRUTE_size(_i);
      goto ERROR;
    }

    /*
     * On compte le nombre de séquences de calcul pour le couple (entitytype,geotype) n°_i
     */
    if ((_err=_MEDnObjects(fid,_path,&_nseq)) <0)
      if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
	MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_ent_geo);
	goto ERROR;
      }

    _prevnseq = _nseq;

    if (checkmeshname || checkmultiplemesh) {
      if ((_cstpid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
	MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
	goto ERROR;
      }
    }

    if (checkmeshname) {
      /*
       * On vérifie que le nom du maillage par défaut est le même pour toutes
       * les étapes de calcul de tous les couples (entitytype,geotype)
       */
      if (_MEDiterate(_cstpid, _MEDcheckAttributeStringFunc, &_itdatas ) < 0) {
	MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ATTRIBUTE,MED_ERR_VALUE_MSG);
	SSCRUTE(_itdatas.attvalprec); SSCRUTE(_itdatas.attval); goto ERROR;
      }
    }

    /*
     * On vérifie qu'un seul  maillage est utilisé pour toutes
     * les étapes de calcul de tous les couples (entitytype,geotype)
     */
    if (checkmultiplemesh) {
      if (_MEDiterate(_cstpid, _MEDchecknSublinkFunc, multiplemesh ) < 0) {
	MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,MED_ERR_NBR_MSG MED_ERR_MESH_MSG);
	SSCRUTE(_path);goto ERROR;
      }
    }

    if (_cstpid>0) if (_MEDdatagroupFermer(_cstpid) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
      ISCRUTE_id(_cstpid);
    }

    _path[_pathlen]='\0';
  }

  if (checkmeshname) _MEDcheckAttributeStringFin(&_itdatas);

  *ncpst = _nseq;

  _MEDfieldCheckedSetCache(fid, fieldname,MED_TRUE);

 SORTIE:

 _ret = 0;

 ERROR:

  return _ret;

}
