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

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

void _MEDsubdomainJointInfo236(int dummy, ...) {


  med_idt  _jntid=0;
  med_err  _ret=-1,_err=-1;
  char     _path[MED_MESH_GRP_SIZE+MED_JOINT_GRP_SIZE+MED_TAILLE_NOM+1]=MED_MESH_GRP;
  int      _num;
  med_size _nstep=0,_nocstpncorrespondence=0;


  MED_VARGS_DECL(const, med_idt      , , fid                   );
  MED_VARGS_DECL(const, char * , const , localmeshname         );
  MED_VARGS_DECL(const, int          , , jointit               );
  MED_VARGS_DECL(, char *, const       , jointname             );
  MED_VARGS_DECL(, char *, const       , description           );
  MED_VARGS_DECL(, med_int *, const    , domainnumber          );
  MED_VARGS_DECL(, char *, const       , remotemeshname        );
  MED_VARGS_DECL(, med_int *, const    , nstep                 );
  MED_VARGS_DECL(, med_int *, const    , nocstpncorrespondence );
  MED_VARGS_DECL(, med_err *                  ,, fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid                   );
  MED_VARGS_DEF(const, char * , const , localmeshname         );
  MED_VARGS_DEF(const, int          , , jointit               );
  MED_VARGS_DEF(, char *, const       , jointname             );
  MED_VARGS_DEF(, char *, const       , description           );
  MED_VARGS_DEF(, med_int *, const    , domainnumber          );
  MED_VARGS_DEF(, char *, const       , remotemeshname        );
  MED_VARGS_DEF(, med_int *, const    , nstep                 );
  MED_VARGS_DEF(, med_int *, const    , nocstpncorrespondence );
  MED_VARGS_DEF(, med_err *                  ,, fret          );

  _num=jointit-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();


  /*
   * On recupere le nom du joint
   */
  strcat(_path,localmeshname);
  strcat(_path,MED_JOINT_GRP);
  if ( _MEDobjectGetName(fid, _path ,_num, jointname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(jointit);
    SSCRUTE(_path);
    goto ERROR;
  }


  /*
   * Si le Data Group JNT n'existe pas => erreur
   */
  strcat(_path,jointname);
  if ((_jntid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);
    goto ERROR;
  }

  /*
   * L'attribut "DES"
   */
  if ( _MEDattrStringLire(_jntid,MED_NOM_DES,MED_TAILLE_DESC,description) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
      SSCRUTE(jointname);SSCRUTE(_path);SSCRUTE(MED_NOM_DES);
      goto ERROR;
  }
  /*
   * L'attribut "MAI"
   */
  if ( _MEDattrStringLire(_jntid,MED_NOM_MAI,MED_TAILLE_NOM,remotemeshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(jointname);SSCRUTE(_path);SSCRUTE(MED_NOM_MAI);SSCRUTE(remotemeshname);
    goto ERROR;
  }

  /*
   * L'attribut "DOM"
   */
  if (_MEDattrEntierLire(_jntid,MED_NOM_DOM,domainnumber) < 0){
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(jointname);SSCRUTE(_path);SSCRUTE(MED_NOM_DOM);ISCRUTE(*domainnumber);
    goto ERROR;
  }

  /*
   * Nombre du nombre de correspondances dans le joint
   */

  if ((_err=_MEDnObjects(_jntid, _path, &_nocstpncorrespondence)) < 0 )
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }

  *nocstpncorrespondence = (med_int) _nocstpncorrespondence;


  *nstep=1;

 _ret = 0;

 ERROR:

  if (_jntid>0)            if (_MEDdatagroupFermer(_jntid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_jntid);
  }

  va_end(params);
  *fret = _ret;
  return;
}


