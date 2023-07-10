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

void _MEDsubdomainJointInfo30(int dummy, ...) {


  med_idt  _jntid=0;
  med_err  _ret=-1,_err=-1;
  char     _path[MED_JOINT_GRP_SIZE+2*MED_NAME_SIZE+2]=MED_JOINT_GRP;
  char     _cstpname[2*MED_MAX_PARA+1]="";
  int      _num;
  int      _pathreflen=0;
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
   * On recupere le nom de l'subdomainJoint
   */
  strcat(_path,localmeshname);
  _pathreflen=strlen(_path);
  if ( _MEDobjectGetName(fid, _path ,_num, jointname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(jointit);
    SSCRUTE(_path);
    goto ERROR;
  }

  _path[_pathreflen]='/';
  strncpy(&_path[_pathreflen+1],jointname,MED_NAME_SIZE+1);
  if ((_jntid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);
    goto ERROR;
  }


  /*
   * L'attribut DES
   */
  if ( _MEDattrStringLire(_jntid,MED_NOM_DES,MED_COMMENT_SIZE,description) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
      SSCRUTE(jointname);SSCRUTE(_path);SSCRUTE(MED_NOM_DES);
      goto ERROR;
  }

  /*
   * L'attribut "MAI"
   */
  if ((_ret = _MEDattrStringLire(_jntid,MED_NOM_MAI,MED_NAME_SIZE,
				 remotemeshname)) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(jointname);SSCRUTE(_path);SSCRUTE(MED_NOM_MAI);SSCRUTE(remotemeshname);
    goto ERROR;
  }

  /*
   * L'attribut "DOM"
   */
  if ((_ret = _MEDattrEntierLire(_jntid, MED_NOM_DOM, domainnumber)) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(jointname);SSCRUTE(_path);SSCRUTE(MED_NOM_DOM);ISCRUTE(*domainnumber);
    goto ERROR;
  }

  /*
   * Nombre d'étapes de calcul
   */
  if ((_err=_MEDnObjects(_jntid,".",&_nstep)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }
  *nstep = (med_int) _nstep;


  /*
   * Nombre de correspondence pour <MED_NO_DT,MED_NO_IT>
   */

  _MEDgetComputationStepName(MED_SORT_DTIT,MED_NO_DT,MED_NO_IT,_cstpname);

  if ((_err=_MEDnObjects(_jntid, _cstpname, &_nocstpncorrespondence)) < 0 )
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_cstpname);
      goto ERROR;
    }

  *nocstpncorrespondence = (med_int) _nocstpncorrespondence;


 _ret = 0;

 ERROR:


 if (_jntid>0)         if (_MEDdatagroupFermer(_jntid) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,&_path[_pathreflen]);
   ISCRUTE_id(_jntid);
 }

  va_end(params);
  *fret = _ret;
  return;
}


