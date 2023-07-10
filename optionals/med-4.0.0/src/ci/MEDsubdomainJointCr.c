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


/**\ingroup MEDsubdomainJoint
   \brief \MEDsubdomainJointCrBrief
   \param fid \fid
   \param localmeshname \localmeshname
   \param jointname \jointname
   \param description \description
   \param domainnumber \domainnumber
   \param remotemeshname \remotemeshname
   \return \error
   \details
   \MEDsubdomainJointCrDetails
   \par Remarques
   \MEDjointDef
*/

med_err
MEDsubdomainJointCr(const med_idt      fid,
		    const char * const localmeshname,
		    const char * const jointname,
		    const char * const description,
		    const med_int      domainnumber,
		    const char * const remotemeshname)
{
  med_access_mode _MED_ACCESS_MODE;
  med_idt        _root=0,_jntid=0,_meshid=0;
  med_err        _ret=-1;
  char           _path[MED_JOINT_GRP_SIZE+MED_NAME_SIZE+1]=MED_JOINT_GRP;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
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
   * Si le DataGroup /JNT/ n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_path)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,_path)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_path);
     goto ERROR;
    }

  NOFINALBLANK(localmeshname,ERROR);

  /*
   * Si le DataGroup /JNT/<localmeshname> n'existe pas, on le cree
   */
  if ((_meshid = _MEDdatagroupOuvrir(_root,localmeshname)) < 0)
    if ((_meshid = _MEDdatagroupCreer(_root,localmeshname)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,localmeshname);
      SSCRUTE(_path);goto ERROR;
    }

  strcat(_path,localmeshname);
  NOFINALBLANK(jointname,ERROR);

  /*
   * Si le Data Group "/JNT/<localmeshname>/<jointname>" n'existe pas, on le cree
   */
  if ((_jntid = _MEDdatagroupOuvrir(_meshid,jointname)) < 0)
    if ((_jntid = _MEDdatagroupCreer(_meshid,jointname)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,jointname);
      SSCRUTE(_path);goto ERROR;
    }

 /*  if ( (_jntid = _MEDmeshAssociatedGroupCr(fid, */
/* 					  MED_JOINT_NAME, */
/* 					  localmeshname, */
/* 					  numdt, */
/* 					  numit, */
/* 					  -1, */
/* 					  MED_FALSE, */
/* 					  subdomainJointname ) ) < 0)  { */
/*     MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAssociatedGroupCr"); */
/*     SSCRUTE(MED_JOINT_NAME);SSCRUTE(localmeshname);ISCRUTE(numit);ISCRUTE(numdt); */
/*     SSCRUTE(subdomainJointname); */
/*     goto ERROR; */
/*   } */

  /*
   * L'attribut "DES"
   */
  if ((_ret = _MEDattributeStringWr(_jntid,MED_NOM_DES,MED_COMMENT_SIZE,
				   description)) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(jointname);SSCRUTE(MED_NOM_DES);SSCRUTE(description);
    goto ERROR;
  }

  /*
   * L'attribut "MAI"
   */
  if ((_ret = _MEDattributeStringWr(_jntid,MED_NOM_MAI,MED_NAME_SIZE,
				   remotemeshname)) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(jointname);SSCRUTE(MED_NOM_MAI);SSCRUTE(remotemeshname);
    goto ERROR;
  }

  /*
   * L'attribut "DOM"
   */
  if ((_ret = _MEDattributeIntWr(_jntid, MED_NOM_DOM, &domainnumber)) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(jointname);SSCRUTE(MED_NOM_DOM);ISCRUTE(domainnumber);
    goto ERROR;
  }


  _ret=0;
 ERROR:

  if (_jntid>0)            if (_MEDdatagroupFermer(_jntid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,jointname);
    ISCRUTE_id(_jntid);SSCRUTE(_path);
  }

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_jntid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_JOINT_GRP);
    ISCRUTE_id(_jntid);
  }

  return _ret;
}



