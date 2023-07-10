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

/**\ingroup MEDequivalence
  \brief \MEDequivalenceCrBrief
  \param fid \fid
  \param meshname \meshname
  \param equivname \equivname
  \param description \description
  \retval med_err  \error
  \details \MEDequivalenceCrDetails
 */

med_err
MEDequivalenceCr(const med_idt      fid,
		 const char * const meshname,
		 const char * const equivname,
		 const char * const description)
{
  med_access_mode _MED_ACCESS_MODE;
  med_idt        _root=0,_eqid=0,_meshid=0;
  med_err        _ret=-1;
  char           _path[MED_EQUIVALENCE_GRP_SIZE+MED_NAME_SIZE+1]=MED_EQUIVALENCE_GRP;

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
   * Si le DataGroup /EQS/ n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_path)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,_path)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_path);
     goto ERROR;
    }

  NOFINALBLANK(meshname,ERROR);

  /*
   * Si le DataGroup /EQS/<meshname> n'existe pas, on le cree
   */
  if ((_meshid = _MEDdatagroupOuvrir(_root,meshname)) < 0)
    if ((_meshid = _MEDdatagroupCreer(_root,meshname)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,meshname);
      SSCRUTE(_path);goto ERROR;
    }

  strcat(_path,meshname);
  NOFINALBLANK(equivname,ERROR);

  /*
   * Si le Data Group "/EQS/<meshname>/<equivname>" n'existe pas, on le cree
   */
  if ((_eqid = _MEDdatagroupOuvrir(_meshid,equivname)) < 0)
    if ((_eqid = _MEDdatagroupCreer(_meshid,equivname)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,equivname);
      SSCRUTE(_path);goto ERROR;
    }

 /*  if ( (_eqid = _MEDmeshAssociatedGroupCr(fid, */
/* 					  MED_EQUIVALENCE_NAME, */
/* 					  meshname, */
/* 					  numdt, */
/* 					  numit, */
/* 					  -1, */
/* 					  MED_FALSE, */
/* 					  equivalencename ) ) < 0)  { */
/*     MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshAssociatedGroupCr"); */
/*     SSCRUTE(MED_EQUIVALENCE_NAME);SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt); */
/*     SSCRUTE(equivalencename); */
/*     goto ERROR; */
/*   } */

  /*
   * L'attribut "DES"
   */
  if ((_ret = _MEDattributeStringWr(_eqid,MED_NOM_DES,MED_COMMENT_SIZE,
				   description)) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_EQUIVALENCE_MSG);
    SSCRUTE(equivname);SSCRUTE(description);

    goto ERROR;
  }


  _ret=0;
 ERROR:

  if (_eqid>0)            if (_MEDdatagroupFermer(_eqid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,equivname);
    ISCRUTE_id(_eqid);SSCRUTE(_path);
  }

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_eqid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_EQUIVALENCE_GRP);
    ISCRUTE_id(_eqid);
  }

  return _ret;
}



