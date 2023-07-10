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
   \brief \MEDsubdomainCorrespondenceWrBrief
   \param fid \fid
   \param localmeshname \localmeshname
   \param jointname \jointname
   \param numdt \numdt
   \param numit \numit
   \param localentitype \localentitype
   \param localgeotype \localgeotype
   \param remoteentitype \remoteentitype
   \param remotegeotype \remotegeotype
   \param nentity \nentity
   \param correspondence \correspondence
   \return \error
   \details
   \MEDsubdomainCorrespondenceWrDetails
   \par Remarques
   \MEDjointDef
*/

med_err
MEDsubdomainCorrespondenceWr(const med_idt            fid,
			     const char * const       localmeshname,
			     const char * const       jointname,
			     const med_int            numdt,
			     const med_int            numit,
			     const med_entity_type    localentitype,
			     const med_geometry_type  localgeotype,
			     const med_entity_type    remoteentitype,
			     const med_geometry_type  remotegeotype,
			     const med_int            nentity,
			     const med_int * const    correspondence) {
  
  med_access_mode     _MED_ACCESS_MODE;
  med_idt            _root=0,_jntid=0,_meshid=0,_cstpid=0,_datagroup2=0;
  med_idt            _dataset=0;
  med_err            _ret=-1;
  char               _path[MED_JOINT_GRP_SIZE+2*MED_NAME_SIZE+2]=MED_JOINT_GRP;
  char               _computationstepname[2*MED_MAX_PARA+1]="";
  char               _datagroupname2[4*MED_TAILLE_NOM_ENTITE+4]="";
  char               _localgeotypename   [MED_TAILLE_NOM_ENTITE+1]="";
  char               _remotegeotypename  [MED_TAILLE_NOM_ENTITE+1]="";
  med_sorting_type   _sortingtype=0;
  med_filter         _filter        = MED_FILTER_INIT;
  med_int            _localgeotype   = MED_NONE;
  med_int            _localentitype  = localentitype;
  med_int            _remotegeotype  = MED_NONE;
  med_int            _remoteentitype = remoteentitype;

  if ( localentitype == MED_NODE_ELEMENT ) _localgeotype=MED_NODE ;

 
  if ( localentitype  != MED_NODE ) _localgeotype  = localgeotype ;
  if ( remoteentitype != MED_NODE ) _remotegeotype = remotegeotype ;

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
   * Ouverture du dataGroup /JNT/
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * Ouverture du dataGroup <meshname>
   */
  if ((_meshid = _MEDdatagroupOuvrir(_root,localmeshname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,localmeshname);
    SSCRUTE(_path);goto ERROR;
  }

  strcat(_path,localmeshname);

  /*
   * Ouverture du data Group "/JNT/<localmeshname>/<jointname>"
   */
  if ((_jntid = _MEDdatagroupOuvrir(_meshid,jointname)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,jointname);
      SSCRUTE(_path);goto ERROR;
  }

  strcat(_path,"/");
  strcat(_path,jointname);

  if ( MEDmeshSortingTypeRd(fid,localmeshname,&_sortingtype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API," MEDmeshSortingTypeRd");
    SSCRUTE(localmeshname);ISCRUTE_int(_sortingtype);goto ERROR;
  }

  _MEDgetComputationStepName(_sortingtype,numdt,numit,_computationstepname);

  if ((_cstpid = _MEDdatagroupOuvrir(_jntid,_computationstepname)) < 0)
    if ((_cstpid = _MEDdatagroupCreer(_jntid,_computationstepname)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_computationstepname);
      SSCRUTE(_path);goto ERROR;
    }

  /*Cree ou ouvre l'attribut MED_NOM_NDT pour �criture */
  if ( _MEDattributeIntWr(_cstpid,MED_NOM_NDT,&numdt) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);
    goto ERROR;
  }


  /*Cree ou ouvre l'attribut MED_NOM_NOR pour �criture */
  if ( _MEDattributeIntWr(_cstpid,MED_NOM_NOR,&numit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);
    goto ERROR;
  }


  /*
   *  Creation/Ouverture du datagroup de niveau
   *   <localentitype>[.<localgeotype>].<remoteentitype>[.<remotegeotype>]
   */
  if (_MEDgetEntityTypeName(_datagroupname2,localentitype) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(localentitype);SSCRUTE(jointname);goto ERROR;
  }
  if ( localentitype != MED_NODE ) {
    if ( _MEDgetInternalGeometryTypeName(fid,_localgeotypename,localgeotype) < 0) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
      ISCRUTE_int(localgeotype);SSCRUTE(jointname);goto ERROR;
    }
      strcat(_datagroupname2,".");
      strcat(_datagroupname2,_localgeotypename);
  }

  strcat(_datagroupname2,".");

  if (_MEDgetEntityTypeName(&_datagroupname2[strlen(_datagroupname2)],remoteentitype) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(remoteentitype);SSCRUTE(jointname);goto ERROR;
  }
  if ( remoteentitype != MED_NODE ) {
    if ( _MEDgetInternalGeometryTypeName(fid,_remotegeotypename,remotegeotype) < 0) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
      ISCRUTE_int(remotegeotype);SSCRUTE(jointname);goto ERROR;
    }
      strcat(_datagroupname2,".");
      strcat(_datagroupname2,_remotegeotypename);
  }


  if ( (_datagroup2 = _MEDdatagroupOuvrir(_cstpid,_datagroupname2)) < 0)
    if ((_datagroup2 = _MEDdatagroupCreer(_cstpid,_datagroupname2)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_datagroupname2);
      SSCRUTE(_path);SSCRUTE(jointname);goto ERROR;
    }

  /*Cree ou ouvre l'attribut MED_NOM_ENT pour �criture */
  if (_MEDattributeIntWr(_datagroup2,MED_NOM_ENT,&_localentitype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_ENT);
    ISCRUTE(_localentitype);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_GEO pour �criture */
  if (_MEDattributeIntWr(_datagroup2,MED_NOM_GEO,&_localgeotype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_GEO);
    ISCRUTE(_localgeotype);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_END pour �criture */
  if (_MEDattributeIntWr(_datagroup2,MED_NOM_END,&_remoteentitype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_END);
    ISCRUTE(_remoteentitype);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_GED pour �criture */
  if (_MEDattributeIntWr(_datagroup2,MED_NOM_GED,&_remotegeotype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_GED);
    ISCRUTE(_remotegeotype);goto ERROR;
  }


  if ( MEDfilterEntityCr(fid, nentity, 1, 2, MED_ALL_CONSTITUENT,
			 MED_NO_INTERLACE,MED_UNDEF_STMODE,
			 MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetWr(_datagroup2,MED_NOM_COR,MED_INTERNAL_INT,&_filter, correspondence) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_COR);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);
    goto ERROR;
  }

  if ((_dataset = _MEDdatasetOuvrir(_datagroup2,MED_NOM_COR)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATASET,MED_NOM_COR);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);SSCRUTE(_datagroupname2);
    goto ERROR;
  }

  if ( _MEDattributeIntWr(_dataset,MED_NOM_NBR,&nentity) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);SSCRUTE(_datagroupname2);
    SSCRUTE(MED_NOM_NBR);ISCRUTE(nentity);goto ERROR;
  }



  _ret=0;
 ERROR:

  if (_dataset>0)     if (_MEDdatasetFermer(_dataset) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASET,MED_NOM_COR);
    ISCRUTE_id(_dataset);
  }

  if (_datagroup2>0)            if (_MEDdatagroupFermer(_datagroup2) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname2);
    ISCRUTE_id(_datagroup2);SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);
  }

  if (_cstpid>0)            if (_MEDdatagroupFermer(_cstpid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_computationstepname);
    ISCRUTE_id(_cstpid);SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);
  }

  if (_jntid>0)            if (_MEDdatagroupFermer(_jntid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,jointname);
    ISCRUTE_id(_jntid);SSCRUTE(_path);
  }

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,localmeshname);
    ISCRUTE_id(_meshid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_JOINT_GRP);
    ISCRUTE_id(_root);
  }

  return _ret;
}
