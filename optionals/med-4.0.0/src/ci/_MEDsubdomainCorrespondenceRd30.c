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

void _MEDsubdomainCorrespondenceRd30(int dummy, ...) {


  med_idt            _root=0,_eqid=0,_meshid=0,_cstpid=0,_datagroup2=0;
  med_idt            _dataset=0;
  med_err            _ret=-1;
  char               _path[MED_JOINT_GRP_SIZE+2*MED_NAME_SIZE+2]=MED_JOINT_GRP;
  char               _computationstepname[2*MED_MAX_PARA+1]="";
  char               _datagroupname2[4*MED_TAILLE_NOM_ENTITE+4]="";
  char               _localgeotypename   [MED_TAILLE_NOM_ENTITE+1]="";
  char               _remotegeotypename   [MED_TAILLE_NOM_ENTITE+1]="";
  med_sorting_type   _sortingtype=0;
  med_filter         _filter        = MED_FILTER_INIT;
  med_geometry_type  _localgeotype=MED_NONE;
  med_geometry_type  _remotegeotype =MED_NONE;
  med_int            _nentity=0;

  MED_VARGS_DECL(const, med_idt            , , fid            );
  MED_VARGS_DECL(const, char * , const       , localmeshname  );
  MED_VARGS_DECL(const, char * , const       , jointname      );
  MED_VARGS_DECL(const, med_int            , , numdt          );
  MED_VARGS_DECL(const, med_int            , , numit          );
  MED_VARGS_DECL(const, med_entity_type    , , localentitype  );
  MED_VARGS_DECL(const, med_geometry_type  , , localgeotype   );
  MED_VARGS_DECL(const, med_entity_type    , , remoteentitype );
  MED_VARGS_DECL(const, med_geometry_type  , , remotegeotype  );
  MED_VARGS_DECL(, med_int *, const          , correspondence );
  MED_VARGS_DECL(, med_err *                ,, fret           );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt            , , fid            );
  MED_VARGS_DEF(const, char * , const       , localmeshname  );
  MED_VARGS_DEF(const, char * , const       , jointname      );
  MED_VARGS_DEF(const, med_int            , , numdt          );
  MED_VARGS_DEF(const, med_int            , , numit          );
  MED_VARGS_DEF(const, med_entity_type    , , localentitype  );
  MED_VARGS_DEF(const, med_geometry_type  , , localgeotype   );
  MED_VARGS_DEF(const, med_entity_type    , , remoteentitype );
  MED_VARGS_DEF(const, med_geometry_type  , , remotegeotype  );
  MED_VARGS_DEF(, med_int *, const          , correspondence );
  MED_VARGS_DEF(, med_err *                ,, fret           );
  if ( localentitype == MED_NODE_ELEMENT ) _localgeotype=MED_NODE ;

  if ( localentitype  != MED_NODE ) _localgeotype=localgeotype ;
  if ( remoteentitype != MED_NODE ) _remotegeotype=remotegeotype ;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

 
  /*
   * Ouverture du dataGroup /JNT/
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * Ouverture du dataGroup <localmeshname>
   */
  if ((_meshid = _MEDdatagroupOuvrir(_root,localmeshname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,localmeshname);
    SSCRUTE(_path);goto ERROR;
  }

  strcat(_path,localmeshname);

  /*
   * Si le Data Group "/JNT/<localmeshname>/<jointname>" n'existe pas, on le cree
   */
  if ((_eqid = _MEDdatagroupOuvrir(_meshid,jointname)) < 0) {
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

  if ((_cstpid = _MEDdatagroupOuvrir(_eqid,_computationstepname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_computationstepname);
    SSCRUTE(_path);goto ERROR;
  }


  /*
   *  Ouverture du datagroup de niveau <localentitype>[.<localgeotype>].<remoteentitype>[.<remotegeotype>]
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

/*   SSCRUTE(_datagroupname2); */
  if ( (_datagroup2 = _MEDdatagroupOuvrir(_cstpid,_datagroupname2)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_datagroupname2);
    SSCRUTE(_path);SSCRUTE(jointname);goto ERROR;
  }


  if ((_dataset = _MEDdatasetOuvrir(_datagroup2,MED_NOM_COR)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATASET,MED_NOM_COR);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);SSCRUTE(_datagroupname2);
    goto ERROR;
  }

  if ( _MEDattrEntierLire(_dataset,MED_NOM_NBR,&_nentity) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);SSCRUTE(_datagroupname2);
    SSCRUTE(MED_NOM_NBR);ISCRUTE(_nentity);goto ERROR;
  }

  if ( MEDfilterEntityCr(fid, _nentity, 1, 2, MED_ALL_CONSTITUENT,
			 MED_NO_INTERLACE,MED_UNDEF_STMODE,
			 MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetRd(_datagroup2,MED_NOM_COR,MED_INTERNAL_INT,&_filter, (unsigned char *) correspondence) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_COR);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);
    goto ERROR;
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

  if (_eqid>0)            if (_MEDdatagroupFermer(_eqid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,jointname);
    ISCRUTE_id(_eqid);SSCRUTE(_path);
  }

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,localmeshname);
    ISCRUTE_id(_eqid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_JOINT_GRP);
    ISCRUTE_id(_eqid);
  }

  va_end(params);
  *fret = _ret;
  return;
}
