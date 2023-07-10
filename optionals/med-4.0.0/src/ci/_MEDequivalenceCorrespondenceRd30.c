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
void
_MEDequivalenceCorrespondenceRd30(int dummy, ...) {

  med_idt            _root=0,_eqid=0,_meshid=0,_cstpid=0,_datagroup2=0;
  med_idt            _dataset=0;
  med_err            _ret=-1;
  char               _path[MED_EQUIVALENCE_GRP_SIZE+2*MED_NAME_SIZE+2]=MED_EQUIVALENCE_GRP;
  char               _computationstepname[2*MED_MAX_PARA+1]="";
  char               _datagroupname2[2*MED_TAILLE_NOM_ENTITE+2]="";
  char               _geotypename   [MED_TAILLE_NOM_ENTITE+1]="";
  med_sorting_type   _sortingtype=0;
  med_filter         _filter        = MED_FILTER_INIT;
  med_geometry_type  _geotype=MED_NONE;
  med_int            _nentity=0;

  MED_VARGS_DECL(const, med_idt            , , fid            );
  MED_VARGS_DECL(const, char * , const       , meshname       );
  MED_VARGS_DECL(const, char * , const       , equivname      );
  MED_VARGS_DECL(const, med_int            , , numdt          );
  MED_VARGS_DECL(const, med_int            , , numit          );
  MED_VARGS_DECL(const, med_entity_type    , , entitype       );
  MED_VARGS_DECL(const, med_geometry_type  , , geotype        );
  MED_VARGS_DECL(, med_int *, const          , correspondence );
  MED_VARGS_DECL(, med_err *                ,, fret           );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt            , , fid            );
  MED_VARGS_DEF(const, char * , const       , meshname       );
  MED_VARGS_DEF(const, char * , const       , equivname      );
  MED_VARGS_DEF(const, med_int            , , numdt          );
  MED_VARGS_DEF(const, med_int            , , numit          );
  MED_VARGS_DEF(const, med_entity_type    , , entitype       );
  MED_VARGS_DEF(const, med_geometry_type  , , geotype        );
  MED_VARGS_DEF(, med_int *, const          , correspondence );
  MED_VARGS_DEF(, med_err *                ,, fret           );
  if ( entitype == MED_NODE_ELEMENT ) _geotype=MED_NODE ;

  if ( (geotype / 100 ) > 2 )  {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_GEOMETRIC,MED_ERR_EQUIVALENCE_MSG);
    SSCRUTE(equivname);ISCRUTE_int(geotype);
    goto ERROR;
  }

  if ( entitype != MED_NODE ) _geotype=geotype ;


  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

 
  /*
   * Ouverture du dataGroup /EQS/
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * Ouverture du dataGroup <meshname>
   */
  if ((_meshid = _MEDdatagroupOuvrir(_root,meshname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,meshname);
    SSCRUTE(_path);goto ERROR;
  }

  strcat(_path,meshname);

  /*
   * Si le Data Group "/EQS/<meshname>/<equivname>" n'existe pas, on le cree
   */
  if ((_eqid = _MEDdatagroupOuvrir(_meshid,equivname)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,equivname);
      SSCRUTE(_path);goto ERROR;
  }

  strcat(_path,"/");
  strcat(_path,equivname);

  if ( MEDmeshSortingTypeRd(fid,meshname,&_sortingtype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API," MEDmeshSortingTypeRd");
    SSCRUTE(meshname);ISCRUTE_int(_sortingtype);goto ERROR;
  }

  _MEDgetComputationStepName(_sortingtype,numdt,numit,_computationstepname);

  if ((_cstpid = _MEDdatagroupOuvrir(_eqid,_computationstepname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_computationstepname);
    SSCRUTE(_path);goto ERROR;
  }


  /*
   *  Ouverture du datagroup de niveau <entitype>[.<geotype>]
   */
  if (_MEDgetEntityTypeName(_datagroupname2,entitype) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(entitype);SSCRUTE(equivname);goto ERROR;
  }
  if ( entitype != MED_NODE ) {
    if ( _MEDgetInternalGeometryTypeName(fid,_geotypename,geotype) < 0) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
      ISCRUTE_int(geotype);SSCRUTE(equivname);goto ERROR;
    }
      strcat(_datagroupname2,".");
      strcat(_datagroupname2,_geotypename);
  }

  if ( (_datagroup2 = _MEDdatagroupOuvrir(_cstpid,_datagroupname2)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_datagroupname2);
    SSCRUTE(_path);SSCRUTE(equivname);goto ERROR;
  }


  if ((_dataset = _MEDdatasetOuvrir(_datagroup2,MED_NOM_COR)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATASET,MED_NOM_COR);
    SSCRUTE(_path);ISCRUTE(numdt);ISCRUTE(numit);SSCRUTE(_datagroupname2);
    goto ERROR;
  }

  if ( _MEDattrEntierLire(_dataset,MED_NOM_NBR,&_nentity) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_EQUIVALENCE_MSG);
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
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_EQUIVALENCE_MSG);
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
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,equivname);
    ISCRUTE_id(_eqid);SSCRUTE(_path);
  }

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,meshname);
    ISCRUTE_id(_eqid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_EQUIVALENCE_GRP);
    ISCRUTE_id(_eqid);
  }

  va_end(params);
  *fret = _ret;

  return;
}
