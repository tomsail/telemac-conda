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

void _MEDsubdomainCorrespondenceSizeInfo30(int dummy, ...) {


  med_err  _ret=-1;
  med_idt  _datagroup1=0,_dataset=0;
  int      _num;
  char     _path[MED_JOINT_GRP_SIZE+2*MED_NAME_SIZE+2+2*MED_MAX_PARA+1+4*MED_TAILLE_NOM_ENTITE+4]=MED_JOINT_GRP;
  char     _datagroupname1[2*MED_TAILLE_NOM_ENTITE+2]="";
  char     _cstpname[2*MED_MAX_PARA+1]="";
  med_sorting_type  _sortingtype=0;
  med_int   _intlocalentitype;
  med_int   _intlocalgeotype;
  med_int   _intremoteentitype;
  med_int   _intremotegeotype;


  MED_VARGS_DECL(const, med_idt              , , fid            );
  MED_VARGS_DECL(const, char * , const         , meshname       );
  MED_VARGS_DECL(const, char * , const         , jointname      );
  MED_VARGS_DECL(const, med_int              , , numdt          );
  MED_VARGS_DECL(const, med_int              , , numit          );
  MED_VARGS_DECL(const, int                  , , corit          );
  MED_VARGS_DECL(, med_entity_type   *, const  , localentitype  );
  MED_VARGS_DECL(, med_geometry_type *, const  , localgeotype   );
  MED_VARGS_DECL(, med_entity_type   *, const  , remoteentitype );
  MED_VARGS_DECL(, med_geometry_type *, const  , remotegeotype  );
  MED_VARGS_DECL(, med_int *, const            , nentitycor     );
  MED_VARGS_DECL(, med_err *                  ,, fret           );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt              , , fid            );
  MED_VARGS_DEF(const, char * , const         , meshname       );
  MED_VARGS_DEF(const, char * , const         , jointname      );
  MED_VARGS_DEF(const, med_int              , , numdt          );
  MED_VARGS_DEF(const, med_int              , , numit          );
  MED_VARGS_DEF(const, int                  , , corit          );
  MED_VARGS_DEF(, med_entity_type   *, const  , localentitype  );
  MED_VARGS_DEF(, med_geometry_type *, const  , localgeotype   );
  MED_VARGS_DEF(, med_entity_type   *, const  , remoteentitype );
  MED_VARGS_DEF(, med_geometry_type *, const  , remotegeotype  );
  MED_VARGS_DEF(, med_int *, const            , nentitycor     );
  MED_VARGS_DEF(, med_err *                  ,, fret           );

  _num = corit -1;

  /*
   * On inhibe le gestionnaire d'erreur
   */

  _MEDmodeErreurVerrouiller();


  if ( MEDmeshSortingTypeRd(fid,meshname,&_sortingtype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API," MEDmeshSortingTypeRd");
    SSCRUTE(meshname);ISCRUTE_int(_sortingtype);goto ERROR;
  }


  strcat( _path, meshname);
  strcat( _path, "/");
  strcat( _path, jointname);
  strcat( _path, "/");
  _MEDgetComputationStepName(_sortingtype,numdt,numit,&_path[strlen(_path)]);
  strcat( _path, "/");

  /*
   * On recupere le nom de <localentitype>[.<localgeotype>].<remoteentitype>[.<remotegeotype>]
   */
  if ( _MEDobjectGetName(fid, _path ,_num, &_path[strlen(_path)]) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(corit);
    goto ERROR;
  }

  if ( (_datagroup1 = _MEDdatagroupOuvrir(fid,_path)) < 0 ) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*lecture localentitype et localgeotype */
  if (_MEDattrEntierLire(_datagroup1,MED_NOM_ENT,&_intlocalentitype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_ENT);
    ISCRUTE(_intlocalentitype);goto ERROR;
  }
  *localentitype = (med_entity_type) _intlocalentitype;

  if (_MEDattrEntierLire(_datagroup1,MED_NOM_GEO,&_intlocalgeotype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_GEO);
    ISCRUTE(_intlocalgeotype);goto ERROR;
  }
  *localgeotype = (med_geometry_type) _intlocalgeotype;

  /*lecture remoteentitype et remotegeotype */
  if (_MEDattrEntierLire(_datagroup1,MED_NOM_END,&_intremoteentitype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_ENT);
    ISCRUTE(_intremoteentitype);goto ERROR;
  }
  *remoteentitype = (med_entity_type) _intremoteentitype;

  if (_MEDattrEntierLire(_datagroup1,MED_NOM_GED,&_intremotegeotype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_GEO);
    ISCRUTE(_intremotegeotype);goto ERROR;
  }
  *remotegeotype = (med_geometry_type) _intremotegeotype;

  if ((_dataset = _MEDdatasetOuvrir(_datagroup1,MED_NOM_COR)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATASET,MED_NOM_COR);
    SSCRUTE(_path);
    goto ERROR;
  }

  if ( _MEDattrEntierLire(_dataset,MED_NOM_NBR,nentitycor) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_NBR);ISCRUTE(*nentitycor);
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  if (_dataset>0)     if (_MEDdatasetFermer(_dataset) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASET,MED_NOM_COR);
    ISCRUTE_id(_dataset);
  }

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_datagroup1);
  }

  va_end(params);
  *fret = _ret;
  return;
}
