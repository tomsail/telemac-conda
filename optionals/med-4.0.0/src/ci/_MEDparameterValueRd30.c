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

void _MEDparameterValueRd30(int dummy, ...) {

  med_err _ret=-1;

  med_idt _paramidt = 0;
  med_idt _cpstidt = 0;
  char _parampath [MED_NUMERICAL_DATA_GRP_SIZE+MED_NAME_SIZE+1] = "";
  char _cpstpath[MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1+2*MED_MAX_PARA+1+1]="";
  char _cpstname [2*MED_MAX_PARA+1]="";
  med_access_mode       _MED_ACCESS_MODE;
  med_int _intparamtype = 0;
  med_parameter_type _paramtype;

  MED_VARGS_DECL(const, med_idt              , , fid       );
  MED_VARGS_DECL(const, char*  , const         , paramname );
  MED_VARGS_DECL(const, med_int              , , numdt     );
  MED_VARGS_DECL(const, med_int              , , numit     );
  MED_VARGS_DECL(,unsigned char*, const        ,value      );
  MED_VARGS_DECL(, med_err *                  ,, fret      );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt              , , fid       );
  MED_VARGS_DEF(const, char*  , const         , paramname );
  MED_VARGS_DEF(const, med_int              , , numdt     );
  MED_VARGS_DEF(const, med_int              , , numit     );
  MED_VARGS_DEF(,unsigned char*, const        ,value      );
  MED_VARGS_DEF(, med_err *                  ,, fret      );


  _MEDmodeErreurVerrouiller();

  /* Verification du mode d'acces aux données */
  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  /* Si le group HDF correspondant au parametre n'existe pas => erreur
     Sinon on l'ouvre */
  NOFINALBLANK(paramname,ERROR);
  strcpy(_parampath, MED_NUMERICAL_DATA_GRP);
  strcat(_parampath, paramname);
  if ((_paramidt = _MEDdatagroupOuvrir(fid,_parampath)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(_parampath); goto ERROR;
  }


  /* On construit le nom du datagroup du pas de temps */
  _MEDgetComputationStepName(MED_SORT_DTIT,numdt,numit,_cpstname);
  strcpy( _cpstpath, _parampath);
  strcat( _cpstpath, "/");
  strcat( _cpstpath, _cpstname);

  /* On ouvre le groupe de la sequence de calcul et on lit la
     valeur du paramètre */
  if ((_cpstidt = _MEDdatagroupOuvrir(_paramidt, _cpstname)) < 0 )  {
      MED_ERR_(_ret, MED_ERR_OPEN, MED_ERR_COMPUTINGSTEP, _cpstname);
      SSCRUTE(paramname);goto ERROR;
  }
  if ( _MEDattrEntierLire(_paramidt,MED_NOM_TYP,&_intparamtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(MED_NOM_TYP);
    goto ERROR;
  }
  _paramtype = (med_field_type) (_intparamtype);
  if (_paramtype == MED_FLOAT64)
    if (_MEDattrFloatLire(_cpstidt, MED_NOM_VAL, value) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
      SSCRUTE(paramname);SSCRUTE(MED_NOM_VAL);
    }
  if (_paramtype != MED_FLOAT64)
    if (_MEDattrEntierLire(_cpstidt, MED_NOM_VAL, value) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
      SSCRUTE(paramname);SSCRUTE(MED_NOM_VAL);
    }

  _ret = 0;
 ERROR :

  if (_cpstidt > 0)
    if (_MEDdatagroupFermer(_cpstidt) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_cpstname);
      ISCRUTE_id(_cpstidt);
  }

  if (_paramidt > 0)
    if (_MEDdatagroupFermer(_paramidt) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_parampath);
      ISCRUTE_id(_paramidt);
    }

  va_end(params);
  *fret = _ret;
  return;
}
