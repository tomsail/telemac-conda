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

/**\ingroup MEDparameter
  \brief \MEDparameterValueWrBrief
  \param fid \fid
  \param paramname \paramname
  \param numdt \numdt
  \param numit \numit
  \param dt \dt
  \param value \value
  \retval med_err  \error
  \details \MEDparameterValueWrDetails
 */


med_err
MEDparameterValueWr(const med_idt              fid,
		    const char*  const         paramname,
		    const med_int              numdt,
		    const med_int              numit,
		    const med_float            dt,
		    const unsigned char* const value)
{
  med_err _ret = -1;
  med_idt _paramidt = 0;
  med_idt _cpstidt = 0;
  char _parampath [MED_NUMERICAL_DATA_GRP_SIZE+MED_NAME_SIZE+1] = "";
  char _cpstpath[MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1+2*MED_MAX_PARA+1+1]="";
  char _cpstname [2*MED_MAX_PARA+1]="";
  med_access_mode       _MED_ACCESS_MODE;
  med_int _intparamtype = 0;
  med_parameter_type _paramtype;

  _MEDmodeErreurVerrouiller();
 if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  /* Verification du mode d'acces aux donn�es */
  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }
  if ( _MED_ACCESS_MODE == MED_ACC_RDONLY) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    ISCRUTE_int(_MED_ACCESS_MODE);
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

  /* Si l'etape de calcul n'existe pas, on la cr�e */
  if ((_cpstidt = _MEDdatagroupOuvrir(_paramidt, _cpstname)) < 0 )
    if ((_cpstidt = _MEDdatagroupCreer(_paramidt,_cpstname)) < 0 ) {
      MED_ERR_(_ret, MED_ERR_EXIST, MED_ERR_COMPUTINGSTEP, _cpstname);
      SSCRUTE(paramname);goto ERROR;
    }

  /* ecriture de l'attribut MED_NOM_NDT pour �criture */
  if ( _MEDattributeIntWr(_cpstidt,MED_NOM_NDT,&numdt) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(_cpstname);SSCRUTE(MED_NOM_NDT);
    ISCRUTE(numdt);goto ERROR;
  }
  /* ecriture de l'attribut MED_NOM_PDT */
  if ( _MEDattrFloatEcrire(_cpstidt,MED_NOM_PDT,&dt) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(_cpstname);SSCRUTE(MED_NOM_PDT);
    RSCRUTE(dt);goto ERROR;
  }
  /* ecriture de l'attribut MED_NOM_NOR  */
  if ( _MEDattributeIntWr(_cpstidt,MED_NOM_NOR,&numit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(_cpstname);SSCRUTE(MED_NOM_NOR);
    ISCRUTE(numit); goto ERROR;
  }

  /* on ecrit la valeur du parametre */
  if ( _MEDattrEntierLire(_paramidt,MED_NOM_TYP,&_intparamtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(MED_NOM_TYP);
    goto ERROR;
  }
  _paramtype = (med_field_type) (_intparamtype);
  if (_paramtype == MED_FLOAT64)
    if (_MEDattrFloatEcrire(_cpstidt, MED_NOM_VAL, value) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
      SSCRUTE(paramname);SSCRUTE(MED_NOM_VAL);
    }
  if (_paramtype != MED_FLOAT64)
    if (_MEDattributeIntWr(_cpstidt, MED_NOM_VAL, value) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
      SSCRUTE(paramname);SSCRUTE(MED_NOM_VAL);
    }

  _ret = 0;
 ERROR:

  /* on ferme tout */

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

  return _ret;
}
