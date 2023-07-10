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
  \brief \MEDparameterComputationStepInfoBrief
  \param fid \fid
  \param paramname \paramname
  \param csit \csit
  \param numdt \numdt
  \param numit \numit
  \param dt \dt
  \retval med_err  \error
  \details \MEDparameterComputationStepInfoDetails
 */

med_err
MEDparameterComputationStepInfo(const med_idt      fid,
				const char * const paramname,
				const int          csit,
				med_int * const    numdt,
				med_int * const    numit,
				med_float * const  dt )
{
  med_err _ret = -1, _err = -1;
  med_idt _cpstidt = 0;
  char    _path[(MED_NUMERICAL_DATA_GRP_SIZE+MED_NAME_SIZE+1)+2*MED_MAX_PARA+1]=MED_NUMERICAL_DATA_GRP;
  int     _num=csit-1;
  char    _cstpname[2*MED_MAX_PARA+1]="";

  _MEDmodeErreurVerrouiller();
  
  /* On recupere le nom du pas de temps */
  strcat(_path, paramname);
  if ( _MEDobjectGetName(fid, _path ,_num, _cstpname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(_num);
    goto ERROR;
  }

  /* on ouvre le groupe HDF correspondant */
  strcat(_path,"/");
  strcat(_path,_cstpname);
  if ((_cpstidt = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /* Lecture des attributs */
  if (_MEDattrEntierLire(_cpstidt, MED_NOM_NDT, (med_int*) numdt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NDT);
    SSCRUTE(_path);ISCRUTE(*numdt);goto ERROR;
  }
  if (_MEDattrFloatLire(_cpstidt, MED_NOM_PDT, (med_float*) dt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_PDT);
    SSCRUTE(_path);RSCRUTE(*dt);goto ERROR;
  }
  if (_MEDattrEntierLire(_cpstidt, MED_NOM_NOR, (med_int*) numit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_NOM_NOR);
    SSCRUTE(_path);ISCRUTE(*numit);goto ERROR;
  }

  _ret = 0;
 ERROR:

  /* on ferme tout */
  if (_cpstidt > 0)            
    if (_MEDdatagroupFermer(_cpstidt) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
      ISCRUTE_id(_cpstidt);
    }

  return _ret;
}
