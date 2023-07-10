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
  \brief \MEDparameterCrBrief
  \param fid \fid
  \param paramname \paramname
  \param paramtype \paramtype
  \param description \description
  \param dtunit \dtunit
  \retval med_err  \error
  \details \MEDparameterCrDetails
 */

med_err
MEDparameterCr(const med_idt fid,
	       const char * const paramname, 
	       const med_parameter_type paramtype,
	       const char* const description,
	       const char * const dtunit
	      )
{
  med_err _ret=-1;
  med_idt _root=0,_datagroup1=0;
  med_access_mode _MED_ACCESS_MODE;
  char _datagroupname1    [MED_NAME_SIZE+1]="";
  med_int _paramtype = (med_int) paramtype;


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
   * Si le DataGroup MED_NUMERICAL_DATA_GRP n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOuvrir(fid,MED_NUMERICAL_DATA_GRP)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,MED_NUMERICAL_DATA_GRP)) < 0) {
     MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_NUMERICAL_DATA_GRP);
     goto ERROR;
    }
  NOFINALBLANK(paramname,ERROR); 

  /*
   * Si le Data Group "/NUM_DATA/<paramname>" n'existe pas, on le cree
   */
  if ((_datagroup1 = _MEDdatagroupOuvrir(_root,paramname)) < 0)
    if ((_datagroup1 = _MEDdatagroupCreer(_root,paramname)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_NUMERICAL_DATA_GRP);
      SSCRUTE(paramname);goto ERROR;
    }
 
  /*
   * Ecriture des infos relatives au parametre : type, dtunit, description
   */
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_TYP,&_paramtype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_TYP);
    ISCRUTE(_paramtype);goto ERROR;
  }
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_DES,MED_COMMENT_SIZE,description) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_DES);
    SSCRUTE(description);goto ERROR;
  }
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_UNT,MED_SNAME_SIZE,dtunit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_UNT);
    SSCRUTE(dtunit);goto ERROR;
  }

  _ret = 0;
 ERROR :

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname1);
    ISCRUTE_id(_datagroup1);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_NUMERICAL_DATA_GRP);
    ISCRUTE_id(_root);
  }

  return _ret;
}
