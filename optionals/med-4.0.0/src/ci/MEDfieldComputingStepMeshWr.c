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

/**\ingroup MEDfield
   \brief \MEDfieldComputingStepMeshWrBrief 
   \param fid \fid 
   \param fieldname \fieldname 
   \param numdt \numdt 
   \param numit \numit 
   \param meshnumdt \meshnumdt 
   \param meshnumit \meshnumit 
   \retval med_err \error 
   \details \MEDfieldComputingStepMeshWrDetails 
 */
med_err
MEDfieldComputingStepMeshWr(const med_idt fid,
			    const char * const fieldname,
			    const med_int numdt,
			    const med_int numit,
			    const med_int meshnumdt,
			    const med_int meshnumit) {

  med_access_mode  _MED_ACCESS_MODE;
  med_err          _ret=-1;
  med_idt          _gid=0;
  char    _path          [(MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1)+2*MED_MAX_PARA+1]=MED_FIELD_GRP;

  /*   char    _datagroupname1[2*MED_MAX_PARA+1]=""; */
  /*   char    _cstpname[2*MED_MAX_PARA+1]=""; */

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
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
   * On construit le nom du datagroup
   */
  strcat(_path,fieldname);
  strcat(_path,"/");
  _MEDgetComputationStepName(MED_SORT_DTIT,numdt,numit,&_path[strlen(_path)]);


  if ((_gid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }


  /*
   * Ecriture des attributs
   */

  if (_MEDattributeIntWr(_gid,MED_NOM_RDT,  &meshnumdt) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_RDT);
    SSCRUTE(_path);ISCRUTE(meshnumdt);goto ERROR;
  }

  if (_MEDattributeIntWr(_gid,MED_NOM_ROR, &meshnumit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_NOM_ROR);
    SSCRUTE(_path);ISCRUTE(meshnumit);goto ERROR;
  }


  _ret = 0;

 ERROR:


  if (_gid>0)            if (_MEDdatagroupFermer(_gid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_gid);
  }

  return _ret;
}
