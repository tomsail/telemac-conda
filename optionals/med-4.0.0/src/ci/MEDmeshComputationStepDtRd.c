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


med_err
MEDmeshComputationStepDtRd(const med_idt fid,
			   const char * const meshname,
			   const med_int numdt,
			   const med_int numit,
			   med_float * const dt )
{

  med_err  _ret=-1;
  med_idt  _meshid=0,_datagroup1=0;
  char     _meshpath[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1+2*MED_MAX_PARA+1+1]=MED_MESH_GRP;
  char     _datagroupname1[2*MED_MAX_PARA+1]="";
  med_sorting_type _sortingtype;
  med_int          _intsortingtype;
 
  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();
  *dt=0.0;

  strcat( _meshpath, meshname);
  if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);goto ERROR;
  }

  if ( _MEDattrEntierLire(_meshid,MED_NOM_SRT,&_intsortingtype) < 0) {
   MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_SRT);
    ISCRUTE(_intsortingtype);goto ERROR;
  }
  _sortingtype = (med_sorting_type) (_intsortingtype);

  _MEDgetComputationStepName(_sortingtype,numdt,numit,_datagroupname1);

  if ( (_datagroup1 = _MEDdatagroupOuvrir(_meshid,_datagroupname1)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_DOESNTEXIST,MED_ERR_COMPUTINGSTEP,_datagroupname1);
      SSCRUTE(meshname);goto ERROR;
  }


  /*Cree ou ouvre l'attribut MED_NOM_NDT pour lecture */
  if ( _MEDattrEntierLire(_datagroup1,MED_NOM_NDT,&numdt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_meshpath);SSCRUTE(MED_NOM_NDT);
    ISCRUTE(numdt);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_PDT pour lecture */
  if ( _MEDattrFloatLire(_datagroup1,MED_NOM_PDT,dt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_meshpath);SSCRUTE(MED_NOM_PDT);
    RSCRUTE(*dt);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_NOR pour lecture */
  if ( _MEDattrEntierLire(_datagroup1,MED_NOM_NOR,&numit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_meshpath);SSCRUTE(MED_NOM_NOR);
    ISCRUTE(numit); goto ERROR;
  }

  _ret = 0;
 ERROR:

  if (_meshid>0)     if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    SSCRUTE(_datagroupname1);ISCRUTE_id(_datagroup1);
  }

  return _ret;
}
