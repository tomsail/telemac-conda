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

/* Crée ou ouvre une structure de la forme
  /<meddatatypename>/<meshname>/<PDT,IT|DEFAULT>/<groupname>/
*/

med_idt
_MEDmeshAssociatedGroupCr(const med_idt               fid,
			  const char*  const          rootname,
			  const char*  const          meshname,
			  const med_int               numdt,
			  const med_int               numit,
			  const med_float             dt,
 			  const med_bool              justopen,
			  const char*  const          datagroupname )

{
  med_access_mode       _MED_ACCESS_MODE;
  med_idt               _ret=-1;
  med_idt               _root=0,_meshid=0,_datagroup1=0,_datagroup2=0,_datagroup3=0;
  char                  _datagroupname2   [2*MED_MAX_PARA+1]="";
  med_sorting_type      _sortingtype=0;
  med_bool              _datagp3created = MED_FALSE;

 /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  if (! justopen) {
    if ( _MED_ACCESS_MODE == MED_ACC_RDONLY ) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
      ISCRUTE_int(_MED_ACCESS_MODE);
      goto ERROR;
    }
  }

  /*
   * Récupère l'ordre de tri des séquences de calcul
   * Si le DataGroup MED_MESH_GRP/<meshname> n'existe pas => erreur
   * il doit être crée par MEDmeshCr
   */
  NOFINALBLANK(meshname,ERROR);
  NOFINALBLANK(datagroupname,ERROR);

  if ( MEDmeshSortingTypeRd(fid,meshname,&_sortingtype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API," MEDmeshSortingTypeRd");
    SSCRUTE(meshname);goto ERROR;
  }

  /*
   * Ouverture du DataGroup rootname
   */

  if ((_root = _MEDdatagroupOuvrir(fid,rootname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,rootname);
    goto ERROR;
  }

  /*
   * Si le Data Group <meshname> n'existe pas, on le cree
   */
  if ((_datagroup1 = _MEDdatagroupOuvrir(_root,meshname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,meshname);
    goto ERROR;
  }

  _MEDgetComputationStepName(_sortingtype,numdt,numit,_datagroupname2);

  /*
   * Creation/Ouverture du datagroup <numdt>.<numit>
   */
  if ( (_datagroup2 = _MEDdatagroupOuvrir(_datagroup1,_datagroupname2)) < 0 ) {
    if (justopen) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_COMPUTINGSTEP,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);goto ERROR;
    }
    if ( MEDmeshComputationStepCr(fid,meshname,numdt,numit,numdt,numit,dt) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_COMPUTINGSTEP,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);goto ERROR;
    } else {
      if ( (_datagroup2 = _MEDdatagroupOuvrir(_datagroup1,_datagroupname2)) < 0 ) {
	MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
	SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
	ISCRUTE_id(_datagroup2);goto ERROR;
      }
    }
  }

  /*
   * Si le Data Group <datagroupname> n'existe pas, on le cree
   */
  if ((_datagroup3 = _MEDdatagroupOuvrir(_datagroup2,datagroupname)) < 0) {
    if (justopen) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(datagroupname);
      goto ERROR;
    }
    if ((_datagroup3 = _MEDdatagroupCreer(_datagroup2,datagroupname)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,datagroupname);
      goto ERROR;
    }
    else
      _datagp3created = MED_TRUE;
  }

  _ret = _datagroup3;

 ERROR:

  if ( (_datagroup3 >0) && ( _datagp3created ) )
    if (_MEDdatagroupFermer(_datagroup3) < 0) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,datagroupname);
      ISCRUTE_id(_datagroup3);
    }

  if (_datagroup2>0)     if (_MEDdatagroupFermer(_datagroup2) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname2);
    ISCRUTE_id(_datagroup2);
  }

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,meshname);
    ISCRUTE_id(_datagroup1);
  }

  if (_root>0)     if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,rootname);
    ISCRUTE_id(_root);
  }


  return _ret;

}
