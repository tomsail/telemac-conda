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

/**\ingroup MEDmesh
  \brief \MEDmeshComputationStepCrBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt1 \numdt1
  \param numit1 \numit1
  \param numdt2 \numdt2
  \param numit2 \numit2
  \param dt2 \dt2
  \retval med_err \error
  \details \MEDmeshComputationStepCrDetails
*/


med_err
MEDmeshComputationStepCr(const med_idt fid,
			 const char * const meshname,
			 const med_int numdt1,
			 const med_int numit1,
			 const med_int numdt2,
			 const med_int numit2,
			 const med_float dt2 )
{

  med_access_mode _MED_ACCESS_MODE;
  med_err  _ret=-1;
  med_idt  _meshid=0,_datagroup1=0,_datagroup2=0,_datagroup3=0;
  char     _meshpath [MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]="";
  char     _datagroupname [2*MED_MAX_PARA+1]="";
  char     _datagroupname2[2*MED_MAX_PARA+1]="";
  char     _datagroupname1bis[2*MED_MAX_PARA+1]="";
  char     _latestcpstname[2*MED_MAX_PARA+1]="";
  char*    _datagroupname1=_datagroupname;
  char     _datagroupname3[2*MED_MAX_PARA+1]="";
  char     _prevcpstname[2*MED_MAX_PARA+1]="";
  char     _pathsrc[MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1+2*MED_MAX_PARA+1+1]="";
  char     _pathdst[MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1+2*MED_MAX_PARA+1+1]="";
  med_bool _datagroup1exist = MED_TRUE;
  med_bool _isasupportmesh = MED_FALSE;
  med_int  _nextdt=MED_NO_DT, _nextit=MED_NO_IT,_pvdt=MED_NO_DT, _pvit=MED_NO_IT;
  med_int  _lastnumdt=MED_NO_DT, _lastnumit=MED_NO_IT;
  med_int  _numdt1=numdt1,_numit1=numit1;
  med_int  _false = 0;
  med_sorting_type _sortingtype;
  med_int          _intsortingtype;
  /*
   * On inhibe le gestionnaire d'erreur
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

/*   strcat( _meshpath, meshname); */
/*   strcat( _pathsrc , meshname);strcat( _pathsrc , "/"); */
/*   strcat( _pathdst , meshname);strcat( _pathdst , "/"); */
/*   if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_meshpath); */
/*     SSCRUTE(_meshid);goto ERROR; */
/*   } */

  if ((_meshid=_MEDmeshDatagroupOpen(fid,meshname,_meshpath,&_isasupportmesh)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(_meshpath); goto ERROR;
  }
  strcat( _pathsrc , _meshpath);strcat( _pathsrc , "/");
  strcat( _pathdst , _meshpath);strcat( _pathdst , "/");

  if ( _MEDattrEntierLire(_meshid,MED_NOM_NXT,&_lastnumdt) < 0) {
/*     MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG); */
/*     SSCRUTE(meshname);SSCRUTE(_meshpath);SSCRUTE(MED_NOM_NXT); */
/*     goto ERROR; */
    _lastnumdt = MED_NO_DT;
  }

  if ( _MEDattrEntierLire(_meshid,MED_NOM_NXI,&_lastnumit) < 0) {
/*     MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG); */
/*     SSCRUTE(meshname);SSCRUTE(_meshpath);SSCRUTE(MED_NOM_NXI); */
/*     goto ERROR; */
    _lastnumit = MED_NO_IT;
  }

 if ( _MEDattrEntierLire(_meshid,MED_NOM_SRT,&_intsortingtype) < 0) {
   MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_SRT);
    ISCRUTE(_intsortingtype);goto ERROR;
  }
  _sortingtype = (med_sorting_type) (_intsortingtype);

  _MEDgetComputationStepName(_sortingtype,numdt1,numit1,_datagroupname);
  _MEDgetComputationStepName(_sortingtype,numdt2,numit2,_datagroupname2);
  _MEDgetComputationStepName(_sortingtype,_lastnumdt,_lastnumit,_latestcpstname);

  /*
    L'utilisateur peut demander la création d'une nouvelle étape de calcul postérieure
    à toutes les autres en indiquant (numdt1 == numdt2) && (numit1 == numit2)
    Celà fonctionne aussi pour MED_NO_DT, NED_NO_IT
  */

  if ( (numdt1 == numdt2) && (numit1 == numit2) ) {
    _numdt1         = _lastnumdt;
    _numit1         = _lastnumit;
    _datagroupname1 = _latestcpstname;
  }
  /*Vérifie que le _datagroupname2 est bien postérieur ou égal au _datagroupname1
   REM: _datagroupname1 peut être la première/dernière séquence de calcul 
   ou une séquence intermédiaire */
  if ( strncmp(_datagroupname2,_datagroupname1,2*MED_MAX_PARA+2) < 0) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_COMPUTINGSTEP,_datagroupname2);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);goto ERROR;
  }

  strcat( _pathsrc , _datagroupname1 );strcat( _pathsrc , "/");
  strcat( _pathdst , _datagroupname2 );strcat( _pathdst , "/");

  /* On accepte l'absence de _datagroupname1 uniquement si
     numdt2 == MED_NO_DT && numit2 == MED_NO_IT */
  if ( (_datagroup1 = _MEDdatagroupOuvrir(_meshid,_datagroupname1)) < 0 ) {
    if ( (numdt2 != MED_NO_DT ) || (numit2 != MED_NO_IT) ) {
      MED_ERR_(_ret,MED_ERR_DOESNTEXIST,MED_ERR_COMPUTINGSTEP,_datagroupname1);
      SSCRUTE(meshname);goto ERROR;
      }
  }

  /*L'étape de calcul à créer ne doit pas déjà exister*/
  if ( (_datagroup2 = _MEDdatagroupOuvrir(_meshid,_datagroupname2)) >= 0 ) {
    MED_ERR_(_ret,MED_ERR_EXIST,MED_ERR_COMPUTINGSTEP,_datagroupname2);
    SSCRUTE(meshname);goto ERROR;
  }


  if ( _datagroup1 > 0 ) {


    /*Lecture NEXT et PREV du datagroup1 */
    if ( _MEDattrEntierLire(_datagroup1,MED_NOM_NXT,&_nextdt) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NXT);
      goto ERROR;
    }

    if ( _MEDattrEntierLire(_datagroup1,MED_NOM_NXI,&_nextit) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NXI);
      goto ERROR;
    }

    /* On vérifie que la nouvelle étape de calcul s'insère correctement :
       < au next du datagroup1 */
/*     if ( !((_nextdt == MED_NO_DT) && (_nextit == MED_NO_IT) )) { */
    if ( (_nextdt != MED_NO_DT) || (_nextit != MED_NO_IT) ) {
      _MEDgetComputationStepName(_sortingtype,_nextdt,_nextit,_datagroupname3);

      if ( strncmp(_datagroupname3,_datagroupname2,2*MED_MAX_PARA+2) <= 0) {
	MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_COMPUTINGSTEP,_datagroupname2);
	SSCRUTE(meshname);SSCRUTE(_datagroupname3);goto ERROR;
      }
    }

    /*On crée la nouvelle étape de calcul (au plus tard, après vérifs...)*/
    if ((_datagroup2 = _MEDdatagroupCreer(_meshid,_datagroupname2)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_datagroupname2);
      SSCRUTE(meshname);goto ERROR;
    }

    /*On crée les datagroups, attributs et liens sur les datatsets et les liens symboliques
      pour avoir une image de l'étape de calcul précedente.*/
    if ( _MEDvisit(fid,_pathsrc,_pathdst,(medvisitorfunc) _MEDlinkobjs ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_VISIT,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_pathsrc);SSCRUTE(_pathdst);
      goto ERROR;
    }

    /*Ecriture NEXT et PREV du nouveau datagroup2 */
    if ( _MEDattributeIntWr(_datagroup2,MED_NOM_NXT,&_nextdt) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_NXT);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_datagroup2,MED_NOM_NXI,&_nextit) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_NXI);
      goto ERROR;
    }


    if ( _MEDattributeIntWr(_datagroup2,MED_NOM_PVT,&_numdt1) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_PVT);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_datagroup2,MED_NOM_PVI,&_numit1) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_PVI);
      goto ERROR;
    }


    /*Modification du NEXT du datagroup1 */
    if ( _MEDattributeIntWr(_datagroup1,MED_NOM_NXT,&numdt2) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NXT);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_datagroup1,MED_NOM_NXI,&numit2) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NXI);
      goto ERROR;
    }

    /*Modification de PREV du ( NEXT de datagroup1 ) s'il existe*/
    if (strlen(_datagroupname3) ) {

      if ( (_datagroup3 = _MEDdatagroupOuvrir(_meshid,_datagroupname3)) < 0 ) {
	MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_COMPUTINGSTEP,_datagroupname3);
	SSCRUTE(meshname);goto ERROR;
      }

      if ( _MEDattrEntierLire(_datagroup3,MED_NOM_PVT,&_pvdt) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	SSCRUTE(meshname);SSCRUTE(_datagroupname3);SSCRUTE(MED_NOM_PVT);
	goto ERROR;
      }

      if ( _MEDattrEntierLire(_datagroup3,MED_NOM_PVI,&_pvit) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	SSCRUTE(meshname);SSCRUTE(_datagroupname3);SSCRUTE(MED_NOM_PVI);
	goto ERROR;
      }
      _MEDgetComputationStepName(_sortingtype,_pvdt,_pvit,_datagroupname1bis);

      if ( strncmp(_datagroupname1,_datagroupname1bis,2*MED_MAX_PARA+2) != 0) {
	MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_COMPUTINGSTEP,_datagroupname1);
	SSCRUTE(meshname);SSCRUTE(_datagroupname1bis);goto ERROR;
      }

      if ( _MEDattributeIntWr(_datagroup3,MED_NOM_PVT,&numdt2) < 0) {
	MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	SSCRUTE(meshname);SSCRUTE(_datagroupname3);SSCRUTE(MED_NOM_PVT);
	goto ERROR;
      }

      if ( _MEDattributeIntWr(_datagroup3,MED_NOM_PVI,&numit2) < 0) {
	MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	SSCRUTE(meshname);SSCRUTE(_datagroupname3);SSCRUTE(MED_NOM_PVI);
	goto ERROR;
      }

    }

  } else { /* Création de la première étape de calcul  */

    /*On crée la nouvelle étape de calcul (au plus tard, après vérifs...) */
    if ((_datagroup2 = _MEDdatagroupCreer(_meshid,_datagroupname2)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_datagroupname2);
      SSCRUTE(meshname);goto ERROR;
    }

    /* Ecriture de MED_NO_IT, MED_NO_DT pour NEXT et PREV de la première
       étape de calcul */
    if ( _MEDattributeIntWr(_datagroup2,MED_NOM_NXT,&numdt2) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_NXT);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_datagroup2,MED_NOM_NXI,&numit2) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_NXI);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_datagroup2,MED_NOM_PVT,&numdt2) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_PVT);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_datagroup2,MED_NOM_PVI,&numit2) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_PVI);
      goto ERROR;
    }

  }

  /*Cree ou ouvre l'attribut MED_NOM_NDT pour écriture */
  if ( _MEDattributeIntWr(_datagroup2,MED_NOM_NDT,&numdt2) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_NDT);
    ISCRUTE(numdt2);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_PDT pour écriture */
  if ( _MEDattrFloatEcrire(_datagroup2,MED_NOM_PDT,&dt2) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_PDT);
    RSCRUTE(dt2);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_NOR pour écriture */
  if ( _MEDattributeIntWr(_datagroup2,MED_NOM_NOR,&numit2) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_NOR);
    ISCRUTE(numit2); goto ERROR;
  }

  /* Une nouvelle étape de calcul est vierge de toute modifiation*/
  if ( _MEDattributeIntWr(_datagroup2,MED_NOM_CGT,&_false) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname3);SSCRUTE(MED_NOM_CGT);
    goto ERROR;
  }


  /* Ecriture de NEXT et PREV  au niveau meshid (dernière étape de calcul créée */
  if ( strncmp(_datagroupname2,_latestcpstname,2*MED_MAX_PARA+2) >= 0) {

    if ( _MEDattributeIntWr(_meshid,MED_NOM_NXT,&numdt2) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_NXT);
      goto ERROR;
    }

    if ( _MEDattributeIntWr(_meshid,MED_NOM_NXI,&numit2) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_NXI);
      goto ERROR;
    }
  }

  _ret = 0;
 ERROR:

  if (_datagroup3>0)     if (_MEDdatagroupFermer(_datagroup3) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname3);
    ISCRUTE_id(_datagroup3);
  }

  if (_datagroup2>0)     if (_MEDdatagroupFermer(_datagroup2) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname2);
    ISCRUTE_id(_datagroup2);
  }

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname1);
    ISCRUTE_id(_datagroup1);
  }

  if (_meshid>0)     if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }

  return _ret;
}
