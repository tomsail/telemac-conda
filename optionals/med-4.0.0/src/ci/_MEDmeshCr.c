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
_MEDmeshCr(const med_idt fid, const char * const root,
	   const char * const meshname, const med_int spacedim,
	   const med_int meshdim, const med_mesh_type meshtype,
	   const char * const description, const char * const dtunit,
	   const med_sorting_type sortingtype,
	   const med_axis_type axistype, const char * const axisname,
	   const char * const axisunit)
{

  med_access_mode _MED_ACCESS_MODE;
  med_err _ret=-1;
  med_idt _root=0,_datagroup1=0;
/* _datagroup2=0,_datagroup3=0; */
  med_int _intmeshtype    = (med_int) meshtype;
  med_int _intaxistype = (med_int) axistype;
  char    _datagroupname1[MED_NAME_SIZE+1]="";
/*   char    _geotypename[MED_NAME_SIZE_ENTITE+1]=""; */
  med_int _lastnumdt=MED_NO_DT, _lastnumit=MED_NO_IT;
  med_int _intsortingtype     =(med_int) MED_SORT_UNDEF;
  med_int _intexistingmeshtype=(med_int) MED_UNDEF_MESH_TYPE;
  med_mesh_type    _existingmeshtype    = meshtype;
  med_sorting_type _existingsortingtype = sortingtype;
  med_size         _nstep=0;

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

  if ( sortingtype == MED_SORT_UNDEF) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,"sortingtype");
    ISCRUTE_int(sortingtype);
    goto ERROR;
  }

  /*
   * Si le DataGroup root n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOuvrir(fid,root)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,root)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,root);
     goto ERROR;
    }

  NOFINALBLANK(meshname,ERROR);
  /*
   * Si le Data Group "/MAA.../<meshname>" n'existe pas, on le cree
   */
  if ((_datagroup1 = _MEDdatagroupOuvrir(_root,meshname)) < 0)
    if ((_datagroup1 = _MEDdatagroupCreer(_root,meshname)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,meshname);
      goto ERROR;
    }

  /*
   * Creation de l'attribut dimension du maillage
   */
  if (meshdim > spacedim) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_PARAMETER,"meshdim");
    ISCRUTE(meshdim);ISCRUTE(spacedim);
    SSCRUTE(root);SSCRUTE(meshname);goto ERROR;
  }

  /*
   * Creation de l'attribut dimension de l'espace
   */
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_ESP,&spacedim) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_ESP);
    ISCRUTE(spacedim);goto ERROR;
  }

  /*
   * Creation de l'attribut dimension du  maillage
   */
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_DIM,&meshdim) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_DIM);
    ISCRUTE(meshdim);goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_TYP */
  if (_MEDattrEntierLire(_datagroup1,MED_NOM_TYP,&_intexistingmeshtype) >= 0)
    _existingmeshtype = (med_mesh_type) (_intexistingmeshtype);

  if ( _existingmeshtype != meshtype ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_WRITE,MED_ERR_ATTRIBUTE_MSG);
    SSCRUTE(MED_NOM_TYP);SSCRUTE(meshname);SSCRUTE(_datagroupname1);
    ISCRUTE_int(_existingmeshtype);ISCRUTE_int(meshtype);goto ERROR;
 
  }
  /*
   * Creation de l'attribut correspondant au type du maillage (MED_STRUCTURE, MED_NON_STRUCTURE)
   * L'attribut "TYP"
   */
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_TYP,&_intmeshtype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_TYP);
    ISCRUTE(_intmeshtype);goto ERROR;
  }

  /*
   * La description associee au maillage
   */
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_DES,MED_COMMENT_SIZE,description) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_DES);
    SSCRUTE(description);goto ERROR;
  }


  /*
   * Creation de l'attribut nom de l'unit~ des dates
   */
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_UNT,MED_SNAME_SIZE,dtunit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_UNT);
    SSCRUTE(dtunit);goto ERROR;
  }

  /*
   *  Test de l'existence de l'attribut de tri des s~quences de calcul
   */
  if ( _MEDattrEntierLire(_datagroup1,MED_NOM_SRT,&_intsortingtype) >= 0) {
    _existingsortingtype = (med_sorting_type) (_intsortingtype);
    /* Nombre d'~tapes de calcul existantes */
    _MEDnObjects(_datagroup1,".",&_nstep);
  }

  if ( (_existingsortingtype != sortingtype) && _nstep ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_COMPUTINGSTEP,MED_ERR_ATTRIBUTE_MSG);
    SSCRUTE(MED_NOM_SRT);SSCRUTE(meshname);SSCRUTE(_datagroupname1);
    ISCRUTE_int(_existingsortingtype);ISCRUTE_int(sortingtype);goto ERROR;
  }

  /*
   * Creation de l'attribut de tri des s~quences de calcul
   */

  _intsortingtype = sortingtype;
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_SRT,&_intsortingtype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_SRT);
    ISCRUTE_int(sortingtype);goto ERROR;
  }

  /*
   * Creation de l'attribut de la derni~re it~ration cr~e
   */
/*   if ( _MEDattributeIntWr(_datagroup1,MED_NOM_NXI,&_lastnumit) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG); */
/*     SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NXI); */
/*     ISCRUTE(_lastnumit);goto ERROR; */
/*   } */


  /*
   * L'attribut "REP"
   */
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_REP,&_intaxistype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_REP);
    ISCRUTE(_intaxistype);goto ERROR;
  }

  /*
   * Attribut "NOM"
   */
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_NOM,spacedim*MED_SNAME_SIZE,axisname) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NOM);
    SSCRUTE(axisname);goto ERROR;
  }

  /*
   * Attribut "UNI"
   */
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_UNI,spacedim*MED_SNAME_SIZE,axisunit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_UNI);
    SSCRUTE(axisunit);goto ERROR;
  }

  /*
   * Cr~ation du datagroup des mailles associ~es ~ un maillage structur~
   * Cel~ permet d'utiliser les routines famille, num~ros/noms optionnels
   * de la m~me fa~on que pour les maillages non structur~s
   */
/*   if ( meshtype == MED_STRUCTURE )  { */

/*     if ((_datagroup2 = _MEDdatagroupCreer(_datagroup1,MED_NOM_MAI)) < 0) { */
/*       MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,root); */
/*       SSCRUTE(meshname);SSCRUTE(MED_NOM_MAI);goto ERROR; */
/*     } */

/*     switch ( meshdim )  { */
/*     case 1 : strcpy(_geotypename,MED_NOM_SE2); */
/*       break; */
/*     case 2 : strcpy(_geotypename,MED_NOM_QU4); */
/*       break; */
/*     case 3 : strcpy(_geotypename,MED_NOM_HE8); */
/*       break; */
/*     case 0 : strcpy(_geotypename,MED_NOM_PO1); */
/*       break; */
/*     default : */
/*       MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_PARAMETER,""); */
/*       ISCRUTE(meshdim);SSCRUTE(root);SSCRUTE(meshname);goto ERROR; */
/*     } */

/*     if ((_datagroup3 = _MEDdatagroupCreer(_datagroup2,_geotypename)) < 0) { */
/*       MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_geotypename); */
/*       SSCRUTE(root);SSCRUTE(meshname);SSCRUTE(MED_NOM_MAI);ISCRUTE_int(_geotypename); */
/*       goto ERROR; */
/*     } */
/*   } */


  /*
   * Nettoyages divers
   */
  _ret = 0;

 ERROR:
/*   if (_datagroup3>0)     if (_MEDdatagroupFermer(_datagroup3) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_geotypename); */
/*     ISCRUTE_id(_datagroup3); */
/*   } */

/*   if (_datagroup2>0)     if (_MEDdatagroupFermer(_datagroup2) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_NOM_MAI); */
/*     ISCRUTE_id(_datagroup2); */
/*   } */

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,meshname);
    ISCRUTE_id(_datagroup1);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,root);
    ISCRUTE_id(_root);
  }

  return _ret;

}

