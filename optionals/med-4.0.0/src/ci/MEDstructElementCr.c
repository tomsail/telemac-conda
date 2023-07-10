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

/**\ingroup MEDstructElement
  \brief \MEDstructElementCrBrief
  \param fid \fid
  \param modelname \modelname
  \param modeldim \modeldim
  \param supportmeshname \supportmeshname
  \param sentitytype \sentitytype
  \param sgeotype \sgeotype
  \retval med_geometry_type mgeotype \mgeotype
  \return \error
  \details \MEDstructElementCrDetails
   \li   \MEDstructElementCrmodelnameCm
   \li   \MEDstructElementCrsupportmeshnameCm1
   \li   \MEDstructElementCrsupportmeshnameCm2
   \li   \MEDstructElementCrsupportmeshnameCm3
  \see MEDstructElementName
  \see MEDstructElementGeotype
  \see MEDmeshElementConnectivityWr
  \see MEDmeshElementConnectivityAdvancedWr
 */

/*PENSEZ A DOCUMENTER LE CAS DE PARTICULE (SANS MAILLAGE SUPPORT : MOT CLE A DEFINIR )*/

med_geometry_type
MEDstructElementCr(const med_idt                 fid,
		   const char*             const modelname,
		   const med_int                 modeldim,
		   const char*             const supportmeshname,
		   const med_entity_type         sentitytype,
		   const med_geometry_type       sgeotype
		   )
{
  med_access_mode   _MED_ACCESS_MODE;
  med_err           _err=-1;
  med_idt           _root=0,_elemid=0;
  med_int           _ret=-1;
  char              _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  med_size          _tmpn=0;
  med_geometry_type _stgeotype=0;
  med_int           _medintstgeotype = 0;
  med_int           _medintsgeotype  = sgeotype;
  med_int           _intentitytype   = sentitytype;
  med_int           _nnode=0,_ncell=0;
  med_bool          _chgt=MED_FALSE,_trsf=MED_FALSE;

/*   char           _entitytypename[MED_TAILLE_NOM_ENTITE+1]=""; */

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

  /*
   * Si le DataGroup /STRUCT/ n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOpen(fid,_path)) < 0)
    if ((_root = _MEDdatagroupCrOrderCr(fid,_path)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_path);
     goto ERROR;
    }

  NOFINALBLANK(modelname,ERROR);

  /*
   * Si le DataGroup /STRUCT/<modelname> n'existe pas, on le cree
   */
  if ((_elemid = _MEDdatagroupOpen(_root,modelname)) < 0) {
    if ((_elemid = _MEDdatagroupCreer(_root,modelname)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,modelname);
      SSCRUTE(_path);goto ERROR;
    }
  } else {
    /*
     * Lecture de l'attribut MED_NOM_NEO (num�ro de type g�om�trique associ� � un �l�ment de structure)
     */
    if ( _MEDattrEntierLire(_elemid,MED_NOM_NEO,&_medintstgeotype) < 0 ) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
      SSCRUTE(MED_NOM_NEO);ISCRUTE(_medintstgeotype);
      goto ERROR;
    }
    _stgeotype = _medintstgeotype;
  }
  strcat(_path,modelname);

  /*
   * Creation/Ecriture de l'attribut MED_NOM_DIM (dimension de l'�l�ment)
   */
  if ( _MEDattributeIntWr(_elemid,MED_NOM_DIM,&modeldim) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_DIM);ISCRUTE(modeldim);
    goto ERROR;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_NOM (nom du maillage support)
   */
  if ( _MEDattributeStringWr(_elemid,MED_NOM_NOM,MED_NAME_SIZE,supportmeshname) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_NOM);SSCRUTE(supportmeshname);
    goto ERROR;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_ENT (type d'entit� support)
   */
/*   if (_MEDgetEntityTypeName(_entitytypename,sentitytype) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG); */
/*     ISCRUTE_int(sentitytype);SSCRUTE(_entitytypename);SSCRUTE(_path); */
/*     goto ERROR; */
/*   } */

  if ( _MEDattributeIntWr(_elemid,MED_NOM_ENT,&_intentitytype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_ENT);ISCRUTE(_intentitytype);
    goto ERROR;
  }

  /*
    Lecture dans le maillages support du nombre de noeuds
    S'il n'y a pas de maillage support : 1
  */
  if (strlen(supportmeshname)) {
    if ( (_nnode = MEDmeshnEntity(fid,supportmeshname,MED_NO_DT,MED_NO_IT,
				     MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,
				     &_chgt,&_trsf) )  <= 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      ISCRUTE(_nnode);goto ERROR;
    }
  } else {
    _nnode=1;
  }

  if (strlen(supportmeshname)) {
    if ( (_ncell = MEDmeshnEntity(fid,supportmeshname,MED_NO_DT,MED_NO_IT,
				     MED_CELL,sgeotype,MED_CONNECTIVITY,MED_NODAL,
				     &_chgt,&_trsf) )  < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      ISCRUTE(_ncell);goto ERROR;
    }
  } else {
    _ncell=0;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_GEO (type g�om�trique des mailles support)
   */
  _medintsgeotype = sgeotype;
  if ( _MEDattributeIntWr(_elemid,MED_NOM_GEO,&_medintsgeotype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_GEO);ISCRUTE(_medintsgeotype);
    goto ERROR;
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_NAV (nombre d'attributs variables associ�s � un �l�ment de structure)
   */
/*   if ( _MEDattributeIntWr(_elemid,MED_NOM_NAV,&nvariableattribute) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path); */
/*     SSCRUTE(MED_NOM_NAV);ISCRUTE(nvariableattribute); */
/*     goto ERROR; */
/*   } */

  /*
   * Creation/Ecriture de l'attribut MED_NOM_NAC (nombre d'attributs constant associ�s � un �l�ment de structure)
   */
/*   if ( _MEDattributeIntWr(_elemid,MED_NOM_NAC,&nconstattribute) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path); */
/*     SSCRUTE(MED_NOM_NAC);ISCRUTE(nconstattribute); */
/*     goto ERROR; */
/*   } */


  /*
   *  Nombre d'�l�ments de structure
   */
  if ( !_stgeotype ) {

    /*Si l'�l�ment de struture ne pr�-existait pas,
      il a �t� cr�e en d�but de proc�dure (il y a au moins 1 �l�ment dans le groupe) */
    if ((_err=_MEDnObjects(fid,MED_ELSTRUCT_GRP,&_tmpn)) <0) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_STRUCT,MED_ELSTRUCT_GRP);
      goto ERROR;
    }

    _stgeotype=_tmpn+MED_STRUCT_GEO_INTERNAL;
    _medintstgeotype=_stgeotype;

    /*
     * Ecriture de l'attribut MED_NOM_NEO (num�ro de type g�om�trique associ� � un �l�ment de structure)
     */
    if ( _MEDattributeIntWr(_elemid,MED_NOM_NEO,&_medintstgeotype) < 0 ) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,_path);
      SSCRUTE(MED_NOM_NEO);ISCRUTE(_medintstgeotype);
      goto ERROR;
    }
  }

  _ret = _stgeotype;

 ERROR:

  if (_elemid>0)     if (_MEDdatagroupFermer(_elemid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_elemid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_ELSTRUCT_GRP);
    ISCRUTE_id(_root);
  }

  return _ret;

}
