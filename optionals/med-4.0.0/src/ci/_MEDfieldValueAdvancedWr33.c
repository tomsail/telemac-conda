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


void  _MEDfieldValueAdvancedWr33(int dummy,...) {

  med_access_mode       _MED_ACCESS_MODE;
  med_err               _ret=-1;
  med_idt               _gid=0,_locgid=0,_datagroup1=0,_datagroup2=0,_datagroup3=0;
  med_int               _nconstituentpervalue=0, _profilearraysize=0;
  med_bool              _defaultprofileexist=MED_FALSE,_defaultlocalizationexist=MED_FALSE;
  med_bool              _attexist=MED_FALSE;
  med_bool              _profilehaschanged=MED_FALSE,_filterparameterexist=MED_FALSE;
  med_int               _nvaluesperentity=0,_nsectioncell=1;
  med_field_type        _fieldtype=0;
  med_int               _intfieldtype=0;
  med_geometry_type     _locgeotype=0;
  med_int               _intlocgeotype=0;
  char _gidname                [MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1]="";
  char _datagroupname1         [2*MED_MAX_PARA+1]="";
  char _datagroupname2         [2*MED_TAILLE_NOM_ENTITE+2]="";
  char _profilename            [MED_NAME_SIZE+1]="";
  char _locgidname             [MED_LOCALIZATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_LOCALIZATION_GRP;
  char _sectionmeshname        [MED_NAME_SIZE+1]="";
  char _sectiongeotypename     [MED_NAME_SIZE+1]="";
  char _defaultprofilename     [MED_NAME_SIZE+1]=MED_NO_PROFILE;
  char _defaultlocalizationname[MED_NAME_SIZE+1]=MED_NO_LOCALIZATION;
  med_filter *               _filter           = NULL;
  med_filter                 _tmpfilter        = MED_FILTER_INIT;
  med_filter                 _paramfilter      = MED_FILTER_INIT;
  med_int                    _MED_NO_DT = MED_NO_DT;
  med_int                    _MED_NO_IT = MED_NO_IT;
  med_bool                   _chgt=MED_FALSE,_trsf=MED_FALSE;


  MED_VARGS_DECL(const, med_idt               , , fid              );
  MED_VARGS_DECL(const, char * , const          , fieldname        );
  MED_VARGS_DECL(const, med_int               , , numdt            );
  MED_VARGS_DECL(const, med_int               , , numit            );
  MED_VARGS_DECL(const, med_float             , , dt               );
  MED_VARGS_DECL(const, med_entity_type       , , entitytype       );
  MED_VARGS_DECL(const, med_geometry_type     , , geotype          );
  MED_VARGS_DECL(const, med_storage_mode      , , storagemode      );
  MED_VARGS_DECL(const, char * , const          , profilename      );
  MED_VARGS_DECL(const, char * , const          , localizationname );
  MED_VARGS_DECL(const, med_switch_mode       , , switchmode       );
  MED_VARGS_DECL(const, med_int               , , componentselect  );
  MED_VARGS_DECL(const, med_filter* , const     , filter           );
  MED_VARGS_DECL(const, med_int               , , nentity       );
  MED_VARGS_DECL(const, unsigned char*, const   , value            );
  MED_VARGS_DECL(, med_err *                   ,, fret             );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt               , , fid              );
  MED_VARGS_DEF(const, char * , const          , fieldname        );
  MED_VARGS_DEF(const, med_int               , , numdt            );
  MED_VARGS_DEF(const, med_int               , , numit            );
  MED_VARGS_DEF(const, med_float             , , dt               );
  MED_VARGS_DEF(const, med_entity_type       , , entitytype       );
  MED_VARGS_DEF(const, med_geometry_type     , , geotype          );
  MED_VARGS_DEF(const, med_storage_mode      , , storagemode      );
  MED_VARGS_DEF(const, char * , const          , profilename      );
  MED_VARGS_DEF(const, char * , const          , localizationname );
  MED_VARGS_DEF(const, med_switch_mode       , , switchmode       );
  MED_VARGS_DEF(const, med_int               , , componentselect  );
  MED_VARGS_DEF(const, med_filter* , const     , filter           );
  MED_VARGS_DEF(const, med_int               , , nentity       );
  MED_VARGS_DEF(const, unsigned char*, const   , value            );
  MED_VARGS_DEF(, med_err *                   ,, fret             );



if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  if (filter) {
    _filter=(med_filter*)(filter); _filterparameterexist=MED_TRUE;
  }
  else {
    _filter=&_tmpfilter;
    (*_filter).nentity              = nentity;
/*   (*_filter).nvaluesperentity     = nvaluesperentity; */
/*   (*_filter).nconstituentpervalue = nconstituentpervalue; */
    (*_filter).constituentselect       = componentselect;
    (*_filter).switchmode              = switchmode;
    (*_filter).storagemode             = storagemode;
    strcpy((*_filter).profilename,profilename);
/*   (*_filter).profilearraysize        = profilearraysize; */
  }

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

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
   * Si le DataGroup MED_FIELD_GRP n'existe pas => erreur
   */
  NOFINALBLANK(fieldname,ERROR);

  strcpy(_gidname,MED_FIELD_GRP);
  strcat(_gidname,fieldname);
  if ((_gid = _MEDdatagroupOuvrir(fid,_gidname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_gidname); goto ERROR;
  }


  /* Lecture de l'attribut MED_NOM_NCO */
  /* Cohérence de l'attribut MED_NOM_NCO avec le filtre*/
  if (_MEDattrEntierLire(_gid,MED_NOM_NCO,&_nconstituentpervalue) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(MED_NOM_NCO);goto ERROR;
  }

  if (_filterparameterexist) {
    if ((*_filter).nconstituentpervalue != _nconstituentpervalue ) {
      MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_VALUE_MSG);
      ISCRUTE((*_filter).nconstituentpervalue); ISCRUTE(_nconstituentpervalue );
      goto ERROR;
    }
  } else {
    (*_filter).nconstituentpervalue = _nconstituentpervalue;
  }



  /* Lecture de l'attribut MED_NOM_TYP */
  if ( _MEDattrEntierLire(_gid,MED_NOM_TYP,&_intfieldtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(MED_NOM_TYP);
    goto ERROR;
  }
  _fieldtype = (med_field_type) (_intfieldtype);


  /*
   * Creation/Ouverture du datagroup de niveau 2 <numdt>.<numit>
   */
  _MEDgetComputationStepName(MED_SORT_DTIT,numdt,numit,_datagroupname1);

  _datagroup1 = 0;
  if ( (_datagroup1 = _MEDdatagroupOuvrir(_gid,_datagroupname1)) < 0 )
    if ((_datagroup1 = _MEDdatagroupCreer(_gid,_datagroupname1)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);goto ERROR;
    }

  /*Cree ou ouvre l'attribut MED_NOM_NDT pour écriture */
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_NDT,&numdt) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NDT);
    ISCRUTE(numdt);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_PDT pour écriture */
  if ( _MEDattrFloatEcrire(_datagroup1,MED_NOM_PDT,&dt) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_PDT);
    RSCRUTE(dt);goto ERROR;
  }

  /*Cree ou ouvre l'attribut MED_NOM_NOR pour écriture */
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_NOR,&numit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NOR);
    ISCRUTE(numit); goto ERROR;
  }

  /*Cree l'attribut MED_NOM_RDT s'il n'exite pas déjà*/
  _MEDattributeExist(_datagroup1,".",MED_NOM_RDT,&_attexist);
  if (!_attexist )
    if ( _MEDattributeIntWr(_datagroup1,MED_NOM_RDT,&_MED_NO_DT) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_RDT);
      ISCRUTE_int(MED_NO_DT);goto ERROR;
    }

  /*Cree l'attribut MED_NOM_ROR s'il n'exite pas déjà*/
  _MEDattributeExist(_datagroup1,".",MED_NOM_ROR,&_attexist);
  if (!_attexist )
    if ( _MEDattributeIntWr(_datagroup1,MED_NOM_ROR,&_MED_NO_IT) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_ROR);
      ISCRUTE_int(MED_NO_IT);goto ERROR;
    }

  /*
   *  Creation/Ouverture du datagroup de niveau 2 <entitytype>[.<geotype>]
   */

  if (_MEDgetFieldEntityGeoTypeName(fid,_datagroupname2,entitytype,geotype) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetFieldEntityGeoTypeName");
    SSCRUTE(fieldname);ISCRUTE_int(entitytype);ISCRUTE_int(geotype);goto ERROR;
  }

  _datagroup2 = 0;
  if ( (_datagroup2 = _MEDdatagroupOuvrir(_datagroup1,_datagroupname2)) < 0)
    if ((_datagroup2 = _MEDdatagroupCreer(_datagroup1,_datagroupname2)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_datagroupname2);
      SSCRUTE(fieldname);goto ERROR;
    }

  /*Cree ou ouvre  l'attribut MED_NOM_PFL   */
  /* Lit le nom du profil associé s'il en existe déjà un */
  if ( _MEDattrStringLire(_datagroup2,MED_NOM_PFL,MED_NAME_SIZE,_defaultprofilename) < 0 ) {
    strncpy(_defaultprofilename,MED_NO_PROFILE,MED_NAME_SIZE+1);
    _defaultprofileexist = MED_FALSE;
  } else
    _defaultprofileexist = MED_TRUE;

  NOFINALBLANK(profilename,ERROR);

  if ( strlen((*_filter).profilename) == 0 ) {  /* idem MED_NOPFL*/
    /*Ecriture de MED_NO_PROFILE_INTERNAL car un datagroup ne peut pas être de nom ""*/
    strncpy(_profilename,MED_NO_PROFILE_INTERNAL,MED_NAME_SIZE+1);
/*     strncpy(_profilename,MED_NO_PROFILE,MED_NAME_SIZE+1); */
/*     _profilparameterexist=MED_FALSE; */
    _profilearraysize = MED_UNDEF_SIZE;
  } else {
    strncpy(_profilename,(*_filter).profilename,MED_NAME_SIZE+1);
    _profilename[MED_NAME_SIZE]='\0'; /*On tronque les eventuels noms trop long*/
/*     _profilparameterexist=MED_TRUE; */
    if ( ( _profilearraysize = MEDprofileSizeByName( fid,_profilename) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);
      SSCRUTE(_profilename);SSCRUTE("MEDprofileSizeByName");goto ERROR;
    }
  }

  /*INUTILE : le paramètre est positionné dans  MEDfilterEntityCr */
  if (!_filterparameterexist) (*_filter).profilearraysize =  _profilearraysize;


  /*Cree ou ouvre  l'attribut MED_NOM_GAU   */
  /* Lit le nom de la localisation associée s'il en existe déjà une */
  if ( _MEDattrStringLire(_datagroup2,MED_NOM_GAU,MED_NAME_SIZE,_defaultlocalizationname) < 0 ) {
    strncpy(_defaultlocalizationname,MED_NO_LOCALIZATION,MED_NAME_SIZE+1);
    _defaultlocalizationexist = MED_FALSE;
  } else {
    _defaultlocalizationexist = MED_TRUE;
  }
  NOFINALBLANK(localizationname,ERROR);


  /*
   * Cree ou ouvre le _datagroup de niveau 3 <profilename>
   */

  _datagroup3 = 0;
  if (((_datagroup3 = _MEDdatagroupOuvrir(_datagroup2,_profilename)) > 0)
      && ( _MED_ACCESS_MODE == MED_ACC_RDEXT )) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ACCESSMODE,MED_ERR_DATAGROUP_MSG);
    SSCRUTE(_profilename);
    ISCRUTE_int(_MED_ACCESS_MODE);
    goto ERROR;
  } else
    if ( _datagroup3 < 0)
      if ((_datagroup3 = _MEDdatagroupCreer(_datagroup2,_profilename)) < 0) {
	MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_profilename);
	SSCRUTE(fieldname);SSCRUTE(_profilename);goto ERROR;
      }

  /*L'attribut MED_NOM_PFL du datagroup2 est crée après le datagroup profilename
   en cas d'erreur de création */
  /*TODO: IL faudra gérer la suppression d'un profil si nentity==0 et *val=0
    TODO: Vérifier alors qu'il n'est pas le profil par défaut sinon proposer une API
    TODO: pour changer le profil par défaut.*/
  if ( !_defaultprofileexist )
    if ( _MEDattributeStringWr(_datagroup2,MED_NOM_PFL,MED_NAME_SIZE,_profilename) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);
      SSCRUTE(MED_NOM_PFL);SSCRUTE(_profilename);goto ERROR;
    }

  if ( !_defaultlocalizationexist ) {
    if ( _MEDattributeStringWr(_datagroup2,MED_NOM_GAU,MED_NAME_SIZE,localizationname) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);
      SSCRUTE(MED_NOM_GAU);SSCRUTE(localizationname);goto ERROR;
    }
  }
  /*Cree ou ouvre l'attribut MED_NOM_NBR */
  if ( _MEDattributeIntWr(_datagroup3,MED_NOM_NBR,&((*_filter).nentity)) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
    SSCRUTE(MED_NOM_NBR);ISCRUTE((*_filter).nentity);goto ERROR;
  }

  /* Lit le nombre de points d'intégation et vérifie   */
  /* que la localisation porte sur le meme type géométrique  */

  /* Dans le cas de champs aux noeuds par element, on stocke
   * une localisation vide, et le nombre de noeuds dans l'attribut NGA
   */
  /*
    TODO : ? SYMETRISATION MED_NODE_ELEMENT POUR MAILLAGE ?
    TODO : DEUX MT CLES RESERVES POUR DES LOCALISATIONS PARTICULIERES
    TODO : MED_NODESOFCELL_WITHOUTWEIGHT sans élément de réf & sans poids
    TODO : MED_NODESOFCELL_WITHWEIGHT ?
  */
  if (entitytype == MED_NODE_ELEMENT ) {
    if ( strlen(localizationname) != 0) {
      MED_ERR_(_ret,MED_ERR_NOTNULL,MED_ERR_PARAMETER,localizationname);
      ISCRUTE_int(entitytype);goto ERROR;
    }
    _nvaluesperentity = geotype % 100;
  } else if (! strcmp(localizationname,MED_GAUSS_ELNO)) {
    /* TODO: générer une erreur si on est en présence d'un élément de structure.*/
    /* Les points de Gauss sont définis sur les noeuds de l'element (mot cle) */
    /* le nombre de points de Gauss est egal au nombre de noeuds de l'element */
    _nvaluesperentity = geotype % 100;
    /*Impose t'on une localisation, sinon test si "" ?*/
  } else if (strlen(localizationname))  {
    strcat(_locgidname,localizationname);

    if ((_locgid = _MEDdatagroupOuvrir(fid,_locgidname)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_locgidname);
      goto ERROR;
    }

    if (_MEDattrEntierLire(_locgid,MED_NOM_NBR,&_nvaluesperentity) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
      SSCRUTE(localizationname);SSCRUTE(MED_NOM_NBR);ISCRUTE(_nvaluesperentity);goto ERROR;
    }


    if ( entitytype == MED_STRUCT_ELEMENT ) {

      /*
       * Lecture de l'attribut MED_NOM_NOM (nom du maillage support de section)
       */
      if ( _MEDattrStringLire(_locgid,MED_NOM_NOM,MED_NAME_SIZE,_sectionmeshname) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_locgidname);
	SSCRUTE(MED_NOM_NOM);SSCRUTE(_sectionmeshname);
	goto ERROR;
      }

      if (strlen(_sectionmeshname) ) {

	if ( _MEDgetSupportMeshNbOfEntities(fid,_sectionmeshname,NULL,NULL,NULL,
					    &_nsectioncell) < 0) {
	  MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetSupportMeshNbOfEntities");
	  SSCRUTE(_sectionmeshname);ISCRUTE(_nsectioncell);goto ERROR;
	}
      }
    }

    _nvaluesperentity*=_nsectioncell;

    if (_MEDattrEntierLire(_locgid,MED_NOM_GEO,&_intlocgeotype) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
      SSCRUTE(localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE_int(_locgeotype);goto ERROR;
    }
    _locgeotype = (med_geometry_type) _intlocgeotype;

    if ( _locgeotype != geotype ) {
      MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
      SSCRUTE(localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE_int(_locgeotype);
      ISCRUTE_int(geotype);goto ERROR;
    }
  }  else  {
    _nvaluesperentity = MED_NO_IPOINT_INTERNAL ;
  }
/*   strcpy(_localizationname,localizationname); */

  if (!_filterparameterexist) (*_filter).nvaluesperentity=_nvaluesperentity;



  /* Cree ou ouvre l'attribut MED_NOM_GAU         */
  /* Ecriture de la localisation des pts de gauss  */
  if ( _MEDattributeStringWr(_datagroup3,MED_NOM_GAU,MED_NAME_SIZE,localizationname) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
      SSCRUTE(MED_NOM_GAU);SSCRUTE(localizationname);goto ERROR;
  }

  /* Cree ou ouvre l'attribut MED_NOM_NGA         */
  /* Ecriture de l'attribut portant le nombre de points de gauss  */
  if ( _MEDattributeIntWr(_datagroup3,MED_NOM_NGA,&_nvaluesperentity) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
    SSCRUTE(MED_NOM_NGA);ISCRUTE(_nvaluesperentity);goto ERROR;
  }

  /* TODO : ?MODE ECRASEMENT ?*/
  /* Si une localisation par défaut existe déjà <=> Un profil par défaut existe déjà :
     Si le profil courant est le profil par défaut met à jour la localisation par défaut */
  if ( _defaultprofileexist && !strcmp((*_filter).profilename,_defaultprofilename) ) {
    if ( _MEDattributeStringWr(_datagroup2,MED_NOM_GAU,MED_NAME_SIZE,localizationname) < 0) {
	  MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
	  SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
	  SSCRUTE(MED_NOM_GAU);SSCRUTE(localizationname);goto ERROR;
	}
      }

  if (!_filterparameterexist) {

#ifdef _DEBUG_
    ISCRUTE((*_filter).nentity              );
    ISCRUTE((*_filter).nvaluesperentity     );
    ISCRUTE((*_filter).nconstituentpervalue );
    ISCRUTE((*_filter).constituentselect       );
    ISCRUTE_int((*_filter).switchmode              );
    ISCRUTE((*_filter).filterarraysize         );
    ISCRUTE((*_filter).profilearraysize        );
    ISCRUTE_int((*_filter).storagemode             );
    SSCRUTE((*_filter).profilename             );
#endif

    if ( MEDfilterEntityCr(fid, (*_filter).nentity,         (*_filter).nvaluesperentity,
			   (*_filter).nconstituentpervalue, (*_filter).constituentselect,
			   (*_filter).switchmode,              (*_filter).storagemode, 
			   (*_filter).profilename, MED_UNDEF_SIZE, NULL, &_paramfilter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
      goto ERROR;
    }
    _filter = &_paramfilter;
  }

  /*CELA N'A PLUS DE SENS AVEC LE MODELE INTERNE 3.0*/
  /* Vérifie si le profil a changé (l'API MED ne permet pas de modifier un profil existant) */
  /*   if (strcmp(_profilename, _defaultprofilename)) */
  /*     _profilehaschanged = 1;               le profil a changé*/
  /*   else */
  /*     _profilehaschanged = 0;               le profil n'a pas changé*/


  /*
   * Ecriture du champ
   */

  /* Tous les types med_field_type sont maintenant autorisés dans MEDfieldCr mais :

   Avant la 3.3.0 seuls les types : MED_FLOAT64, MED_INT32 et MED_INT64 étaient autorisés dans MEDfieldCr et seuls les types med_int et med_float64 pouvaient être utilisés en C.
     La configuration du med_int était prédominante sur le choix du type de champ pour définir de la taille de stockage interne.
     Il faut garder à l'esprit que les étapes d'écriture et de lecture ne se font pas forcément avec la même configuration de med_int.
 
   A l'écriture :
      - si med_int=int  les champs MED_INT32 sont stockés   en 32bits
      - si med_int=int  les champs MED_INT64 sont interdits
      - si med_int=long les champs MED_INT32 sont stockés   en 64bits
      - si med_int=long les champs MED_INT64 sont stockés   en 64bits

   A la lecture :
      - si med_int=int  les champs MED_INT32 sont lus       en 32bits avec conversion 64->32 s'il avait été stocké en 64bits (configuration écriture med_int=long)
      - si med_int=int  les champs MED_INT64 ne pouvaient pas être lu (pour prevenir la perte d'information)
      - si med_int=long les champs MED_INT32 sont lus       en 64bits avec conversion 32->64 s'il avait été stocké en 32bits (configuration écriture med_int=int)
      - si med_int=long les champs MED_INT64 sont lus       en 64bits

   Depuis la 3.3.0 en plus des types MED_FLOAT64, MED_INT32 et MED_INT64, les types MED_FLOAT32 et MED_INT sont autorisés. 
     Aux types med_int et med_float64 utilisés en C sont ajoutés les types med_float32, med_int32 et med_int64.
     Si la plateforme possède des entiers 64bits testé à la configuration.

   A l'écriture :  
      - si med_int=int  les champs MED_INT32 sont toujours  stockés              en 32bits  (utiliser med_int32 ou med_int   )
      - si med_int=int  les champs MED_INT64 sont désormais autorisés et stockés en 64bits  (utiliser med_int64              )
      - si med_int=int  les champs MED_INT   sont désormais acceptés  et stockés en 32bits  (utiliser med_int   ou med_int32 )
      - si med_int=long les champs MED_INT32 sont désormais stockés              en 32bits  (utiliser med_int32) 
      - si med_int=long les champs MED_INT64 sont toujours  autorisés et stockés en 64bits  (utiliser med_int64 ou med_int )
      - si med_int=long les champs MED_INT   sont désormais acceptés  et stockés en 64bits  (utiliser med_int ou med_int64 ) 

   A la lecture :  
      - si med_int=int  les champs MED_INT32 sont toujours    lus                en 32bits                 (utiliser med_int32 ou med_int)
      - si med_int=int  les champs MED_INT64 sont acceptés et lus                en 64bits sans conversion (utiliser med_int64)
      - si med_int=int  les champs MED_INT   sont acceptés et lus                en 32bits avec conversion si necessaire (0 si > maxint32 , utiliser med_int ou med_int32)
      - si med_int=long les champs MED_INT32 sont toujours    lus                en 32bits sans conversion (utiliser le type med_int32)
      - si med_int=long les champs MED_INT64 sont toujours    lus                en 64bits sans conversion (utiliser le type med_int64 ou med_int)
      - si med_int=long les champs MED_INT   sont acceptés et lus                en 64bits avec conversion  si necessaire (utiliser le type med_int32)
  
REM : 
   Sur un Unix 32 bits sur architecture 64bits il est possible d'utiliser des MED_INT64, l'étape de configuration vérifier qu'elle peut utiliser ou définit le type C int64_t
REM2:
   A lecture d'un fichier < 3.3.0 avec une bibliothèque >= 3.3.0 configurée avec med_int=long :
     - Si le fichier contient un champ MED_INT32, le driver 3.0 de la bibliothèque > 3.3.0 relis en 64 bits ce qui provoque une erreur de segmentation si l'allocation n'est pas adéquate.
     - Il est du coup plus cohérent d'utiliser le driver 33 pour la lecture de fichier < 33.
  */  
  switch(_fieldtype)
    {
    case MED_FLOAT64 :
    case MED_FLOAT32 :
    case MED_INT64 :
    case MED_INT32 :
      if ( _MEDdatasetWr(_datagroup3,MED_NOM_CO,_fieldtype,_filter,value) < 0) {
	MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_CO);
	SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
	goto ERROR;
      }
      break;

    case MED_INT:
#if defined(HAVE_F77INT64)
      if ( _MEDdatasetWr(_datagroup3,MED_NOM_CO,MED_INT64,_filter,value) < 0) {
	MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_CO);
	SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
	goto ERROR;
      }
#else
      if ( _MEDdatasetWr(_datagroup3,MED_NOM_CO,MED_INT32,_filter,value) < 0){
	MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_CO);
	SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
	goto ERROR;
      }
#endif
      break;

    default :
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_RANGE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);ISCRUTE_int(_fieldtype);
      goto ERROR;
    }


  /*
   * On ferme tout
   */

  _ret = 0;

 ERROR:

  if (!_filterparameterexist) {
    if ( MEDfilterClose(_filter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
      goto ERROR;
    }
  }

  if (_datagroup3>0)     if (_MEDdatagroupFermer(_datagroup3) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_profilename);
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

  if (_gid>0)            if (_MEDdatagroupFermer(_gid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_gidname);
    ISCRUTE_id(_gid);
  }

  if (_locgid>0)     if (_MEDdatagroupFermer(_locgid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_locgidname);
    ISCRUTE_id(_locgid);
  }

  va_end(params);
  *fret = _ret;
  return;
}

