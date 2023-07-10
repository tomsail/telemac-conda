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

void _MEDfieldValueAdvancedRd33(int dummy,...) {

  med_err               _ret=-1;
  med_idt               _gid=0,_locgid=0, _datagroup1=0, _datagroup2=0, _datagroup3=0;
  med_int               _nconstituentpervalue=0,_nvaluesperentityfromloc=0;
  med_int               _nvaluesperentity=0,_profilearraysize=0;
  med_int               _nsectioncell=1;
  med_bool              _filterparameterexist=MED_FALSE;
  med_field_type        _fieldtype=0;
  med_int               _intfieldtype=0;
  med_geometry_type     _locgeotype=0,_sectiongeotype=0;
  med_int               _intlocgeotype=0;
  char _gidname           [MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1]=MED_FIELD_GRP;
  char _datagroupname1    [2*MED_MAX_PARA+1]="";
  char _datagroupname2    [2*MED_TAILLE_NOM_ENTITE+2]="";
  char _profilename       [MED_NAME_SIZE+1]=""; /*TODO DEFAULT? */
  char _meshname          [MED_NAME_SIZE+1]="";
  char _locgidname        [MED_LOCALIZATION_GRP_SIZE+MED_NAME_SIZE+1]="";
  char _sectionmeshname      [MED_NAME_SIZE+1]="";
  char _localizationname  [MED_NAME_SIZE+1]=""; /*TODO DEFAULT? */
  med_filter *               _filter           = NULL;
  med_filter                 _tmpfilter        = MED_FILTER_INIT;
  med_filter                 _paramfilter      = MED_FILTER_INIT;
  med_bool                   _chgt=MED_FALSE,_trsf=MED_FALSE;


  MED_VARGS_DECL(const, med_idt               , , fid              );
  MED_VARGS_DECL(const, char * , const          , fieldname        );
  MED_VARGS_DECL(const, med_int               , , numdt            );
  MED_VARGS_DECL(const, med_int               , , numit            );
  MED_VARGS_DECL(const, med_entity_type       , , entitytype       );
  MED_VARGS_DECL(const, med_geometry_type     , , geotype          );
  MED_VARGS_DECL(const, char * , const          , meshname         );
  MED_VARGS_DECL(const, med_storage_mode      , , storagemode      );
  MED_VARGS_DECL(const, char * , const          , profilename      );
  MED_VARGS_DECL(const, med_switch_mode       , , switchmode       );
  MED_VARGS_DECL(const, med_int               , , componentselect  );
  MED_VARGS_DECL(const, med_filter* , const     , filter           );
  MED_VARGS_DECL(,unsigned char*, const         , value            );
  MED_VARGS_DECL(, med_err *                   ,, fret             );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt               , , fid              );
  MED_VARGS_DEF(const, char * , const          , fieldname        );
  MED_VARGS_DEF(const, med_int               , , numdt            );
  MED_VARGS_DEF(const, med_int               , , numit            );
  MED_VARGS_DEF(const, med_entity_type       , , entitytype       );
  MED_VARGS_DEF(const, med_geometry_type     , , geotype          );
  MED_VARGS_DEF(const, char * , const          , meshname         );
  MED_VARGS_DEF(const, med_storage_mode      , , storagemode      );
  MED_VARGS_DEF(const, char * , const          , profilename      );
  MED_VARGS_DEF(const, med_switch_mode       , , switchmode       );
  MED_VARGS_DEF(const, med_int               , , componentselect  );
  MED_VARGS_DEF(const, med_filter* , const     , filter           );
  MED_VARGS_DEF(,unsigned char*, const         , value            );
  MED_VARGS_DEF(, med_err *                   ,, fret             );

  if (filter) {
    _filter=(med_filter*)(filter); _filterparameterexist=MED_TRUE;
  }
  else {
    _filter=&_tmpfilter;
/*   (*_filter).nentity              = nentity; */
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

  /*
   * Si le Data Group cha n'existe pas => erreur
   */
  strcat(_gidname,fieldname);

  /* Lecture de l'attribut MED_NOM_MAI */
  if ( _MEDattributeStringRdByName(fid,_gidname,MED_NOM_MAI,MED_NAME_SIZE,_meshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(_gidname);SSCRUTE(MED_NOM_MAI);SSCRUTE(_meshname);
    goto ERROR;
  }

  if (strlen(meshname) )
    if (strcmp(_meshname,meshname) ) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,"meshname");
      SSCRUTE(_gidname);SSCRUTE(_meshname);SSCRUTE(meshname);
      goto ERROR;
    }

  if ((_gid = _MEDdatagroupOuvrir(fid,_gidname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_gidname); goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_NCO */
  /* Coh�rence de l'attribut MED_NOM_NCO avec le filtre*/
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
   *  Si le Data Group de niveau 1 <numdt>.<numit> n'existe pas => erreur
   */

  _MEDgetComputationStepName(MED_SORT_DTIT,numdt,numit,_datagroupname1);

  _datagroup1 = 0;
  if ( (_datagroup1 = _MEDdatagroupOuvrir(_gid,_datagroupname1)) < 0 ) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);goto ERROR;
  }


  /*
   * Si le Data Group  de niveau 2 <type_ent>[.<type_geo>] n'existe pas => erreur
   */
  if (_MEDgetFieldEntityGeoTypeName(fid,_datagroupname2,entitytype,geotype) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetFieldEntityGeoTypeName");
    SSCRUTE(fieldname);ISCRUTE_int(entitytype);ISCRUTE_int(geotype);goto ERROR;
  }
  _datagroup2 = 0;
  if ( (_datagroup2 = _MEDdatagroupOuvrir(_datagroup1,_datagroupname2)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_datagroupname2);
    SSCRUTE(fieldname);goto ERROR;
  }


  /*
   * Ouvre le datagroup de niveau 3 <profilename>
   */

  /*MODEL : Il n'y a plus de datagroup de niveau 3 <maa> mais <profilename> */

  /* MODEL : d�placement de l'attribut MED_NOM_PFL */
  /*Cree ou ouvre  l'attribut MED_NOM_PFL   */

  NOFINALBLANK(profilename,ERROR);

  if ( strlen((*_filter).profilename) == 0 ) {  /* idem MED_NOPFL*/

    /* Ecriture de MED_NO_PROFILE_INTERNAL car le datagroup profil ne peut �tre ""*/
    strncpy(_profilename,MED_NO_PROFILE_INTERNAL,MED_NAME_SIZE+1);
    _profilearraysize = MED_UNDEF_SIZE;
  } else {
    strncpy(_profilename,(*_filter).profilename,MED_NAME_SIZE+1);
    _profilename[MED_NAME_SIZE]='\0'; /*On tronque les eventuels noms trop long*/
    if ( ( _profilearraysize = MEDprofileSizeByName( fid,_profilename) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);
      SSCRUTE(_profilename);SSCRUTE("MEDprofileSizeByName");goto ERROR;
    }
  }

  /*INUTILE : le param�tre est positionn� dans  MEDfilterEntityCr */
  if (!_filterparameterexist) (*_filter).profilearraysize =  _profilearraysize;


  _datagroup3 = 0;
  if ((_datagroup3 = _MEDdatagroupOuvrir(_datagroup2,_profilename)) < 0) {
	MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_profilename);
	SSCRUTE(fieldname);SSCRUTE(_profilename);goto ERROR;
  }

  /*MODEL : MED_NOM_NBR nbelem qui contenait nentity*nvalueperentity mais pas nconstituentpervalue  */
  /*Lit le nombre d'entit�s */
  if ( _MEDattrEntierLire(_datagroup3,MED_NOM_NBR,&((*_filter).nentity)) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
    SSCRUTE(MED_NOM_NBR);ISCRUTE((*_filter).nentity);goto ERROR;
  }

  /* Lecture du nom de la localization  */
  if ( _MEDattrStringLire(_datagroup3,MED_NOM_GAU,MED_NAME_SIZE,_localizationname) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
      SSCRUTE(MED_NOM_GAU);SSCRUTE(_localizationname);goto ERROR;
  }
/*   SSCRUTE(_localizationname); */

  /* Lecture du nombre de points d'int�gration */
  /* Ecriture de l'attribut portant le nombre de points de gauss  */
  if ( _MEDattrEntierLire(_datagroup3,MED_NOM_NGA,&_nvaluesperentity) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
    SSCRUTE(MED_NOM_NGA);ISCRUTE(_nvaluesperentity);goto ERROR;
  }


  if (entitytype == MED_NODE_ELEMENT ) {
    if (strlen(_localizationname) ) {
      MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_PARAMETER,_localizationname);
      SSCRUTE(MED_NO_LOCALIZATION);ISCRUTE_int(entitytype);goto ERROR;
    }
    _nvaluesperentityfromloc = geotype % 100;
  } else if (! strcmp(_localizationname,MED_GAUSS_ELNO)) {
    /* Les points de Gauss sont d�finis sur les noeuds de l'element (mot cle) */
    /* le nombre de points de Gauss est egal au nombre de noeuds de l'element */
    _nvaluesperentityfromloc = geotype % 100;
  } else  if ( strlen( _localizationname) ) {
    strcpy(_locgidname,MED_LOCALIZATION_GRP);
    strcat(_locgidname,_localizationname);

    if ((_locgid = _MEDdatagroupOuvrir(fid,_locgidname)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_locgidname);
      goto ERROR;
    }

    if (_MEDattrEntierLire(_locgid,MED_NOM_NBR,&_nvaluesperentityfromloc) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
      SSCRUTE(_localizationname);SSCRUTE(MED_NOM_NBR);ISCRUTE(_nvaluesperentity);goto ERROR;
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

    _nvaluesperentityfromloc*=_nsectioncell;


    if (_MEDattrEntierLire(_locgid,MED_NOM_GEO,&_intlocgeotype) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
      SSCRUTE(_localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE_int(_locgeotype);goto ERROR;
    }
    _locgeotype = (med_geometry_type) _intlocgeotype;

    if ( _locgeotype != geotype ) {
      MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
      SSCRUTE(_localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE_int(_locgeotype);ISCRUTE_int(geotype);goto ERROR;
    }
  } else {
    _nvaluesperentityfromloc = 1;
  }

  if ( _nvaluesperentityfromloc != _nvaluesperentity ) {
    MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(_localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE(_nvaluesperentityfromloc);
    ISCRUTE(_nvaluesperentity);goto ERROR;
  }

  if (!_filterparameterexist) (*_filter).nvaluesperentity=_nvaluesperentity;


  /*Ce n'est plus un param�tre de sortie*/
  /* FT 108 : on r�tablit la bonne valeur externe de locname : MED_NOGAUSS */
/*   if ( ! strcmp(locname,MED_NOGAUSSi)) */
/*     strcpy(locname,MED_NOGAUSS); */


  if (!_filterparameterexist) {

    if ( MEDfilterEntityCr(fid, (*_filter).nentity,         (*_filter).nvaluesperentity,
			   (*_filter).nconstituentpervalue, (*_filter).constituentselect,
			   (*_filter).switchmode,              (*_filter).storagemode, 
			   (*_filter).profilename, MED_UNDEF_SIZE, NULL, &_paramfilter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
      goto ERROR;
    }


    _filter = &_paramfilter;
  }

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
  /*
   * Lecture du champ
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

  */

  switch(_fieldtype)
    {
    case MED_FLOAT64 :
    case MED_FLOAT32 :
    case MED_INT32   :
    case MED_INT64   :
      if ( _MEDdatasetRd(_datagroup3,MED_NOM_CO,_fieldtype,_filter,value) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,MED_NOM_CO);
	SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
/* 	ISCRUTE((void*)value); */
/* 	H5Eprint1(stderr); */
	goto ERROR;
      }
      break;
      /*Le champ est un champ 32bits stocké */
    case MED_INT :
#if defined(HAVE_F77INT64)
      if ( _MEDdatasetRd(_datagroup3,MED_NOM_CO,MED_INT64,_filter,value) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,MED_NOM_CO);
	SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(_datagroupname2);SSCRUTE(_profilename);
	goto ERROR;
      }
#else
      if ( _MEDdatasetRd(_datagroup3,MED_NOM_CO,MED_INT32,_filter,value) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,MED_NOM_CO);
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

/*   if ( pfluse ) { free(pfltab); free(pfltabtmp);} */
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




