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

void _MEDmeshAdvancedRd30(int dummy, ...)
{
  med_access_mode       _MED_ACCESS_MODE;
  med_err               _ret=-1;
  med_idt               _meshid=0, _elemid=0;
  med_idt               _datagroup=0,_fdatagroup=0,_dataset=0;
  med_idt               _datagroup1=0,_datagroup2=0,_datagroup3=0,_datagroup4=0;
  char                  _meshpath         [MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]="";
  char                  _strpath          [MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  char                  _datagroupname1   [2*MED_MAX_PARA+1]="";
  char                  _datagroupname2   [MED_TAILLE_NOM_ENTITE+1]="";
  char                  _datagroupname3   [MED_TAILLE_NOM_ENTITE+1]="";
  char                  _datagroupname4   [MED_NAME_SIZE+1]="";
  char                  _profilename      [MED_NAME_SIZE+1]="";
  char                  _geotypename      [MED_TAILLE_NOM_ENTITE+1]="";
/*   char                  *_datasetname=NULL; */
  char                  _datasetname[MED_TAILLE_NOM_ENTITE+1]="";
  char                  _MED_NOM_COO[]=MED_NOM_COO, _MED_NOM_NOD[]=MED_NOM_NOD,_MED_NOM_DES[]=MED_NOM_DES;
  med_bool              _filterparameterexist=MED_FALSE;
/*   int                   _entitydim=0,_entitynnodes=0,_entityndes=0; */
  med_filter *          _filter           = NULL;
  med_filter            _tmpfilter        = MED_FILTER_INIT;
  med_filter            _paramfilter      = MED_FILTER_INIT;
  med_int               _nconstituentpervalue=0,_spacedim=0;
  med_sorting_type      _sortingtype=0;
  med_int               _intsortingtype=0;
  med_int               _false=0,_true=1,_true_or_false=_false;
/*   med_int               _profilearraysize=0; */
  char                  _MED_NO_PROFILE_INTERNAL[]=MED_NO_PROFILE_INTERNAL;
  char *                _tmpprofilename=_MED_NO_PROFILE_INTERNAL;
  med_internal_type     _datatype;
  med_int               _nvalueperentity=0;
  med_int               _intmeshtype=0;
  med_geometry_type     _sgeotype      = MED_NONE;
  med_int               _medintsgeotype= 0;
  med_bool              _chgt=MED_FALSE,_trsf=MED_FALSE;
  med_bool              _isasupportmesh=MED_FALSE;
  char                  _supportmeshname[MED_NAME_SIZE+1]="";
  med_bool              _datasetexist=MED_FALSE,_isasoftlink=MED_FALSE;


  MED_VARGS_DECL(const, med_idt               , , fid         );
  MED_VARGS_DECL(const, char*  , const          , meshname    );
  MED_VARGS_DECL(const, med_data_type         , , meddatatype );
  MED_VARGS_DECL(const, char*  , const          , datasetname );
  MED_VARGS_DECL(const, med_internal_type     , , datatype    );
  MED_VARGS_DECL(const, med_int               , , numdt       );
  MED_VARGS_DECL(const, med_int               , , numit       );
  MED_VARGS_DECL(const, med_entity_type       , , entitytype  );
  MED_VARGS_DECL(const, med_geometry_type     , , geotype     );
  MED_VARGS_DECL(const, med_connectivity_mode , , cmode       );
  MED_VARGS_DECL(const, med_storage_mode      , , storagemode );
  MED_VARGS_DECL(const, char * , const          , profilename );
  MED_VARGS_DECL(const, med_switch_mode       , , switchmode  );
  MED_VARGS_DECL(const, med_int               , , dimselect   );
  MED_VARGS_DECL(const, med_filter * , const    , filter      );
  MED_VARGS_DECL(,unsigned char*, const         , value       );
  MED_VARGS_DECL(, med_err *                   ,, fret        );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt               , , fid         );
  MED_VARGS_DEF(const, char*  , const          , meshname    );
  MED_VARGS_DEF(const, med_data_type         , , meddatatype );
  MED_VARGS_DEF(const, char*  , const          , datasetname );
  MED_VARGS_DEF(const, med_internal_type     , , datatype    );
  MED_VARGS_DEF(const, med_int               , , numdt       );
  MED_VARGS_DEF(const, med_int               , , numit       );
  MED_VARGS_DEF(const, med_entity_type       , , entitytype  );
  MED_VARGS_DEF(const, med_geometry_type     , , geotype     );
  MED_VARGS_DEF(const, med_connectivity_mode , , cmode       );
  MED_VARGS_DEF(const, med_storage_mode      , , storagemode );
  MED_VARGS_DEF(const, char * , const          , profilename );
  MED_VARGS_DEF(const, med_switch_mode       , , switchmode  );
  MED_VARGS_DEF(const, med_int               , , dimselect   );
  MED_VARGS_DEF(const, med_filter * , const    , filter      );
  MED_VARGS_DEF(,unsigned char*, const         , value       );
  MED_VARGS_DEF(, med_err *                   ,, fret        );

  _datatype=datatype;


  if (filter) {
    _filter=(med_filter*)(filter); _filterparameterexist=MED_TRUE;
  }
  else {
    _filter=&_tmpfilter;
/*     (*_filter).nentity              = nentity; */
    (*_filter).nvaluesperentity     = 1;
/*   (*_filter).nconstituentpervalue = nconstituentpervalue; */
    (*_filter).constituentselect       = dimselect;
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

  /*
   * Si le DataGroup MED_MESH_GRP n'existe pas => erreur
   */
  NOFINALBLANK(meshname,ERROR);


  if ((_meshid=_MEDmeshDatagroupOpen(fid,meshname,_meshpath,&_isasupportmesh)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(_meshpath); goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_ESP  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_ESP,&_spacedim) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_ESP);goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_TYP  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_TYP,&_intmeshtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_TYP);ISCRUTE(_intmeshtype);goto ERROR;
  }


  /*DEBUT  TODO : Externaliser ce traitement lié aux éléments de structures */
  if ( entitytype == MED_STRUCT_ELEMENT ) {
    if ( MEDstructElementName(fid, geotype,_geotypename) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDstructElementName");
      ISCRUTE_int(geotype);goto ERROR;
    }

    if (meddatatype != MED_VARIABLE_ATTRIBUTE) {
      strcat(_strpath,_geotypename);

      /*
       * Si le DataGroup /STRUCT/<elementname> n'existe pas => erreur
       */
      if ((_elemid = _MEDdatagroupOpen(fid,_strpath)) < 0)  {
	MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_strpath);
	goto ERROR;
      }

       /*
       * Lecture de l'attribut MED_NOM_NOM (nom du maillage support)
       */
      /* Chercher plutôt ds le maillage support et supprimer les attributs NBM et NBN */
      if ( _MEDattrStringLire(_elemid,MED_NOM_NOM,MED_NAME_SIZE,_supportmeshname) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_strpath);
	SSCRUTE(MED_NOM_NOM);SSCRUTE(_supportmeshname);
	goto ERROR;
      }

      /*
       * Lecture de l'attribut MED_NOM_GEO (type géométrique des mailles support)
       */
      if ( _MEDattrEntierLire(_elemid,MED_NOM_GEO,&_medintsgeotype) < 0 ) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_strpath);
	SSCRUTE(MED_NOM_GEO);ISCRUTE(_medintsgeotype);
	goto ERROR;
      }
      _sgeotype = _medintsgeotype;

      if (strlen(_supportmeshname) )
	if ( (_nvalueperentity = MEDmeshnEntity(fid,_supportmeshname,MED_NO_DT,MED_NO_IT,
						   MED_CELL,_sgeotype,MED_CONNECTIVITY,MED_NODAL,
						   &_chgt,&_trsf) )  < 0) {
	  MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
	  ISCRUTE(_nvalueperentity);goto ERROR;
	}

      if (_nvalueperentity ) {
	if (!_nconstituentpervalue) _nconstituentpervalue=_sgeotype%100;
      } else {
	_nvalueperentity=1;
	if (!_nconstituentpervalue) _nconstituentpervalue=1;
      }

    }
  }

  if (meddatatype == MED_VARIABLE_ATTRIBUTE) {
    if ( MEDstructElementVarAttInfoByName(fid,
					  _geotypename,
					  datasetname,
					  (med_attribute_type* const) &_datatype,
					  &_nconstituentpervalue
					  ) < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDstructElementVarAttInfoByName");
      ISCRUTE_int(geotype);SSCRUTE(datasetname);
      ISCRUTE_int(_datatype);ISCRUTE(_nconstituentpervalue);goto ERROR;
    }
    _nvalueperentity=1;
  }

  /* FIN TODO*/

  /* Si un nom de dataset est précisé, il est prioritaire sur la detection automatique en
     fonction des paramètres meddatatype et cmode */
  if ( !strlen(datasetname) ) {
    if ( _MEDgetDatasetName( _datasetname, meddatatype, cmode ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetDatasetname");
      ISCRUTE_int(meddatatype);ISCRUTE_int(cmode);SSCRUTE( _datasetname);goto ERROR;
    }
  } else
    strncpy(_datasetname,datasetname,MED_TAILLE_NOM_ENTITE+1);


  /* Si un type de dataset est précisé, il est prioritaire sur la détection automatique en
     fonction des paramètres meddatatype et cmode */
  /* TODO : renommer datatype en idatatype (internal) */
  if ( _datatype == MED_INTERNAL_UNDEF)
    if ( _MEDgetDatatype( &_datatype, meddatatype, cmode ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetDatatype");
      ISCRUTE_int(meddatatype);ISCRUTE_int(cmode);ISCRUTE_int(_datatype);goto ERROR;
    }

  /* Vérification :
     Le type de données MED traitées sont-elles compatibles avec le type de maillage ?
  */
  /* REM : Si datasetname ou datatype ne sont pas précisés, meddatatype doit l'être */
  if ( meddatatype != MED_UNDEF_DATATYPE )
    if ( _MEDmeshtypeCompatibility(meddatatype, _intmeshtype) < 0) {
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_MEDDATATYPE,MED_ERR_VALUE_MSG);
      ISCRUTE_int(meddatatype);ISCRUTE(_intmeshtype);goto ERROR;
    }

  if ( ! (_nvalueperentity && _nconstituentpervalue) )
    if ( _MEDgetDatasetParameter( meddatatype, _spacedim, entitytype, geotype, cmode,
				  &_nvalueperentity,&_nconstituentpervalue) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetDatasetParameter");
      ISCRUTE_int(meddatatype);ISCRUTE_int(cmode);ISCRUTE(_nvalueperentity);
      ISCRUTE(_nconstituentpervalue);goto ERROR;
    }

  if (_filterparameterexist) {
    if ((*_filter).nconstituentpervalue != _nconstituentpervalue ) {
      MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_VALUE_MSG);
      ISCRUTE((*_filter).nconstituentpervalue); ISCRUTE(_nconstituentpervalue );
      goto ERROR;
    }
    if ((*_filter).nvaluesperentity != _nvalueperentity ) {
      MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_VALUE_MSG);
      ISCRUTE((*_filter).nvaluesperentity); ISCRUTE(_nvalueperentity );
      goto ERROR;
    }
  } else {
    (*_filter).nconstituentpervalue = _nconstituentpervalue;
    (*_filter).nvaluesperentity     = _nvalueperentity;
  }

  /*
   * Ouverture du datagroup de niveau 2 <numdt>.<numit>
   */
  if ( _MEDattrEntierLire(_meshid,MED_NOM_SRT,&_intsortingtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_SRT);ISCRUTE(_intsortingtype);goto ERROR;
  }
  _sortingtype = (med_sorting_type) (_intsortingtype);

  _MEDgetComputationStepName(_sortingtype,numdt,numit,_datagroupname1);
  if ( (_datagroup1 = _MEDdatagroupOuvrir(_meshid,_datagroupname1)) < 0 ) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
    SSCRUTE(_datagroupname1);ISCRUTE_id(_datagroup1);goto ERROR;
  }



  /*
   *  Ouverture du datagroup de niveau 3 <entitytype>
   */
  if (_MEDgetEntityTypeName(_datagroupname2,entitytype) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(entitytype);SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);goto ERROR;
  }

  if ((_datagroup2 = _MEDdatagroupOuvrir(_datagroup1,_datagroupname2)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
    goto ERROR;
  }



  /*
   *  Ouverture du datagroup de niveau 4 [.<geotype>]
   */

  if ( entitytype != MED_NODE ) {

    if (strlen(_geotypename))
      strncpy(_datagroupname3,_geotypename,MED_NAME_SIZE+1);
    else
      if ( _MEDgetInternalGeometryTypeName(fid,_datagroupname3,geotype) < 0) {
	MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
	ISCRUTE_int(geotype);SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
	SSCRUTE(_datagroupname2);goto ERROR;
      }

    if ((_datagroup3 = _MEDdatagroupOuvrir(_datagroup2,_datagroupname3)) < 0) {
	  MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
	  SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
	  SSCRUTE(_datagroupname2);SSCRUTE(_datagroupname3);goto ERROR;
    }
  }


  if (_datagroup3) _datagroup=_datagroup3; else _datagroup=_datagroup2;

  /*
   *  Ouverture du datagroup de niveau 5 [VARATR]
   */
  /*Problème d'activation du profil */
  if ( (meddatatype == MED_VARIABLE_ATTRIBUTE) || (meddatatype == MED_COORDINATE_TRSF) ) {
    /* TODO : Pour simplifier et systématiser la procédure il faut
       créer une routine spécifique qui donne le nom du datagroup  supplémentaire optionnel.
       Si le retour est "" ne pas créer le datagroup. */
    if (meddatatype == MED_VARIABLE_ATTRIBUTE)
      strcpy(_datagroupname4,MED_VARATR_NOM);
    else
      strcpy(_datagroupname4,MED_COOTRF_NOM);

    if ((_datagroup4 = _MEDdatagroupOuvrir(_datagroup,_datagroupname4)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
      SSCRUTE(_datagroupname3);SSCRUTE(_datagroupname4);SSCRUTE(MED_VARATR_NOM);goto ERROR;
    }
  }

  if (_datagroup4) _fdatagroup=_datagroup4; else _fdatagroup=_datagroup;

  /*
   * Attribut PFL (nombre de noeuds ou d'elements)
   */
  if ( _MEDattrStringLire(_datagroup,MED_NOM_PFL,MED_NAME_SIZE,_profilename) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
    SSCRUTE(_datagroupname3); SSCRUTE(MED_NOM_PFL);SSCRUTE(_profilename);goto ERROR;
  }

  /*
   * Coh�rence de l'attribut PFL (nombre de noeuds ou d'elements)
   */
  if ( strlen((*_filter).profilename)  )   /* != MED_NOPFL*/
    _tmpprofilename=(*_filter).profilename;

  if ( strcmp(_tmpprofilename,_profilename) ) {
    MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_PROFILE_MSG);
    SSCRUTE(_tmpprofilename);SSCRUTE((*_filter).profilename);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
    SSCRUTE(_datagroupname3);goto ERROR;
  }

/*INUTILE : le param�tre est positionn� dans  MEDfilterEntityCr */
/*   if (!_filterparameterexist) (*_filter).profilearraysize =  _profilearraysize; */


  /*
   * Attribut NBR (nombre de noeuds ou d'elements)
   */

  if (_MEDdatasetExist(_fdatagroup,_datasetname,&_datasetexist,&_isasoftlink) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDdatasetExist");
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
    SSCRUTE(_datagroupname3);SSCRUTE(_datasetname);SSCRUTE(_profilename);
    goto ERROR;
  }

  /*INHIBITION DU MESSAGE POUR COMPATIBILITE COMPORTEMENT 2.3.6*/
  if ( !_datasetexist) {
    _ret = MED_ERR_DOESNTEXIST;
    goto ERROR;
  }

  if ((_dataset = _MEDdatasetOuvrir(_fdatagroup,_datasetname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATASET,_datasetname);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
    SSCRUTE(_datagroupname2);SSCRUTE(_datagroupname3);
    goto ERROR;
  }

  if ( _MEDattrEntierLire(_dataset,MED_NOM_NBR,&((*_filter).nentity)) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);SSCRUTE(_datagroupname3);
    SSCRUTE(MED_NOM_NBR);ISCRUTE((*_filter).nentity);goto ERROR;
  }


  if (!_filterparameterexist) {

#if _DEBUG_
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
#if _DEBUG_
  ISCRUTE((*_filter).nentity              );
  ISCRUTE((*_filter).nvaluesperentity     );
  ISCRUTE((*_filter).nconstituentpervalue );
  ISCRUTE((*_filter).constituentselect       );
  ISCRUTE_int((*_filter).switchmode              );
  ISCRUTE((*_filter).filterarraysize         );
  ISCRUTE((*_filter).profilearraysize        );
  ISCRUTE_int((*_filter).storagemode             );
  SSCRUTE((*_filter).profilename             );
  ISCRUTE_int(_datatype);
#endif

  /*Il n'y a pas de connectivité à écrire pour les particules */
  if (!( ( meddatatype == MED_CONNECTIVITY   )  &&
	 (!strcmp(_geotypename,"MED_PARTICLE"))
	 ) )
    if ( _MEDdatasetRd(_fdatagroup,_datasetname,_datatype,_filter,value) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,datasetname);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
      SSCRUTE(_datagroupname3);SSCRUTE(_profilename);
      goto ERROR;
    }

  _ret = 0;

 ERROR:

  if (!_filterparameterexist) {
    if ( MEDfilterClose(_filter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);SSCRUTE(_datagroupname3);
    SSCRUTE(_profilename);goto ERROR;
    }
  }

  if (_dataset>0)     if (_MEDdatasetFermer(_dataset) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASET,MED_NOM_COO);
    ISCRUTE_id(_dataset);
  }

  if (_datagroup4>0)     if (_MEDdatagroupFermer(_datagroup4) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_VARATR_NOM);
    ISCRUTE_id(_datagroup4);
  }

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

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }


  va_end(params);
  *fret = _ret;

  return;
}

