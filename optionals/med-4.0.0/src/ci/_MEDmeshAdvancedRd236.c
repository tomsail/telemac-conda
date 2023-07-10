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

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"
#include "2.3.6/med23v30_hdfi.h"


void _MEDmeshAdvancedRd236(int dummy, ...)
{
  med_access_mode       _MED_ACCESS_MODE;
  med_err               _ret=-1;
  med_idt               _meshid=0;
  med_idt               _datagroup=0, _datagroup2=0,_datagroup3=0,_dataset=0;
  char                  _meshpath         [MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]="";
  char                  _datagroupname2   [MED_TAILLE_NOM_ENTITE+1]="";
  char                  _datagroupname3   [MED_TAILLE_NOM_ENTITE+1]="";
  char                  _datasetname      [MED_TAILLE_NOM_ENTITE+1]="";

  char                  _geotypename      [MED_TAILLE_NOM_ENTITE+1]="";
  med_bool              _filterparameterexist=MED_FALSE;
  med_filter *          _filter           = NULL;
  med_filter            _tmpfilter        = MED_FILTER_INIT;
  med_int               _nconstituentpervalue=0,_spacedim=0;
  med_internal_type     _datatype;
  med_int               _nvalueperentity=0;
  med_mesh_type         _meshtype=MED_UNDEF_MESH_TYPE;
  med_int               _intmeshtype=0;
  med_bool              _isasupportmesh=MED_FALSE;
  med_int               _intgridtype=0;
  med_grid_type         _gridtype=MED_UNDEF_GRID_TYPE;


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
  MED_VARGS_DEF(const, char*  , const          , meshname           );
  MED_VARGS_DEF(const, med_data_type         , , meddatatype );
  MED_VARGS_DEF(const, char*  , const          , datasetname );
  MED_VARGS_DEF(const, med_internal_type     , , datatype           );
  MED_VARGS_DEF(const, med_int               , , numdt           );
  MED_VARGS_DEF(const, med_int               , , numit           );
  MED_VARGS_DEF(const, med_entity_type       , , entitytype  );
  MED_VARGS_DEF(const, med_geometry_type     , , geotype           );
  MED_VARGS_DEF(const, med_connectivity_mode , , cmode           );
  MED_VARGS_DEF(const, med_storage_mode      , , storagemode );
  MED_VARGS_DEF(const, char * , const          , profilename );
  MED_VARGS_DEF(const, med_switch_mode       , , switchmode  );
  MED_VARGS_DEF(const, med_int               , , dimselect   );
  MED_VARGS_DEF(const, med_filter * , const    , filter           );
  MED_VARGS_DEF(,unsigned char*, const         , value       );
  MED_VARGS_DEF(, med_err *                   ,, fret        );

  _datatype=datatype;

  if (filter) {
    _filter=(med_filter*)(filter); _filterparameterexist=MED_TRUE;
  }
  else {
    _filter=&_tmpfilter;
/*  Inutilisé en lecture 2.3.x*/
/*     (*_filter).nentity              = nentity; */
    (*_filter).nvaluesperentity     = 1;
/*   (*_filter).nconstituentpervalue = nconstituentpervalue; */
    (*_filter).constituentselect       = dimselect;
    (*_filter).switchmode              = switchmode;
    (*_filter).storagemode             = storagemode;
    strcpy((*_filter).profilename,profilename);
    (*_filter).profilearraysize        = 0;
  }

  if ( (*_filter).storagemode == MED_UNDEF_PFLMODE)
    (*_filter).storagemode              = MED_GLOBAL_PFLMODE;

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


  /* Les séquences de calcul dans les maillages ne sont gérées qu'à partir de la 3.0*/
  if ( (numdt != MED_NO_DT) || (numit != MED_NO_IT) ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_COMPUTINGSTEP,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numdt);ISCRUTE(numit);
    goto ERROR;
  }

  if ((_meshid=_MEDmeshDatagroupOpen(fid,meshname,_meshpath,&_isasupportmesh)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(_meshpath); goto ERROR;
  }

  /* Lecture de l'attribut MED_NOM_ESP  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_ESP,&_spacedim) < 0) {
    if (_MEDattrEntierLire(_meshid,MED_NOM_DIM,&_spacedim) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(MED_NOM_DIM);goto ERROR;
    }
  }

  /* Lecture de l'attribut MED_NOM_TYP  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_TYP,&_intmeshtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_TYP);ISCRUTE(_intmeshtype);goto ERROR;
  }
  _meshtype = (med_mesh_type) _intmeshtype;

  if ( _meshtype == MED_STRUCTURED_MESH ) {
 /*
   * Lecture du type de grille (attribut MED_NOM_GTY)
   */
    if (_MEDattrEntierLire(_meshid,MED_NOM_GTY,&_intgridtype) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(MED_NOM_GTY);ISCRUTE(_intgridtype);goto ERROR;
    }
    _gridtype=(med_grid_type) _intgridtype;
  }


  if ( (entitytype == MED_STRUCT_ELEMENT) || (meddatatype == MED_VARIABLE_ATTRIBUTE) ) {
    MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(entitytype);ISCRUTE_int(meddatatype);goto ERROR;
  }


  /* Si un nom de dataset est précisé, il n'est pas géré en 2.3.6 
     Detection automatique en
     fonction des paramètres meddatatype et cmode */
  if ( !strlen(datasetname) ) {
    if ( _MEDgetDatasetName( _datasetname, meddatatype, cmode ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetDatasetname");
      ISCRUTE_int(meddatatype);ISCRUTE_int(cmode);SSCRUTE( _datasetname);goto ERROR;
    }
  } else {
    MED_ERR_(_ret,MED_ERR_NULL,MED_ERR_DATASET,datasetname);
    goto ERROR;
  }

  /* Si un type de dataset est précisé, il est prioritaire sur la détection automatique en
     fonction des paramètres meddatatype et cmode */
  /* TODO : renommer datatype en idatatype (internal) */
/*   if ( _datatype == MED_INTERNAL_UNDEF) */
/*     if ( _MEDgetDatatype( &_datatype, meddatatype, cmode ) < 0 ) { */
/*       MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetDatatype"); */
/*       ISCRUTE(meddatatype);ISCRUTE(cmode);ISCRUTE(_datatype);goto ERROR; */
/*     } */

  /* Vérification :
     Le type de données MED traitées sont-elles compatibles avec le type de maillage ?
  */
  /* REM : Si datasetname ou datatype ne sont pas précisés, meddatatype doit l'être */
/*   if ( meddatatype != MED_UNDEF_DATATYPE ) */
/*     if ( _MEDmeshtypeCompatibility(meddatatype, _intmeshtype) < 0) { */
/*       MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_MEDDATATYPE,MED_ERR_VALUE_MSG); */
/*       ISCRUTE_int(meddatatype);ISCRUTE_int(_intmeshtype);goto ERROR; */
/*     } */

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
   *  Ouverture du datagroup de niveau 3 <entitytype>
   */
  if (_MEDgetEntityTypeName(_datagroupname2,entitytype) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(entitytype);SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);goto ERROR;
  }

  if ((_datagroup2 = _MEDdatagroupOuvrir(_meshid,_datagroupname2)) < 0) {
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
	ISCRUTE_int(geotype);SSCRUTE(meshname);SSCRUTE(_datagroupname2);
	goto ERROR;
      }

    if ((_datagroup3 = _MEDdatagroupOuvrir(_datagroup2,_datagroupname3)) < 0) {
	  MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
	  SSCRUTE(meshname);SSCRUTE(_datagroupname2);SSCRUTE(_datagroupname3);
	  goto ERROR;
    }
  }


  if (_datagroup3) _datagroup=_datagroup3; else _datagroup=_datagroup2;


  /*
   * Cohérence de l'attribut PFL (nombre de noeuds ou d'elements)
   */
  if ( strlen((*_filter).profilename)  ) {  /* != MED_NOPFL*/
    MED_ERR_(_ret,MED_ERR_NULL,MED_ERR_PROFILE,(*_filter).profilename);
    SSCRUTE(meshname);SSCRUTE(_datagroupname2);
    SSCRUTE(_datagroupname3);goto ERROR;
  }

  if ((*_filter).storagemode != MED_GLOBAL_PFLMODE ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_FILTER,"");
    ISCRUTE_int(geotype);SSCRUTE(meshname);SSCRUTE(_datagroupname2);
    ISCRUTE_int((*_filter).storagemode);goto ERROR;
  }

  switch(meddatatype)
    {

    case MED_COORDINATE_AXIS1 :
      strcpy(_datasetname,MED_NOM_IN1);
      if (_gridtype == MED_CURVILINEAR_GRID) goto CURVILINEAR_LABEL; else goto COORDINATE_LABEL;

    case MED_COORDINATE_AXIS2 :
      strcpy(_datasetname,MED_NOM_IN2);
      if (_gridtype == MED_CURVILINEAR_GRID) goto CURVILINEAR_LABEL; else goto COORDINATE_LABEL;

    case MED_COORDINATE_AXIS3 :
      strcpy(_datasetname,MED_NOM_IN3);
      if (_gridtype == MED_CURVILINEAR_GRID) goto CURVILINEAR_LABEL; else goto COORDINATE_LABEL;

    CURVILINEAR_LABEL:
      if ((_dataset = _MEDdatasetOuvrir(_datagroup,MED_NOM_COO)) < 0) {
	MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATASET,MED_NOM_COO);
	goto ERROR;
      }

      if (_MEDattrEntierLire(_dataset,_datasetname, (unsigned char*) value ) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_datasetname);
	goto ERROR;
      }
      break;


    case MED_COORDINATE :
    COORDINATE_LABEL:
      if (_MEDdatasetNumLire(_datagroup,_datasetname,MED_FLOAT64,
				    (med_mode_switch) (*_filter).switchmode ,
				    (med_size)        (*_filter).nconstituentpervalue,
				    (med_size)        (*_filter).constituentselect,
				    (med_size)        (*_filter).filterarraysize,
				    MED_COMPACT,MED_PFL_NON_COMPACT,
				    (*_filter).filterarray23v30,
				    MED_NOPG, 0, (unsigned char*) value ) < 0)
	goto ERROR;
      break;


    case MED_INDEX_FACE :
    case MED_INDEX_NODE :
      switch(cmode)
	{
	case MED_NODAL :
	  switch(meddatatype)
	    {
	    case MED_CONNECTIVITY :
	      strcpy(_datasetname,MED_NOM_NOD);
	      break;
	    case MED_INDEX_FACE :
	      if (geotype == MED_POLYHEDRON)
		strcpy(_datasetname,MED_NOM_INN);
	      else
		strcpy(_datasetname,MED_NOM_IFN);
	      break;
	    case MED_INDEX_NODE :
	      if (geotype == MED_POLYHEDRON)
		strcpy(_datasetname,MED_NOM_IFN);
	      else
		strcpy(_datasetname,MED_NOM_INN);
	      break;
	    }
	  break;

	case MED_DESCENDING :
	  switch(meddatatype)
	    {
	    case MED_CONNECTIVITY :
	      strcpy(_datasetname,MED_NOM_DES);
	      break;
	    case MED_INDEX_FACE :
	      if (geotype == MED_POLYHEDRON)
		strcpy(_datasetname,MED_NOM_IND);
	      else
		strcpy(_datasetname,MED_NOM_IFD);
	      break;
	    case MED_INDEX_NODE :
	      if (geotype == MED_POLYHEDRON)
		strcpy(_datasetname,MED_NOM_IFD);
	      else
		strcpy(_datasetname,MED_NOM_IND);
	      break;
	    }
	  break;

	default :
	  MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_CONNECTIVITYMODE,MED_ERR_VALUE_MSG);
	  ISCRUTE_int(cmode);goto ERROR;
	}

    case MED_CONNECTIVITY :
#if defined(HAVE_F77INT64)
      if ( _MEDdatasetNumLire(_datagroup,_datasetname,MED_INT64,
			      (med_mode_switch) (*_filter).switchmode ,
			      (med_size) (*_filter).nconstituentpervalue,
			      (med_size) (*_filter).constituentselect,
			      (med_size) (*_filter).filterarraysize,MED_COMPACT,MED_PFL_NON_COMPACT,
			      (*_filter).filterarray23v30,
			      MED_NOPG, 0, (unsigned char*) value ) < 0)
	goto ERROR;
#else
      if ( _MEDdatasetNumLire(_datagroup,_datasetname,MED_INT32,
			      (med_mode_switch) (*_filter).switchmode ,
			      (med_size) (*_filter).nconstituentpervalue,
			      (med_size) (*_filter).constituentselect,
			      (med_size) (*_filter).filterarraysize,MED_COMPACT,MED_PFL_NON_COMPACT,
			      (*_filter).filterarray23v30,
			      MED_NOPG, 0, (unsigned char*) value ) < 0)
	goto ERROR;
#endif

      break;

    case MED_NAME :
      if (_MEDdatasetStringLire(_datagroup,_datasetname,(char *) value) < 0)
	goto ERROR;
      break;

    case MED_NUMBER :
    case MED_GLOBAL_NUMBER :
    case MED_FAMILY_NUMBER :
#if defined(HAVE_F77INT64)
      if (_MEDdatasetNumLire(_datagroup,_datasetname,MED_INT64,
			     MED_NO_INTERLACE,
			     (med_size)1,
			     (med_size) (*_filter).constituentselect,
			     (med_size) MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,
			     (med_size *) NULL, MED_NOPG,0,
			     (unsigned char*) value) < 0)
	goto ERROR;
#else
      if (_MEDdatasetNumLire(_datagroup,_datasetname,MED_INT32,
			     MED_NO_INTERLACE,
			     (med_size) 1,
			     (med_size) (*_filter).constituentselect,
			     (med_size) MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,
			     (med_size *) NULL, MED_NOPG,0,
			     (unsigned char*) value) < 0)
	goto ERROR;
#endif
      break;


    case MED_COORDINATE_TRSF :
    default :
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_MEDDATATYPE,MED_ERR_VALUE_MSG);
      ISCRUTE_int(meddatatype);goto ERROR;
    }


  _ret = 0;

 ERROR:

/*   if (!_filterparameterexist) { */
/*     if ( MEDfilterClose(_filter) < 0 ) { */
/*       MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_MESH); */
/*     SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);SSCRUTE(_datagroupname3); */
/*     SSCRUTE(_profilename);goto ERROR; */
/*     } */
/*   } */

/*   if (_dataset>0)     if (_MEDdatasetFermer(_dataset) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASET,MED_NOM_COO); */
/*     ISCRUTE_id(_dataset); */
/*   } */

/*   if (_datagroup4>0)     if (_MEDdatagroupFermer(_datagroup4) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_VARATR_NOM); */
/*     ISCRUTE_id(_datagroup4); */
/*   } */

  if (_dataset>0)     if (_MEDdatasetFermer(_dataset) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASET,MED_NOM_COO);
    ISCRUTE_id(_dataset);
  }

  if (_datagroup3>0)     if (_MEDdatagroupFermer(_datagroup3) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname3);
    ISCRUTE_id(_datagroup3);
  }

  if (_datagroup2>0)     if (_MEDdatagroupFermer(_datagroup2) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname2);
    ISCRUTE_id(_datagroup2);
  }

/*   if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) { */
/*     MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname1); */
/*     ISCRUTE_id(_datagroup1); */
/*   } */

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }


  va_end(params);
  *fret = _ret;

  return;
}

