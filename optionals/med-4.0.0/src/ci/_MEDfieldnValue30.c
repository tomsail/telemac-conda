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
#include <string.h>
#include <stdlib.h>
#include <med_outils.h>


void
_MEDfieldnValue30(int dummy, ...)
{


  med_int  _ret=-1;
  med_idt  _gid=0,_locgid=0,_datagroup1=0,_datagroup2=0;
  char     _path[(MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1)+(2*MED_MAX_PARA+1)+1]=MED_FIELD_GRP;
  char     _datagroupname1[2*MED_TAILLE_NOM_ENTITE+2]="";
  char     _datagroupname2[MED_NAME_SIZE+1]="";
  char     _tmpprofilename[MED_NAME_SIZE+1]="", *_profilename=0;
  char     _geotypename      [MED_TAILLE_NOM_ENTITE+1]="";
  char     _locgidname        [MED_LOCALIZATION_GRP_SIZE+MED_NAME_SIZE+1]="";
  char     _sectionmeshname      [MED_NAME_SIZE+1]="";
  char     _localizationname  [MED_NAME_SIZE+1]=""; /*TODO DEFAULT? */
  med_int  _nconstituentpervalue=0,_nvaluesperentityfromloc=0;
  med_int  _numdt=0, _numit=0;
  med_int  _profilearraysize=0,_nvaluesperentity=0;
  med_int  _nsectioncell=1;
  med_size _n=0;
  med_int  _intn=0;
  med_geometry_type    _locgeotype   =0,_sectiongeotype=0;
  med_int              _intlocgeotype=0;
  int      _num;
  med_bool _anyprofile=MED_FALSE;
  med_bool _chgt=MED_FALSE,_trsf=MED_FALSE;


  MED_VARGS_DECL(const, med_idt           , , fid                        );
  MED_VARGS_DECL(const, char * , const      , fieldname                  );
  MED_VARGS_DECL(const, med_int           , , numdt                      );
  MED_VARGS_DECL(const, med_int           , , numit                      );
  MED_VARGS_DECL(const, med_entity_type   , , entitytype                 );
  MED_VARGS_DECL(const, med_geometry_type , , geotype                    );
  MED_VARGS_DECL(, char *, const      , profilename                );
  MED_VARGS_DECL(const, int               , , profileit                  );
  MED_VARGS_DECL(const, med_storage_mode  , , storagemode                );
  MED_VARGS_DECL(, med_int *, const   , profilesize                );
  MED_VARGS_DECL(, char *, const      , localizationname           );
  MED_VARGS_DECL(, med_int *, const   , nintegrationpoint       );
  MED_VARGS_DECL(, med_int *         ,, fret                       );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid                        );
  MED_VARGS_DEF(const, char * , const      , fieldname                  );
  MED_VARGS_DEF(const, med_int           , , numdt                      );
  MED_VARGS_DEF(const, med_int           , , numit                      );
  MED_VARGS_DEF(const, med_entity_type   , , entitytype                 );
  MED_VARGS_DEF(const, med_geometry_type , , geotype                    );
  MED_VARGS_DEF(, char *, const      , profilename                );
  MED_VARGS_DEF(const, int               , , profileit                  );
  MED_VARGS_DEF(const, med_storage_mode  , , storagemode                );
  MED_VARGS_DEF(, med_int *, const   , profilesize                );
  MED_VARGS_DEF(, char *, const      , localizationname           );
  MED_VARGS_DEF(, med_int *, const   , nintegrationpoint       );
  MED_VARGS_DEF(, med_int *         ,, fret                       );

  _num = profileit -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  NOFINALBLANK(fieldname,ERROR);

  /*
   * On construit le chemin d'acc�s jusqu'aux groupes <type_ent>[.<type_geo>]
   */
  strcat(_path,fieldname);
  strcat(_path,"/");
  _MEDgetComputationStepName(MED_SORT_DTIT,numdt,numit,&_path[strlen(_path)]);
  strcat(_path,"/");

  if ((_gid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_FIELD_MSG);
    SSCRUTE(_path); goto ERROR;
  }


  /*Lit l'attribut MED_NOM_NDT  */
  if ( _MEDattrEntierLire(_gid,MED_NOM_NDT, &_numdt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_NDT);
    ISCRUTE(_numdt);goto ERROR;
  }

  if ( _numdt != numdt ) {
    MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(_path);ISCRUTE(_numdt);ISCRUTE(numdt);goto ERROR;
  }

  /*Lit l'attribut MED_NOM_NOR */
  if ( _MEDattrEntierLire(_gid,MED_NOM_NOR, &_numit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(MED_NOM_NOR);
    ISCRUTE(_numit); goto ERROR;
  }

  if ( _numit != numit) {
    MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);ISCRUTE(_numit);ISCRUTE(numit);goto ERROR;
  }

  /*
   * Si le Data Group  de niveau <type_ent>[.<type_geo>] n'existe pas retroune 0
   */

  if (_MEDgetEntityTypeName(_datagroupname1,entitytype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ENTITY,_datagroupname1);
    goto ERROR;
  }

  if (entitytype != MED_NODE) {
    if ( entitytype == MED_STRUCT_ELEMENT ) {
      if ( MEDstructElementName(fid, geotype,_geotypename) < 0 ) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDstructElementName");
	ISCRUTE_int(geotype);goto ERROR;
      }
    } else {
      if (_MEDgetInternalGeometryTypeName(0,_geotypename,geotype) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_GEOMETRIC,_geotypename);
	goto ERROR;
      }
    }
    strcat(_datagroupname1,".");
    strcat(_datagroupname1,_geotypename);
  }
  /*   strcat(_path,_datagroupname1); */
  /*   strcat(_path,"/"); */


  if ((_datagroup1 = _MEDdatagroupOuvrir(_gid,_datagroupname1)) < 0) {
    _n=0;
  } else {
    if (strlen(profilename) && (profileit >=0))  {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(_datagroupname1);
      SSCRUTE(profilename); ISCRUTE_int(profileit); goto ERROR;
    }

    if ( profileit <0 ) {
      /* PATCH : UsesCase_MEDfield2.c ! */
      if ( !strlen(profilename)) {
	_profilename = MED_NO_PROFILE_INTERNAL;
      }
      else {
	_profilename=profilename;
	_anyprofile=MED_TRUE;
      }
    } else {
      _profilename=_tmpprofilename;
      if ( _MEDobjectGetName(_datagroup1, "." ,_num, _profilename) < 0 ) {
	MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);
	SSCRUTE(_datagroupname1);ISCRUTE_int(profileit);
	goto ERROR;
      }
      if ( strcmp(_profilename,MED_NO_PROFILE_INTERNAL) ) {
	strncpy(profilename,_profilename,MED_NAME_SIZE+1);
	profilename[MED_NAME_SIZE]='\0';
	_anyprofile=MED_TRUE;
      } else {
	profilename[0]='\0';
      }
    }
    
    if ((_datagroup2 = _MEDdatagroupOuvrir(_datagroup1,_profilename)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(_datagroupname1); SSCRUTE(_profilename);goto ERROR;
      goto ERROR;
    }
    
    
    if ( _MEDattrEntierLire(_datagroup2,MED_NOM_NBR,&_intn) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(_datagroupname1); SSCRUTE(_profilename);
      SSCRUTE(MED_NOM_NBR);ISCRUTE_size(_n);goto ERROR;
    }
    _n = _intn;

    if ( _anyprofile ) {
      if ( (_profilearraysize=MEDprofileSizeByName(fid, _profilename)) < 0)  {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_FIELD_MSG);
	SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(_datagroupname1); SSCRUTE(_profilename);
	SSCRUTE("MEDprofileSizeByName");goto ERROR;
      }
    } else {
      _profilearraysize = (med_int) _n;
    }
    
    *profilesize=_profilearraysize;

    switch(storagemode) {
      
    case MED_GLOBAL_STMODE :
      
      break;
      
    case MED_COMPACT_STMODE :
      
      _n=_profilearraysize;
      
      break;
      
    default :
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_STORAGEMODE,MED_ERR_VALUE_MSG);
      ISCRUTE_int(storagemode);goto ERROR;
      break;
      
    }

    /* Lecture du nom de la localization  */
    if ( _MEDattrStringLire(_datagroup2,MED_NOM_GAU,MED_NAME_SIZE,_localizationname) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(_datagroupname1);SSCRUTE(_profilename);
      SSCRUTE(MED_NOM_GAU);SSCRUTE(_localizationname);goto ERROR;
    }
    strncpy(localizationname,_localizationname,MED_NAME_SIZE+1);
    localizationname[MED_NAME_SIZE]='\0';

    /* Lire le nbre de valeurs pas entit�*/
    if ( _MEDattrEntierLire(_datagroup2,MED_NOM_NGA,&_nvaluesperentity) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(_datagroupname1);
      SSCRUTE(_profilename); SSCRUTE(MED_NOM_NGA);ISCRUTE(_nvaluesperentity);goto ERROR;
    }


    /* V�rification de la coh�rence du  nombre de valeurs pas entit�*/
    if (entitytype == MED_NODE_ELEMENT ) {
      if ( strlen( _localizationname) ) {
	MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_PARAMETER,_localizationname);
	SSCRUTE(MED_NO_LOCALIZATION);ISCRUTE_int(entitytype);goto ERROR;
      }
      _nvaluesperentityfromloc = geotype % 100;
    } else if (! strcmp(_localizationname,MED_GAUSS_ELNO)) {
      /* Les points de Gauss sont d�finis sur les noeuds de l'element (mot cle) */
      /* le nombre de points de Gauss est egal au nombre de noeuds de l'element */
      _nvaluesperentityfromloc = geotype % 100;
    } else if ( strlen( _localizationname) ) {
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
	SSCRUTE(_localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE(_intlocgeotype);goto ERROR;
      }
      _locgeotype = (med_geometry_type) _intlocgeotype;
      
      if ( _locgeotype != geotype ) {
	MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
	SSCRUTE(_localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE_int(_locgeotype);ISCRUTE_int(geotype);
	goto ERROR;
      }
      
    } else {
      _nvaluesperentityfromloc = 1;
    }
    
    if ( _nvaluesperentityfromloc != _nvaluesperentity ) {
      MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
      SSCRUTE(_localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE(_nvaluesperentityfromloc);
      ISCRUTE(_nvaluesperentity);goto ERROR;
    }
    
    *nintegrationpoint = _nvaluesperentity;

  }

  _ret = _n;
 ERROR:

  if (_datagroup2>0)            if (_MEDdatagroupFermer(_datagroup2) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    SSCRUTE(_datagroupname1); SSCRUTE(_profilename); ISCRUTE_id(_datagroup2);
  }

  if (_datagroup1>0)            if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    SSCRUTE(_datagroupname1); ISCRUTE_id(_datagroup1);
  }

  if (_gid>0)            if (_MEDdatagroupFermer(_gid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
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
