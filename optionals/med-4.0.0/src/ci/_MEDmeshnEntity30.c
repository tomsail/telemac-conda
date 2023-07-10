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
_MEDmeshnEntity30(int dummy, ...)
{


  med_access_mode       _MED_ACCESS_MODE;
  med_int               _ret=-1,_err=-1;
  med_idt               _meshid=0, _datagroup=0,_datagroupf=0,_datagroup1=0;
  med_idt               _datagroup2=0,_datagroup3=0,_datagroup4=0,_dataset=0;
  char                  _meshpath         [MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]="";
  char                  _datagroupname1   [2*MED_MAX_PARA+1]       ="";
  char                  _datagroupname2   [MED_TAILLE_NOM_ENTITE+1]="";
  char                  _datagroupname3   [MED_TAILLE_NOM_ENTITE+1]="";
  char                  _datagroupname4   [MAX(MED_TAILLE_VARATR,MED_TAILLE_COOTRF)]="";
  char                  _datasetconame    [3 + 1 + 3 + 1 ]         ="";
  char                  _profilename      [MED_NAME_SIZE+1]        ="";
  char                  _geotypename      [MED_TAILLE_NOM_ENTITE+1]="";
  char                  _datasetname      [MED_TAILLE_NOM_ENTITE+1]="";
  med_sorting_type      _sortingtype       = 0;
  med_int               _intsortingtype    = 0;
  med_int               _profilearraysize  = 0;
  med_int               _changement=0, _changement_s=0, _transformation=0;
  med_bool              _changement_co     = MED_FALSE;
  med_size              _n=0;
  med_int               _intn=0;
  med_bool              _isasupportmesh    = MED_FALSE;
  med_bool              _isasoftlink       = MED_FALSE;
  med_bool              _datasetexist      = MED_FALSE;
  med_int               _ntmpmeddatatype   = 1;
  med_data_type         _tmpmeddatatype[4] = {MED_UNDEF_DATATYPE,MED_UNDEF_DATATYPE,MED_UNDEF_DATATYPE,MED_UNDEF_DATATYPE};
  med_grid_type         _gridtype          = MED_UNDEF_GRID_TYPE;
  med_int               _intgridtype       = 0;
  med_int               _intmeshtype       = 0;
  med_int               _meshdim           = 0;
  int                   _i                 = 0;
  med_connectivity_mode _cmode                ;


  MED_VARGS_DECL(const, med_idt                , , fid           );
  MED_VARGS_DECL(const, char * , const           , meshname      );
  MED_VARGS_DECL(const, med_int                , , numdt         );
  MED_VARGS_DECL(const, med_int                , , numit         );
  MED_VARGS_DECL(const, med_entity_type        , , entitytype    );
  MED_VARGS_DECL(const, med_geometry_type      , , geotype       );
  MED_VARGS_DECL(const, med_data_type          , , meddatatype   );
  MED_VARGS_DECL(const, med_connectivity_mode  , , cmode         );
  MED_VARGS_DECL(const, med_storage_mode       , , storagemode   );
  MED_VARGS_DECL(, char     *, const       , profilename         );
  MED_VARGS_DECL(, med_int  *, const       , profilesize         );
  MED_VARGS_DECL(, med_bool *, const       , changement          );
  MED_VARGS_DECL(, med_bool *, const       , transformation      );
  MED_VARGS_DECL(, med_int  *             ,, fret                );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt                , , fid           );
  MED_VARGS_DEF(const, char * , const           , meshname      );
  MED_VARGS_DEF(const, med_int                , , numdt         );
  MED_VARGS_DEF(const, med_int                , , numit         );
  MED_VARGS_DEF(const, med_entity_type        , , entitytype    );
  MED_VARGS_DEF(const, med_geometry_type      , , geotype       );
  MED_VARGS_DEF(const, med_data_type          , , meddatatype   );
  MED_VARGS_DEF(const, med_connectivity_mode  , , cmode         );
  MED_VARGS_DEF(const, med_storage_mode       , , storagemode   );
  MED_VARGS_DEF(, char     *, const       , profilename         );
  MED_VARGS_DEF(, med_int  *, const       , profilesize         );
  MED_VARGS_DEF(, med_bool *, const       , changement          );
  MED_VARGS_DEF(, med_bool *, const       , transformation      );
  MED_VARGS_DEF(, med_int  *             ,, fret                );

  _cmode = cmode;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  if ( (_MED_ACCESS_MODE = _MEDmodeAcces(fid) ) == MED_ACC_UNDEF ) {
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_ACCESSMODE,MED_ERR_FILE_MSG);
    goto ERROR;
  }

  /*
   * Ouverture du datagroup de niveau 1 <_meshpath>/<meshname>
   */
  NOFINALBLANK(meshname,ERROR);

  if ((_meshid=_MEDmeshDatagroupOpen(fid,meshname,_meshpath,&_isasupportmesh)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
    SSCRUTE(meshname); goto ERROR;
  }

  /* Lecture de la dimension du maillage  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_DIM,&_meshdim) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_DIM);ISCRUTE(_meshdim);goto ERROR;
  }

  /* Lecture du type de maillage (attribut MED_NOM_TYP)  */
  if (_MEDattrEntierLire(_meshid,MED_NOM_TYP,&_intmeshtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_TYP);ISCRUTE(_intmeshtype);goto ERROR;
  }

  if ( ( (med_mesh_type) _intmeshtype ) != MED_UNSTRUCTURED_MESH ) {

    /* Lecture de l'attribut MED_NOM_GTY  */
    if (_MEDattrEntierLire(_meshid,MED_NOM_GTY,&_intgridtype) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(MED_NOM_GTY);ISCRUTE(_intgridtype);goto ERROR;
    }
    _gridtype= (med_grid_type) _intgridtype;
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
   * Attribut CGT (un changement a eu lieu depuis l'étape de calcul précédente)
   */
  if ( _MEDattrEntierLire(_datagroup1,MED_NOM_CGT,&_changement) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
    SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_CGT);goto ERROR;
  }

  *changement     = (med_bool) _changement;
  *transformation = MED_FALSE;
  /*
   * Gestion entitytype == MED_UNDEF_ENTITY_TYPE
   */
  if ( entitytype == MED_UNDEF_ENTITY_TYPE ) {
    _n=0; goto SORTIE;
  }

  /*
   * Gestion entitytype == MED_ALL_ENTITY_TYPE
   */
  if ( entitytype == MED_ALL_ENTITY_TYPE ) {
    _err=_MEDnObjects(_datagroup1,".",&_n);
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_datagroupname1);
      goto ERROR;
    }
    goto SORTIE;
  }


  /*
   *  Ouverture du datagroup de niveau 3 <entitytype>
   */
  if (_MEDgetEntityTypeName(_datagroupname2,entitytype) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(entitytype);SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);goto ERROR;
  }

  if ((_datagroup2 = _MEDdatagroupOuvrir(_datagroup1,_datagroupname2)) < 0) {
    *changement     = (med_bool) MED_FALSE;
    _n=0;_datagroup2=0;
    goto SORTIE;
  }

  /*  MAJ du changement pour le type d'entité <entitytype>
   *   ( annulant eventuellement le changement global précédent concernant tous les types d'entités)
   *  Attribut CGT (un changement a eu lieu depuis l'étape de calcul précédente)
   */
  if ( _MEDattrEntierLire(_datagroup2,MED_NOM_CGT,&_changement) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
    SSCRUTE(_datagroupname2);SSCRUTE(MED_NOM_CGT);
    goto ERROR;
  }
  *changement = (med_bool) _changement;

  /*
   *  Ouverture du datagroup de niveau 4 <geotype>
   */

  /* Pour <entitype>==MED_NODE && geotype == MED_GEO_ALL, on renvoie 1 s'il existe au moins un dataset car
     par construction du modèle via l'API le tableau de coordonnées preexiste forcément
     aux autres tableaux.
   */
  if ( geotype == MED_GEO_ALL ) {

    _err=_MEDnObjects(_datagroup2,".",&_n);
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_datagroupname2);
      goto ERROR;
    }
    /*Par construction du modèle, le tableau de coordonnées preexiste aux autres */
    if ( ( entitytype == MED_NODE ) && (_n > 0) ) _n=1;
    goto SORTIE;
  }

  if ( entitytype != MED_NODE ) {

    /* Lecture du nom de type géométrique */
    /*TODO : Remplacer les deux appels suivants par un seul gérant les geotype dynamiques et statiques*/
    if ( entitytype == MED_STRUCT_ELEMENT ) {
      if ( MEDstructElementName(fid, geotype,_datagroupname3) < 0 ) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDstructElementName");
	ISCRUTE_int(geotype);goto ERROR;
      }
    } else
      if ( _MEDgetInternalGeometryTypeName(0,_datagroupname3,geotype) < 0) {
	MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
	ISCRUTE_int(geotype);SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
	SSCRUTE(_datagroupname2);goto ERROR;
      }

    if ((_datagroup3 = _MEDdatagroupOuvrir(_datagroup2,_datagroupname3)) < 0) {
      _n=0;
      *changement = MED_FALSE;
      goto SORTIE;
    }
  }

  if (_datagroup3) _datagroup=_datagroup3; else _datagroup=_datagroup2;

  /*  MAJ du changement pour le type géométrique d'entité <geotype>
   *   ( annulant eventuellement le changement global précédent concernant tous les types d'entités)
   *  Attribut CGT (un changement a eu lieu depuis l'étape de calcul précédente)
   * Prend en charge le cas d'un dataset vide écrit pour un typegeo donné (CHGT==1)
   */
  if (_datagroup3) {
    if ( _MEDattrEntierLire(_datagroup3,MED_NOM_CGT,&_changement) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
      SSCRUTE(_datagroupname3);SSCRUTE(MED_NOM_CGT);
      goto ERROR;
    }
    *changement = (med_bool) _changement;
  }

  /*
   * Lecture du flag indiquant une modification sur autre chose que les
   datasets : MED_CONNECTIVITY,MED_COORDINATE,MED_COORDINATE_AXIS<i>
   *
   */
  if ( _MEDattrEntierLire(_datagroup,MED_NOM_CGS,&_changement_s) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
    SSCRUTE(_datagroupname3);SSCRUTE(MED_NOM_CGS);goto ERROR;
  }

  /* 1) Lorsque l'utilisateur interroge un <meddatatype> qui est relatif a des coordonnées (famille,....) il faut mettre
   * le flag chgt à jour par rapport à des modifications potentiellement effectuées sur ces coordonnées
   * <=>
   * Si on interroge autre chose que MED_CONNECTIVITY,MED_COORDINATE,MED_COORDINATE_AXIS<i>
   * et qu'un changement est présent sur MED_CONNECTIVITY,MED_COORDINATE,MED_COORDINATE_AXIS<i>
   * le flag chgt doit être positionné à vrai
   * Une demande entitype==MED_NODE && (meddatatype == MED_COORDINATE_AXIS1)||(meddatatype == MED_COORDINATE_AXIS2)
   *    ||(meddatatype === MED_COORDINATE_AXIS3) est assimilée à une demande concernant des coordonnées
   * <=>
   * Y-a-t'il une modification des datasets COO(si NOE)| NOD|DES(si !NOE)
   */
  /* 2)
   * Positionne un mode de connectivité _cmode si le meddatatype demandé
   * est autre chose que des coordonnées ou des connectivités et que le cmode n'a pas été
   * spécifié par l'utilisateur.
   * Cette Information est necessaire pour construire le nom du dataset.
   */
  if (    (meddatatype != MED_CONNECTIVITY) && ( meddatatype != MED_COORDINATE )
       && (meddatatype != MED_COORDINATE_AXIS1)
       && (meddatatype != MED_COORDINATE_AXIS2)
       && (meddatatype != MED_COORDINATE_AXIS3)
       && (meddatatype != MED_INDEX_FACE)
       && (meddatatype != MED_INDEX_NODE)) {

    if (entitytype == MED_NODE) {
      if ( ( (med_mesh_type) _intmeshtype ) != MED_UNSTRUCTURED_MESH ) {
	/* - Quelque soit le type de grille, les tableaux MED_COORDINATE_AXISx sont utilisés.
	 * - Les grilles MED_POLAR_GRID et MED_CARTESIAN_GRID les utilisent pour enregistrer les coordonnées  
	 *   de référence sur les différents axes du système de coordonnées choisi.
	 * - Les grilles curvilinéaires utilisent MED_COORDINATE_AXISx pour stocker la structure
	 *   et le tableau MED_COORDINATE pour stocker les coordonnées des noeuds. 
	 */
	  _ntmpmeddatatype   = _meshdim;
	  _tmpmeddatatype[0] = MED_COORDINATE_AXIS1;
	  _tmpmeddatatype[1] = MED_COORDINATE_AXIS2;
	  _tmpmeddatatype[2] = MED_COORDINATE_AXIS3;
	  if (_gridtype == MED_CURVILINEAR_GRID ) {
	    ++_ntmpmeddatatype;
	    _tmpmeddatatype[_meshdim] = MED_COORDINATE;
	  }
      } else
	_tmpmeddatatype[0] = MED_COORDINATE;
    } else {
      switch (geotype) {
      case MED_POLYHEDRON:
	_ntmpmeddatatype=3;
	_tmpmeddatatype[2] = MED_CONNECTIVITY;
	_tmpmeddatatype[1] = MED_INDEX_FACE;
	_tmpmeddatatype[0] = MED_INDEX_NODE;
	break;
      case MED_POLYGON:
	_ntmpmeddatatype=2;
	_tmpmeddatatype[1] = MED_CONNECTIVITY;
	_tmpmeddatatype[0] = MED_INDEX_NODE;
	break;
      default:
	if ( ( (med_mesh_type) _intmeshtype ) == MED_STRUCTURED_MESH ) {
	  _ntmpmeddatatype=0;
	  break;
	}
	_tmpmeddatatype[0] = MED_CONNECTIVITY;
	if ( cmode == MED_NO_CMODE ) {
	  if ( _MEDdatasetExistByMedtype(_datagroup,MED_CONNECTIVITY,MED_NODAL,
				&_datasetexist, &_isasoftlink) < 0) {
	    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDdatasetExistByMedtype");
	    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
	    SSCRUTE(_datagroupname3);goto ERROR;
	  }

	  if ( _datasetexist ) _cmode= MED_NODAL;
	  else
	    if ( _MEDdatasetExistByMedtype(_datagroup,MED_CONNECTIVITY,MED_DESCENDING,
				  &_datasetexist, &_isasoftlink)  < 0) {
	      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDdatasetExistByMedtype");
	      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
	      SSCRUTE(_datagroupname3);goto ERROR;
	    } else
	      _cmode = MED_DESCENDING;
	}
      }
    }

    *changement = MED_FALSE;
    for (_i=0; _i < _ntmpmeddatatype ;++_i) {
      if ( _MEDgetDatasetChgt( _datagroup, _tmpmeddatatype[_i], _cmode,
			       &_isasoftlink, &_changement_co ) < 0) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"getDatasetChgt");
	SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
	SSCRUTE(_datagroupname3);goto ERROR;
      }
      *changement |= _changement_co;
    }
  }


  /*
   * Niveau de datagroup 5 supplémentaire pour les éléments de structure ou les transformations.
   */
  if (  (meddatatype==MED_VARIABLE_ATTRIBUTE) || (meddatatype==MED_COORDINATE_TRSF ) ) {
    if (meddatatype == MED_VARIABLE_ATTRIBUTE)
      strcpy(_datagroupname4,MED_VARATR_NOM);
    else
      strcpy(_datagroupname4,MED_COOTRF_NOM);

    if ((_datagroup4 = _MEDdatagroupOuvrir(_datagroup,_datagroupname4)) < 0) {
      _n=0;
      goto SORTIE;
    }
  }
  if ( (meddatatype==MED_VARIABLE_ATTRIBUTE) || (meddatatype==MED_COORDINATE_TRSF ) )
    _datagroupf=_datagroup4;
  else
    _datagroupf=_datagroup;


  /*
   * Construction du nom du dataset à lire
   */
  if (  _MEDgetDatasetName(_datasetname,meddatatype,cmode) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetDatasetName");
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
    SSCRUTE(_datagroupname3);goto ERROR;
  }

  /*
   *  MAJ du champ transformation pour le (meddatatype==MED_VARIABLE_ATTRIBUTE) || (meddatatype==MED_COORDINATE_TRSF)
   */
  if (_datagroup4) {
    if ( _MEDattrEntierLire(_datagroup4,MED_NOM_CGT,&_transformation) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
      SSCRUTE(_datagroupname3);SSCRUTE(_datagroupname4);SSCRUTE(MED_NOM_CGT);
      goto ERROR;
    }
  }

  if ( (_dataset = _MEDdatasetOuvrir(_datagroupf,_datasetname)) < 0) {
    _n=0;_dataset=0;*transformation = (med_bool) MED_FALSE;
/*     if ( (numdt != MED_NO_DT) || (numit != MED_NO_IT) ) { */
/*       MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATASET,_datasetname); */
/*       SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt); */
/*       SSCRUTE(_datagroupname2);SSCRUTE(_datagroupname3);SSCRUTE(_datagroupname4); */
/*       ISCRUTE_size(_n); */
/*       goto ERROR; */
/*     } */
    goto SORTIE;
  }


  if (!_datagroup4) {

    /*
     * Lire l'attribut CGT pour savoir si le dataset a changé
     * n'a pas de sens si le dataset est en fait un lien vers le dataset précedent.
     * En testant si le dataset est un lien on détermine si un changement a eu lieu
     * depuis la séquence de calcul précédente.
     * Ce traitement ne doit pas être effectué pour le pas de temps initial et pour
     * (meddatatype==MED_VARIABLE_ATTRIBUTE) || (meddatatype==MED_COORDINATE_TRSF)
     */

    if ( _MEDisasoftlink(_datagroupf, _datasetname,MED_TRUE, &_isasoftlink ) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_LINK,_datasetname);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
      SSCRUTE(_datagroupname2);SSCRUTE(_datagroupname3);SSCRUTE(_datagroupname4);
      ISCRUTE_size(_n);goto ERROR;
    }

    if (_isasoftlink)
      _transformation = MED_FALSE;
    else {

      if ( _MEDattrEntierLire(_dataset,MED_NOM_CGT,&_transformation) < 0) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
	SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
	SSCRUTE(_datagroupname3);SSCRUTE(MED_NOM_CGT);
	goto ERROR;
      }
    }

  }

  *transformation = (med_bool) _transformation;

  if ( ( meddatatype == MED_CONNECTIVITY) || ( meddatatype == MED_COORDINATE ) ) {
    *transformation &= (med_bool) !_changement_s;
  }

  /*
   * Attribut PFL (nombre de noeuds ou d'elements)
   * la lecture est faite sur le datagroup _datagroup et non sur _datagroupf
   * pour (meddatatype==MED_VARIABLE_ATTRIBUTE) || (meddatatype==MED_COORDINATE_TRSF)
   */
  _profilearraysize = 0;
  profilename[0]='\0';

  if ( _MEDattrStringLire(_datagroup,MED_NOM_PFL,MED_NAME_SIZE,_profilename) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
    SSCRUTE(_datagroupname3); SSCRUTE(MED_NOM_PFL);SSCRUTE(_profilename);goto ERROR;
  }

  if ( strcmp(_profilename,MED_NO_PROFILE_INTERNAL) ) {
    strncpy(profilename,_profilename,MED_NAME_SIZE+1);
    profilename[MED_NAME_SIZE]='\0';

    if ( ( _profilearraysize = MEDprofileSizeByName( fid,_profilename) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);SSCRUTE(_datagroupname2);
      SSCRUTE(_profilename);SSCRUTE("MEDprofileSizeByName");goto ERROR;
    }
  }
  *profilesize=(med_int) _profilearraysize;
  

  /*
   * Attribut NBR (nombre d'entité)
   */
  if ( _MEDattrEntierLire(_dataset,MED_NOM_NBR,&_intn) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE(numit);ISCRUTE(numdt);
    SSCRUTE(_datagroupname2);SSCRUTE(_datagroupname3);SSCRUTE(_datagroupname4);
    SSCRUTE(_datasetname);SSCRUTE(MED_NOM_NBR);ISCRUTE_size(_n);goto ERROR;
  }
  _n = _intn;

  if (_profilearraysize)
    switch(storagemode) {

    case MED_GLOBAL_STMODE :
      break;

    case MED_COMPACT_STMODE :
      if ( meddatatype!=MED_COORDINATE_TRSF )
	_n=_profilearraysize;

      break;

    default :
      MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_STORAGEMODE,MED_ERR_VALUE_MSG);
      ISCRUTE_int(storagemode);goto ERROR;
      break;

    }

 SORTIE:

  _ret = _n;

 ERROR:


  if (_dataset>0)     if (_MEDdatasetFermer(_dataset) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATASET,MED_NOM_COO);
    ISCRUTE_id(_dataset);
  }

  if (_datagroup4>0)     if (_MEDdatagroupFermer(_datagroup4) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname4);
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

/*   _MEDobjetsOuverts(fid); */

  *fret = _ret;
  va_end(params);
  return;
}
