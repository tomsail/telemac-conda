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

void
_MEDlocalizationRd30(int dummy, ...) {


  med_err _ret = -1;
  med_idt _lzid=0, _root=0;
  med_int _nentity=0,_nipoint=0,_localizationspacedimension=0;
  med_int _intgeotype = -1;
  char    _path[MED_LOCALIZATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_LOCALIZATION_GRP;
  med_filter     _filter        = MED_FILTER_INIT;

  char     _elempath[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  char     _supportmeshname[MED_NAME_SIZE+1]="";
  med_bool _chgt=MED_FALSE,_trsf=MED_FALSE;
  med_int  _nnodes=0;
  med_int  _supportmeshspacedimension=0;
  char     _meshpath[MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_SUPPORT_GRP;

  MED_VARGS_DECL(const, med_idt                 , , fid               );
  MED_VARGS_DECL(const, char*            , const  , localizationname  );
  MED_VARGS_DECL(const, med_switch_mode         , , switchmode        );
  MED_VARGS_DECL(, med_float*      , const  , elementcoordinate       );
  MED_VARGS_DECL(, med_float*      , const  , ipointcoordinate        );
  MED_VARGS_DECL(, med_float*      , const  , weight                  );
  MED_VARGS_DECL(, med_err *                     ,, fret              );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt                 , , fid               );
  MED_VARGS_DEF(const, char*            , const  , localizationname  );
  MED_VARGS_DEF(const, med_switch_mode         , , switchmode        );
  MED_VARGS_DEF(, med_float*      , const  , elementcoordinate       );
  MED_VARGS_DEF(, med_float*      , const  , ipointcoordinate        );
  MED_VARGS_DEF(, med_float*      , const  , weight                  );
  MED_VARGS_DEF(, med_err *                     ,, fret              );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * MED_GAUSS_ELNO est un mot cle reserve
   */
  if (! strcmp(localizationname,MED_GAUSS_ELNO)) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_LOCALIZATION,localizationname);
    goto ERROR;
  }

  /*
   * Ouverture du dataGroup /GAUSS/
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  NOFINALBLANK(localizationname,ERROR);
  /*
   * Ouverture du dataGroup  /GAUSS/<localizationname>
   */
  if ((_lzid = _MEDdatagroupOuvrir(_root,localizationname)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,localizationname);
    SSCRUTE(_path);goto ERROR;
  }

  strcat(_path,localizationname);


  /*
   * On lit <nipoint> sous forme d'attribut
   */
  if (_MEDattrEntierLire(_lzid,MED_NOM_NBR,&_nipoint) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(localizationname);SSCRUTE(MED_NOM_NBR);ISCRUTE(_nipoint);
    goto ERROR;
  }

  /*
   * On lit <typgeo> sous forme d'attribut
   */
  /* sizeof(enum) tjrs = sizeof(int) en C, or
     sur machines 64 bits par défaut med_int==long,
     du coup sur  machines 64 bits _MEDattributeIntWr utilise
     le type hdf NATIVE_LONG, ce qui pose un probl�me qd on passe
     un enum.
  */
  if (_MEDattrEntierLire(_lzid,MED_NOM_GEO,&_intgeotype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE(_intgeotype);
    goto ERROR;
  };

  /*
   * On lit <spacedimension>
   */
  if (_MEDattrEntierLire(_lzid,MED_NOM_DIM,&_localizationspacedimension) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(localizationname);SSCRUTE(MED_NOM_DIM);ISCRUTE(_localizationspacedimension);
    goto ERROR;
  };


  /*
   * On lit les coordonn�es de r�f�rence dans un dataset
   */
  if ( ! ((_intgeotype > MED_STRUCT_GEO_INTERNAL    ) &&
	  (_intgeotype < MED_STRUCT_GEO_SUP_INTERNAL)    ) ) {

    _nentity = (_intgeotype%100);

    if ( MEDfilterEntityCr(fid,_nentity, 1, _localizationspacedimension, MED_ALL_CONSTITUENT,
			   switchmode,MED_UNDEF_STMODE,
			   MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
      goto ERROR;
    }

    if ( _MEDdatasetRd(_lzid,MED_NOM_COO,MED_INTERNAL_FLOAT64,&_filter,
		       (unsigned char *) elementcoordinate) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,MED_NOM_COO);SSCRUTE(_path);
      goto ERROR;
    }

    if ( MEDfilterClose(&_filter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_LOCALIZATION_MSG); SSCRUTE(_path);
      goto ERROR;
    }
    /*Localisation des points d'intégration sur élément de structure */
  } else {

    if ( MEDstructElementName(fid,(med_int) _intgeotype,&_elempath[strlen(_elempath)]) < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDstructElementName");
      SSCRUTE(_elempath);goto ERROR;
    }

    /*
     * Lecture  de l'attribut MED_NOM_NOM (nom du maillage support)
     */
    if ( _MEDattributeStringRdByName(fid,_elempath,MED_NOM_NOM, MED_NAME_SIZE,
				     _supportmeshname) < 0) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_elempath);
      SSCRUTE(MED_NOM_NOM);SSCRUTE(_supportmeshname);
      goto ERROR;
    }

    /*
     * Lecture du nombre de noeuds support
     */
    if (strlen(_supportmeshname)) {

      strcat(_meshpath,_supportmeshname);
      if ( _MEDattributeNumRdByName(fid, _meshpath ,MED_NOM_ESP,MED_INTERNAL_INT,
				    (unsigned char *) &_supportmeshspacedimension) <0 ) {
	MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_meshpath);
	SSCRUTE(MED_NOM_ESP);
	goto ERROR;
      }

      /* La description des noeuds du modèle d'élément de structure de référence d'une localisation
       doit être de la dimension de celle de l'espace du maillage support 
       de définition de ce modèle d'élément */
      if (_supportmeshspacedimension != _localizationspacedimension) {
	MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_DIMENSION,"");
	ISCRUTE(_localizationspacedimension);ISCRUTE(_supportmeshspacedimension);
	goto ERROR;
      }


      if ( (_nnodes = MEDmeshnEntity(fid,_supportmeshname,MED_NO_DT,MED_NO_IT,
				     MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,
				     &_chgt,&_trsf) )  <= 0) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
	ISCRUTE(_nnodes);goto ERROR;
      }

      if ( MEDmeshNodeCoordinateRd(fid, _supportmeshname,
				   MED_NO_DT, MED_NO_IT, MED_FULL_INTERLACE,
				   elementcoordinate) <0) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshNodeCoordinateRd");
	goto ERROR;
      }
    } else {
      /*TODO : générer des coordonnées pour MED_PARTICLE? */
      _nnodes=1;
    }
  }

  /*
   * On lit les points d'intégration
   */

  _nentity = _nipoint;
  if ( MEDfilterEntityCr(fid,_nentity, 1, _localizationspacedimension, MED_ALL_CONSTITUENT,
			 switchmode,MED_UNDEF_STMODE,
			 MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

#ifdef _DEBUG_
  ISCRUTE((_filter).nentity              );
  ISCRUTE((_filter).nvaluesperentity     );
  ISCRUTE((_filter).nconstituentpervalue );
  ISCRUTE((_filter).constituentselect       );
  ISCRUTE_int((_filter).switchmode              );
  ISCRUTE((_filter).filterarraysize         );
  ISCRUTE((_filter).profilearraysize        );
  ISCRUTE_int((_filter).storagemode             );
  SSCRUTE((_filter).profilename             );
#endif

  if ( _MEDdatasetRd(_lzid,MED_NOM_GAU,MED_INTERNAL_FLOAT64,&_filter,
		     (unsigned char *)  ipointcoordinate) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,MED_NOM_GAU);SSCRUTE(_path);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_LOCALIZATION_MSG); SSCRUTE(_path);
    goto ERROR;
  }

  /*
   * On lit les poids associés
   */

  _nentity = _nipoint;
  if ( MEDfilterEntityCr(fid,_nentity, 1, 1, MED_ALL_CONSTITUENT,
			 switchmode,MED_UNDEF_STMODE,
			 MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetRd(_lzid,MED_NOM_VAL,MED_INTERNAL_FLOAT64,&_filter,
		     (unsigned char *) weight) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_DATASET,MED_NOM_VAL);SSCRUTE(_path);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_LOCALIZATION_MSG); SSCRUTE(_path);
    goto ERROR;
  }


  _ret = 0;

 ERROR:

  if (_lzid>0)            if (_MEDdatagroupFermer(_lzid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,localizationname);
    ISCRUTE_id(_lzid);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_LOCALIZATION_GRP);
    ISCRUTE_id(_root);
  }

  va_end(params);
  *fret = _ret;

  return;
}
