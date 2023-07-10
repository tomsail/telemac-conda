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

/**\ingroup MEDlocalization
  \brief \MEDlocalizationWrBrief
  \param fid \fid
  \param localizationname \localizationname
  \param geotype \geotype
  \param spacedimension \spacedimension
  \param elementcoordinate \elementcoordinate
  \param switchmode \switchmode
  \param nipoint \nipoint
  \param ipointcoordinate \ipointcoordinate
  \param weight \weight
  \param geointerpname        \geointerpname
  \param sectionmeshname \sectionmeshname
  \retval med_err  \error
  \details \MEDlocalizationWrDetails
  \par Définition
  \MEDlocalizationDef
  \remarks
  \MEDlocalizationRem
  \MEDlocalizationWrRem
 */

med_err
MEDlocalizationWr(const med_idt           fid,
		  const char *     const  localizationname,
		  const med_geometry_type geotype,
		  const med_int           spacedimension,
		  const med_float* const  elementcoordinate,
		  const med_switch_mode   switchmode,
		  const med_int           nipoint,
		  const med_float* const  ipointcoordinate,
		  const med_float* const  weight,
		  const char *     const  geointerpname,
		  const char *     const  sectionmeshname
		  )
{
  med_access_mode _MED_ACCESS_MODE;
  med_err _ret = -1;
  med_idt _lzid=0, _root=0;
  med_int _nentity=0;
  med_int _intgeotype = -1;
  char    _path[MED_LOCALIZATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_LOCALIZATION_GRP;
  med_filter     _filter        = MED_FILTER_INIT;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
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
   * MED_GAUSS_ELNO est un mot cle reserve
   */
  if (! strcmp(localizationname,MED_GAUSS_ELNO)) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_LOCALIZATION,localizationname);
    goto ERROR;
  }

  /*
   * Si le DataGroup /GAUSS/ n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOuvrir(fid,_path)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,_path)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,_path);
     goto ERROR;
    }

  NOFINALBLANK(localizationname,ERROR);
  /*
   * Si le DataGroup /GAUSS/<localizationname> n'existe pas, on le cree
   */
  if ((_lzid = _MEDdatagroupOuvrir(_root,localizationname)) < 0)
    if ((_lzid = _MEDdatagroupCreer(_root,localizationname)) < 0) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,localizationname);
      SSCRUTE(_path);goto ERROR;
    }

  strcat(_path,localizationname);


  /*
   * On stocke <nipoint> sous forme d'attribut
   */
  if (_MEDattributeIntWr(_lzid,MED_NOM_NBR,&nipoint) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(localizationname);SSCRUTE(MED_NOM_NBR);ISCRUTE(nipoint);
    goto ERROR;
  }


  /*
   * On stocke <typgeo> sous forme d'attribut
   */
  _intgeotype = (med_int) geotype;
  /* sizeof(enum) tjrs = sizeof(int) en C, or
     sur machines 64 bits par d�faut med_int==long,
     du coup sur  machines 64 bits _MEDattributeIntWr utilise 
     le type hdf NATIVE_LONG, ce qui pose un probl�me qd on passe
     un enum.
  */
  if (_MEDattributeIntWr(_lzid,MED_NOM_GEO,&_intgeotype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE(_intgeotype);
    goto ERROR;
  };

  /*
   * On stocke <spacedimension> sous forme d'attribut
   */
  if (_MEDattributeIntWr(_lzid,MED_NOM_DIM,&spacedimension) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(localizationname);SSCRUTE(MED_NOM_DIM);ISCRUTE(spacedimension);
    goto ERROR;
  };


  if ( ! ((geotype > MED_STRUCT_GEO_INTERNAL    ) &&
	  (geotype < MED_STRUCT_GEO_SUP_INTERNAL)    ) ) {
    /*
     * On stocke les coordonn�es de r�f�rence dans un dataset
     */
    _nentity = (geotype%100);

    if ( MEDfilterEntityCr(fid,_nentity, 1, spacedimension, MED_ALL_CONSTITUENT,
			   switchmode,MED_UNDEF_STMODE,
			   MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
      goto ERROR;
    }

    if ( _MEDdatasetWr(_lzid,MED_NOM_COO,MED_INTERNAL_FLOAT64,&_filter, elementcoordinate) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_COO);SSCRUTE(_path);
      goto ERROR;
    }

    if ( MEDfilterClose(&_filter) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_LOCALIZATION_MSG); SSCRUTE(_path);
      goto ERROR;
    }
  } else {

    /*
     * Creation/Ecriture de l'attribut MED_NOM_NOM (nom du maillage de section)
     */
    if ( _MEDattributeStringWr(_lzid,MED_NOM_NOM,MED_NAME_SIZE,sectionmeshname) < 0) {
      MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
      SSCRUTE(localizationname);SSCRUTE(MED_NOM_NOM);SSCRUTE(sectionmeshname);
      goto ERROR;
    }
  }

  /*
   * Creation/Ecriture de l'attribut MED_NOM_INM (nom de l'interpolation géométrique)
   */
  if ( _MEDattributeStringWr(_lzid,MED_NOM_INM,MED_NAME_SIZE,geointerpname) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG);
    SSCRUTE(localizationname);SSCRUTE(MED_NOM_INM);SSCRUTE(geointerpname);
    goto ERROR;
  }


  /*
   * On stocke les points d'int�gration dans un dataset
   */

  _nentity = nipoint;
  if ( MEDfilterEntityCr(fid,_nentity, 1, spacedimension, MED_ALL_CONSTITUENT,
			 switchmode,MED_UNDEF_STMODE,
			 MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetWr(_lzid,MED_NOM_GAU,MED_INTERNAL_FLOAT64,&_filter, ipointcoordinate) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_GAU);SSCRUTE(_path);
    goto ERROR;
  }

  if ( MEDfilterClose(&_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILTER,MED_ERR_LOCALIZATION_MSG); SSCRUTE(_path);
    goto ERROR;
  }

  /*
   * On stocke les poids dans un dataset
   */

  _nentity = nipoint;
  if ( MEDfilterEntityCr(fid,_nentity, 1, 1, MED_ALL_CONSTITUENT,
			 switchmode,MED_UNDEF_STMODE,
			 MED_NO_PROFILE, MED_UNDEF_SIZE, NULL, &_filter) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_FILTER,MED_ERR_INTERNAL_MSG);
    goto ERROR;
  }

  if ( _MEDdatasetWr(_lzid,MED_NOM_VAL,MED_INTERNAL_FLOAT64,&_filter, weight) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_DATASET,MED_NOM_VAL);SSCRUTE(_path);
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

  return _ret;

}

