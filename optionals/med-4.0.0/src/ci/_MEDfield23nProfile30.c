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

void
_MEDfield23nProfile30(int dummy, ...) {


  med_int  _ret=-1,_err=-1;
  med_idt  _gid=0,_datagroup1=0;
  char     _path[(MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1)+(2*MED_MAX_PARA+1)+1]=MED_FIELD_GRP;
  char     _datagroupname1[2*MED_TAILLE_NOM_ENTITE+2]="";
  char     _geotypename      [MED_TAILLE_NOM_ENTITE+1]="";
  char     _profilename  [MED_NAME_SIZE+1]="";
  med_int  _numdt=0, _numit=0;
  med_size _n=0;


  MED_VARGS_DECL(const, med_idt           , , fid                     );
  MED_VARGS_DECL(const, char * , const      , fieldname               );
  MED_VARGS_DECL(const, med_int           , , numdt                   );
  MED_VARGS_DECL(const, med_int           , , numit                   );
  MED_VARGS_DECL(const, med_entity_type   , , entitype                );
  MED_VARGS_DECL(const, med_geometry_type , , geotype                 );
  MED_VARGS_DECL(const, int               , , meshit                  );
  MED_VARGS_DECL(, char *, const      , meshname                      );
  MED_VARGS_DECL(, char *, const      , defaultprofilename            );
  MED_VARGS_DECL(, char *, const      , defaultlocalizationname       );
  MED_VARGS_DECL(, med_int *         ,, fret                          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid                     );
  MED_VARGS_DEF(const, char * , const      , fieldname               );
  MED_VARGS_DEF(const, med_int           , , numdt                   );
  MED_VARGS_DEF(const, med_int           , , numit                   );
  MED_VARGS_DEF(const, med_entity_type   , , entitype                );
  MED_VARGS_DEF(const, med_geometry_type , , geotype                 );
  MED_VARGS_DEF(const, int               , , meshit                  );
  MED_VARGS_DEF(, char *, const      , meshname                      );
  MED_VARGS_DEF(, char *, const      , defaultprofilename            );
  MED_VARGS_DEF(, char *, const      , defaultlocalizationname       );
  MED_VARGS_DEF(, med_int *         ,, fret                          );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  NOFINALBLANK(fieldname,ERROR);

  if (meshit != 1 ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,"meshit");
    ISCRUTE_int(meshit);
    goto ERROR;
  }

  /*
   * On construit le chemin d'acc�s jusqu'aux groupes <type_ent>[.<type_geo>]
   */
  strcat(_path,fieldname);

  /* Lecture de l'attribut MED_NOM_MAI */
  if ( _MEDattributeStringRdByName(fid,_path,MED_NOM_MAI,MED_NAME_SIZE,meshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(_path);SSCRUTE(MED_NOM_MAI);SSCRUTE(meshname);
    goto ERROR;
  }

  strcat(_path,"/");
  _MEDgetComputationStepName(MED_SORT_DTIT,numdt,numit,&_path[strlen(_path)]);
  strcat(_path,"/");

  if ((_gid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_path); goto ERROR;
  }


  /*Lit l'attribut MED_NOM_NDT  */
  if ( _MEDattrEntierLire(_gid,MED_NOM_NDT, &_numdt) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(MED_NOM_NDT);
    ISCRUTE(_numdt);goto ERROR;
  }

  if ( _numdt != numdt) {
    MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);ISCRUTE(_numdt);ISCRUTE(numdt);goto ERROR;
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

  if (_MEDgetEntityTypeName(_datagroupname1,entitype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ENTITY,_datagroupname1);
    goto ERROR;
  }

  if (entitype != MED_NODE) {
    if ( (entitype == MED_STRUCT_ELEMENT) && (geotype != MED_NO_GEOTYPE )) {
      if ( MEDstructElementName(fid,geotype,_geotypename) < 0 ) {
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
    *defaultprofilename='\0';
    *defaultlocalizationname='\0';
    _ret = 0;
    goto ERROR;
  }

  if ((_err=_MEDnObjects(_datagroup1,".",&_n)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_datagroupname1);
      goto ERROR;
    }

  /* Lit le nom du profil associ� s'il en existe d�j� un */
  if ( _MEDattrStringLire(_datagroup1,MED_NOM_PFL,MED_NAME_SIZE,_profilename) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(MED_NOM_PFL);
    goto ERROR;
  }

  if ( strcmp(_profilename,MED_NO_PROFILE_INTERNAL) ) {
    strncpy(defaultprofilename,_profilename,MED_NAME_SIZE+1);
    defaultprofilename[MED_NAME_SIZE]='\0';
  } else {
    defaultprofilename[0]='\0';
  }

  /* Lit le nom de la localization associ� s'il en existe d�j� une */
  if ( _MEDattrStringLire(_datagroup1,MED_NOM_GAU,MED_NAME_SIZE,defaultlocalizationname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(MED_NOM_GAU);
    goto ERROR;
  }

  _ret = (med_int) _n;

 ERROR:


  if (_datagroup1>0)            if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    SSCRUTE(_datagroupname1); ISCRUTE_id(_datagroup1);
  }

  if (_gid>0)            if (_MEDdatagroupFermer(_gid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_gid);
  }

  va_end(params);
  *fret = _ret;
  return;
}
