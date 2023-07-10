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

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

void
_MEDfieldnProfile236(int dummy, ...) {


  med_int  _ret=-1,_err=-1;
  med_idt  _gid=0,_datagroup1=0;
  char     _path[(MED_FIELD_GRP_SIZE+MED_NAME_SIZE+1)+(2*MED_MAX_PARA+1)+1]=MED_FIELD_GRP;
  char     _datagroupname1  [2*MED_TAILLE_NOM_ENTITE+2]="";
  char     _nomdatagroup2   [2*MED_MAX_PARA+1]         ="";
  char     _geotype         [MED_TAILLE_NOM_ENTITE+1]  ="";
  char     _profilename     [MED_NAME_SIZE+1]          ="";
  char     _meshname        [MED_NAME_SIZE+1]          ="";
  char     _localizationname[MED_NAME_SIZE+1]          ="";
  med_int  _numdt=0, _numit=0;
  med_size _n=0,_ncpst=0;
  med_bool _checkmultiplemesh=MED_TRUE, _multiplemesh        =MED_FALSE;
  med_bool _checkmeshname    =MED_TRUE,  _samedefaultmeshname=MED_FALSE;
  char     _tmp1         [MED_TAILLE_NOM_ENTITE+1]="";
/*   med_size _rank; */


  MED_VARGS_DECL(const, med_idt           , , fid                     );
  MED_VARGS_DECL(const, char * , const      , fieldname               );
  MED_VARGS_DECL(const, med_int           , , numdt                   );
  MED_VARGS_DECL(const, med_int           , , numit                   );
  MED_VARGS_DECL(const, med_entity_type   , , entitype                );
  MED_VARGS_DECL(const, med_geometry_type , , geotype                 );
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
  MED_VARGS_DEF(, char *, const      , defaultprofilename            );
  MED_VARGS_DEF(, char *, const      , defaultlocalizationname       );
  MED_VARGS_DEF(, med_int *         ,, fret                          );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  NOFINALBLANK(fieldname,ERROR);

  /*
   * Autorise la lecture du profile et de la localisation des points d'intégration
   * s'il n'y a qu'un même maillage sur tous les pas de temps de tous les couples
   * (typent,typgeo)
   */
#if ! defined(MED_CHECK_23FORMAT)
#error "Data model objects version checking must be either activated or deactivated, hence MED_CHECK_23FORMAT macro must be defined."
#error "Verify that you include med_config.h, thanks."
#endif

# if MED_CHECK_23FORMAT == 1

  if (  _MEDfieldComputingStepCheck236(fid,  fieldname,
				      &_ncpst,
				      _checkmultiplemesh, &_multiplemesh,
				      _checkmeshname,     &_samedefaultmeshname) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_FIELD,fieldname);
    goto ERROR;
  }
# endif

  strcat(_path,fieldname);
  strcat(_path,"/");

  if (_MEDnomEntite(_datagroupname1,entitype) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(entitype);SSCRUTE(_path);goto ERROR;
  };

  if ((entitype != MED_NOEUD)) {
    if (_MEDnomGeometrie30(_tmp1,geotype) < 0) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
      ISCRUTE_int(geotype);SSCRUTE(_path);goto ERROR;
    }
    strcat(_datagroupname1,".");
    strcat(_datagroupname1,_tmp1);
  }
  strcat(_path,_datagroupname1);

  if ((_datagroup1 = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    _ret = 0;
    goto ERROR;
  }

  strcat(_path,"/");

  /*TODO : Vérifier IFORMAT */
  sprintf(_nomdatagroup2,"%*li%*li",MED_MAX_PARA,(long ) numdt,MED_MAX_PARA,(long ) numit);

  strcat(_path,_nomdatagroup2);

/*   if (  _MEDobjectGetRank(fid, */
/* 			  _path, */
/* 			  _rank ) < 0 ) { */
/*     MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDobjectGetRank"); */
/*     goto ERROR; */
/*   } */

  if ((_gid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    _ret = 0;
    goto ERROR;
  }
  strcat(_path,"/");

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


  /*Pas d'info maillage local ou distant*/
  if (_MEDattrStringLire(_gid,MED_NOM_MAI,MED_TAILLE_NOM,_meshname) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_path);SSCRUTE(MED_NOM_MAI);
    SSCRUTE(_meshname); goto ERROR;
  };

  strcat(_path,_meshname);

  if ( _MEDattributeStringRdByName(fid, _path, MED_NOM_GAU, MED_TAILLE_NOM, _localizationname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_ATTRIBUTE, MED_ERR_NAME_MSG );
    SSCRUTE(MED_NOM_GAU);SSCRUTE(_localizationname);goto ERROR;
  }

  if ( ! strcmp(_localizationname,MED_NOGAUSSi))
    strcpy(defaultlocalizationname,MED_NO_LOCALIZATION);
  else
    strcpy(defaultlocalizationname,_localizationname);

  if ( _MEDattributeStringRdByName(fid, _path, MED_NOM_PFL, MED_TAILLE_NOM, _profilename) < 0 ) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_ATTRIBUTE, MED_ERR_NAME_MSG );
    SSCRUTE(MED_NOM_PFL);SSCRUTE(_profilename);goto ERROR;
  }

  if ( strcmp(_profilename,MED_NOPFLi) &&
       strcmp(_profilename,"") ) /* le test MED_NOPFLi pour des raisons de compatibilité */
    {
      strcpy(defaultprofilename,_profilename);
    }  else {
    strcpy(defaultprofilename, MED_NO_PROFILE);
  }


  _ret = (med_int) 1;

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
