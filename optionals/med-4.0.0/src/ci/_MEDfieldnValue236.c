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
#include "med_versioned.h"

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"


void
_MEDfieldnValue236(int dummy, ...)
{


  med_int               _ret                 =-1;
  med_int               _nprofile         = 0;
  med_int               _n                   = 0;
  med_int               _profilearraysize    = 0,_nvaluesperentity=0;
  med_int               _nintegrationpoint= 0;
  med_bool              _anyprofile          = MED_FALSE;
  med_geometrie_element _locgeotype          = 0;
  med_int               _intlocgeotype       = 0;
  med_int               _nvaluesperentityfromloc=0;
  int                   _dummy                     =0;
  char     _localizationname  [MED_TAILLE_NOM+1]                 = "";
  char     _tmpprofilename    [MED_NAME_SIZE+1]                  = "", *_profilename=0;
  char     _path              [MED_LOCALIZATION_GRP_SIZE+MED_TAILLE_NOM+1]= MED_LOCALIZATION_GRP;
  char     _geotype           [MED_TAILLE_NOM_ENTITE+1]          = "";


  MED_VARGS_DECL(const, med_idt           , , fid                        );
  MED_VARGS_DECL(const, char * , const      , fieldname                  );
  MED_VARGS_DECL(const, med_int           , , numdt                      );
  MED_VARGS_DECL(const, med_int           , , numit                      );
  MED_VARGS_DECL(const, med_entity_type   , , entitytype                 );
  MED_VARGS_DECL(const, med_geometry_type , , geotype                    );
  MED_VARGS_DECL(, char *, const      , profilename                      );
  MED_VARGS_DECL(const, int               , , profileit                  );
  MED_VARGS_DECL(const, med_storage_mode  , , storagemode                );
  MED_VARGS_DECL(, med_int *, const   , profilesize                      );
  MED_VARGS_DECL(, char *, const      , localizationname                 );
  MED_VARGS_DECL(, med_int *, const   , nintegrationpoint             );
  MED_VARGS_DECL(, med_int *         ,, fret                             );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid                        );
  MED_VARGS_DEF(const, char * , const      , fieldname                  );
  MED_VARGS_DEF(const, med_int           , , numdt                      );
  MED_VARGS_DEF(const, med_int           , , numit                      );
  MED_VARGS_DEF(const, med_entity_type   , , entitytype                 );
  MED_VARGS_DEF(const, med_geometry_type , , geotype                    );
  MED_VARGS_DEF(, char *, const      , profilename                      );
  MED_VARGS_DEF(const, int               , , profileit                  );
  MED_VARGS_DEF(const, med_storage_mode  , , storagemode                );
  MED_VARGS_DEF(, med_int *, const   , profilesize                      );
  MED_VARGS_DEF(, char *, const      , localizationname                 );
  MED_VARGS_DEF(, med_int *, const   , nintegrationpoint             );
  MED_VARGS_DEF(, med_int *         ,, fret                             );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  NOFINALBLANK(fieldname,ERROR);

  if ( (profileit != 1) &&  
       (profileit != -1)) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,MED_ERR_VALUE_MSG);
    ISCRUTE_int(profileit);goto ERROR;
  }

  /*
   * Cette appel effectue l'appel à _MEDfieldComputingStepCheck236
   */
  _MEDfieldnProfile236(_dummy,fid,fieldname,numdt,numit,entitytype,geotype,
		       &_tmpprofilename,&_localizationname,&_nprofile );


  if ( _nprofile  < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDfieldnProfile236");
    goto ERROR;
  }

  /* REM: MEDnVal prend en compte le nombre de points de Gauss, ce qui n'est pas le cas
     de MEDfieldnValue */
  if ( (_n = MEDnVal(fid, (char *) fieldname,entitytype,geotype,
		     numdt,numit,MED_NOREF,(med_mode_profil) storagemode) ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDnVal");
    goto ERROR;
  }

  /*
   * Lecture de la taille du profil (eventuellement MED_ALLENTITIES_PROFILE)
   */

  /* REM:  (profileit < 0 ) signifie que l'on nous a fourni <profilename> */
  if ( profileit < 0 ) {
      if ( !strlen(profilename)) {
	_profilename = MED_NOPFL;
      }  else {
	if ( strcmp(_tmpprofilename,profilename) ) {
	  MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,profilename);
	  SSCRUTE(_tmpprofilename); goto ERROR;
	}
	_profilename=profilename;
	_anyprofile=MED_TRUE;
      }
  } else {
    strncpy(profilename,_tmpprofilename,MED_TAILLE_NOM+1);
    profilename[MED_TAILLE_NOM]='\0';
    _profilename=profilename;
    if ( strlen(profilename)) _anyprofile=MED_TRUE;
  }

  if ( _anyprofile ) {
    if ( (_profilearraysize=MEDprofileSizeByName(fid, _profilename)) < 0)  {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_FIELD_MSG);
      SSCRUTE(fieldname); SSCRUTE(_profilename);
      SSCRUTE("MEDprofileSizeByName");goto ERROR;
    }
  }

  /*
   * Lecture du nombre de points d'intégration.
   */

  strncpy(localizationname,_localizationname,MED_TAILLE_NOM+1);
  localizationname[MED_TAILLE_NOM]='\0';

  /* Vérification de la cohérence du  nombre de valeurs pas entité */
  if (entitytype == MED_NODE_ELEMENT ) {
    if ( strlen( _localizationname) ) {
	MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_PARAMETER,_localizationname);
	SSCRUTE(MED_NO_LOCALIZATION);ISCRUTE_int(entitytype);goto ERROR;
    }
    _nvaluesperentityfromloc = geotype % 100;
  } else if (! strcmp(_localizationname,MED_GAUSS_ELNO)) {
    /* Les points de Gauss sont d"finis sur les noeuds de l'element (mot cle) */
    /* le nombre de points de Gauss est egal au nombre de noeuds de l'element */
    _nvaluesperentityfromloc = geotype % 100;
  } else if ( strlen(_localizationname) ) {

    strcat(_path,_localizationname);

    if ( _MEDattributeNumRdByName(fid, _path, MED_NOM_NBR,MED_INTERNAL_INT,(unsigned char *) &_nvaluesperentityfromloc) < 0 ) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE, MED_ERR_NAME_MSG );
      SSCRUTE(MED_NOM_NBR);SSCRUTE(_path); ISCRUTE(_nvaluesperentityfromloc);goto ERROR;
    }

    if ( _MEDattributeNumRdByName(fid, _path, MED_NOM_GEO, MED_INTERNAL_INT,(unsigned char *) &_intlocgeotype) < 0 ) {
      MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE, MED_ERR_NAME_MSG );
      SSCRUTE(MED_NOM_GEO);SSCRUTE(_path); ISCRUTE(_intlocgeotype);goto ERROR;
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

  /* Pour effectuer la vérification suivante (incohérence dans le fichier suite à une 
     maj de la localisation ou du champ  sans prise en compte d'un nombre de points d'intégration différent),
     il faudrait lire l'attribut MED_NOM_NGA sur le maillage par défaut du champ*/
  /* Pour celà il faut soit developper une nouvelle routine soit utiliser MEDpasdetempsInfo mais qui
     est itérative*/
/*   if ( _nvaluesperentityfromloc != _nvaluesperentity ) { */
/*     MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG); */
/*     SSCRUTE(_localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE(_nvaluesperentityfromloc); */
/*     ISCRUTE(_nvaluesperentity);goto ERROR; */
/*   } */

  *nintegrationpoint = _nvaluesperentityfromloc;

  _n/=_nvaluesperentityfromloc;

  /*Rectification de la valeur 2.3.6 qui prend en compte le nombre de points d'intégration*/
  if (_anyprofile)
    *profilesize=_profilearraysize;
  else
    *profilesize=_n;


  _ret = _n;
 ERROR:

  va_end(params);
  *fret = _ret;

  return;
}
