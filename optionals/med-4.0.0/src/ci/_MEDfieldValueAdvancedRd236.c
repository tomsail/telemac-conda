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


#include <med_config.h>
#include <med.h>
#include <med_outils.h>

#include <string.h>
#include <stdlib.h>

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"


void _MEDfieldValueAdvancedRd236(int dummy,...) {


  med_err               _ret=-1;
/*   med_int               _nconstituentpervalue=0,_nvaluesperentityfromloc=0; */
/*   med_int               _nvaluesperentity=0,_profilearraysize=0; */
  med_bool              _filterparameterexist=MED_FALSE;

  char _localizationname   [MED_NAME_SIZE+1]=""; /*TODO DEFAULT? */
  char _profilename        [MED_NAME_SIZE+1]=""; /*TODO DEFAULT? */
  char _geotypename[MED_TAILLE_NOM_ENTITE+1]="";
  char _pfltmp             [MED_NAME_SIZE+1]="";
  med_filter *               _filter           = NULL;
  med_filter                 _tmpfilter        = MED_FILTER_INIT;
  med_filter                 _paramfilter      = MED_FILTER_INIT;

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
   * Il n'existe pas de possibilité de filtrage par n° d'entité sur les champs en 2.3.6
   */
  if (_filterparameterexist) 
    if ( (*_filter).filterarraysize ) {
      MED_ERR_(_ret,MED_ERR_NULL,MED_ERR_FILTER,MED_ERR_VALUE_MSG);
      ISCRUTE((*_filter).filterarraysize);
      goto ERROR;
    }

  NOFINALBLANK(profilename,ERROR);

  if ( strlen((*_filter).profilename) == 0 ) {  /* idem MED_NOPFL*/

    _profilename[0]='\0';
/*     _profilearraysize = MED_UNDEF_SIZE; */
  } else {
    strncpy(_profilename,(*_filter).profilename,MED_NAME_SIZE+1);
    _profilename[MED_NAME_SIZE]='\0'; /*On tronque les eventuels noms trop long*/

    /* Le paramètre (*_filter).profilearraysize est positionné dans  _MEDfilterEntityCr236 */
    /* Il est donc inutile de vérifier sa valeur  */
  }


  /*Pour vérifier les paramètres nvaluesperentity et nconstituentpervalue, il faudrait 
   appeler _MEDfieldnValue236 mais qui effectue aussi un check */
/*   if ( _nvaluesperentityfromloc != _nvaluesperentity ) { */
/*     MED_ERR_(_ret,MED_ERR_NOTEQUAL,MED_ERR_ATTRIBUTE,MED_ERR_LOCALIZATION_MSG); */
/*     SSCRUTE(_localizationname);SSCRUTE(MED_NOM_GEO);ISCRUTE(_nvaluesperentityfromloc); */
/*     ISCRUTE(_nvaluesperentity);goto ERROR; */
/*   } */

/*   if (!_filterparameterexist) (*_filter).nvaluesperentity=_nvaluesperentity; */

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

    if ( MEDchampLire( fid, (char *) meshname, (char *) fieldname, (unsigned char *) value,
		       (*_filter).switchmode, (*_filter).constituentselect,
		       _localizationname, _pfltmp,
		       (*_filter).storagemode,
		       entitytype, geotype, numdt, numit) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDchampLire");
      SSCRUTE(fieldname);SSCRUTE(_profilename);
      goto ERROR;
    }

    if (strcmp(_profilename,_pfltmp)) {
      MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PROFILE,MED_ERR_NAME_MSG);
      SSCRUTE(_profilename);SSCRUTE(_pfltmp);
      goto ERROR;
    }

    /*
     * On ferme tout
     */

    _ret = 0;

  ERROR:

/*   if ( pfluse ) { free(pfltab); free(pfltabtmp);} */
    
  va_end(params);
  *fret = _ret;
  return;
  }
  

