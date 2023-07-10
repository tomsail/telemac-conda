/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2013  EDF R&D, CEA/DEN
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
_MEDfieldCr31(int dummy, ...) {

  med_err _ret=-1;
  med_idt _root=0,_datagroup1=0;
  med_int _fieldtype ;
  char    _datagroupname1[MED_NAME_SIZE+1]="";
  med_access_mode _MED_ACCESS_MODE;


  MED_VARGS_DECL(const, med_idt           , , fid           );
  MED_VARGS_DECL(const, char * , const      , fieldname     );
  MED_VARGS_DECL(const, med_field_type    , , fieldtype     );
  MED_VARGS_DECL(const, med_int           , , ncomponent    );
  MED_VARGS_DECL(const, char *, const       , componentname );
  MED_VARGS_DECL(const, char *, const       , componentunit );
  MED_VARGS_DECL(const, char *, const       , dtunit        );
  MED_VARGS_DECL(const, char *, const       , meshname      );
  MED_VARGS_DECL(, med_err *              , , fret          );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt           , , fid           );
  MED_VARGS_DEF(const, char * , const      , fieldname     );
  MED_VARGS_DEF(const, med_field_type,     , fieldtype     );
  MED_VARGS_DEF(const, med_int           , , ncomponent    );
  MED_VARGS_DEF(const, char *, const       , componentname );
  MED_VARGS_DEF(const, char *, const       , componentunit );
  MED_VARGS_DEF(const, char *, const       , dtunit        );
  MED_VARGS_DEF(const, char *, const       , meshname      );
  MED_VARGS_DEF(, med_err *              , , fret          );


 _MEDmodeErreurVerrouiller();

 _fieldtype = (med_int) fieldtype;

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
   * Si le DataGroup MED_FIELD_GRP n'existe pas, on le cree
   */
  if ((_root = _MEDdatagroupOuvrir(fid,MED_FIELD_GRP)) < 0)
    if ((_root = _MEDdatagroupCreer(fid,MED_FIELD_GRP)) < 0) {
     MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_FIELD_GRP);
     goto ERROR;
    }

  NOFINALBLANK(fieldname,ERROR);
  /*
   * Si le Data Group "/CHA/<fieldname>" n'existe pas, on le cree
   */
  if ((_datagroup1 = _MEDdatagroupOuvrir(_root,fieldname)) < 0)
    if ((_datagroup1 = _MEDdatagroupCrOrderCr(_root,fieldname)) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CREATE,MED_ERR_DATAGROUP,MED_FIELD_GRP);
      SSCRUTE(fieldname);goto ERROR;
    }


  /*
   * Les infos sur les composantes du champ
   */
  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_NCO,&ncomponent) < 0 ) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NCO);
    ISCRUTE(ncomponent);goto ERROR;
  }



  
  /* Tous les types med_field_type sont autorisés dans MEDfieldCr mais :
     Avant la 3.3.0 seuls les types : MED_FLOAT64,MED_INT32 et MED_INT64 étaient autorisés dans MEDfieldValueAdvancedWr 
       et seul le type med_int et med_float64 pouvaient être utilisés en C.

      A l'écriture, si med_int=int  les champs MED_INT32 étaient stockés en interne en HDFINT32bits
      A l'écriture, si med_int=long les champs MED_INT32 étaient stockés en interne en HDFINT64bits
      A la lecture : - si med_int=int  le champ MED_INT32 est relu en en HDFINT32bit avec conversion 64->32 si necessaire
                    - si med_int=long le champ MED_INT32 est relu en en HDFINT64bit avec conversion 32->64 si necessaire
      A l'écriture, si med_int=int les champs MED_INT64 étaient interdits
      A l'écriture, si med_int=long les champs MED_INT64 étaient stockés en interne en HDFINT64bits
      A la lecture : - si med_int=int  le champ MED_INT64 ne pouvait pas être relu (pour prevenir la perte d'information)
                    - si med_int=long le champ MED_INT64 est relu sans conversion

     Depuis la 3.3.0 en plus des types MED_FLOAT64,MED_INT32 et MED_INT64 les types MED_FLOAT32 et MED_INT sont autorisés dans MEDfieldValueAdvancedWr et aux types med_int et med_float64 utilisés en C sont ajoutés les types med_float32, med_int32 et med_int64.

     MED_INT32 :
      A l'écriture :  si med_int=int  les champs MED_INT32 sont toujours  stockés en interne en HDFINT32bits   (utiliser med_int32 ou med_int(!))
                      si med_int=long les champs MED_INT32 sont désormais stockés en interne en HDFINT32bits   (utiliser le type med_int64) et désormais 
      A la lecture :  si med_int=int  les champs MED_INT32 sont toujours  relus en HDFINT32bits sans conversion (utiliser med_int32 ou med_int(!))
                      si med_int=long les champs MED_INT32 sont désormais relus en HDFINT32bits sans conversion (utiliser le type med_int32)
     MED_INT64 :
      A l'écriture :  si med_int=int  les champs MED_INT64 sont désormais autorisés et stockés en interne en HDFINT64bits  (utiliser le type med_int64)
                      si med_int=long les champs MED_INT64 sont toujours  autorisés et stockés en interne en HDFINT64bits  (utiliser le type med_int64 ou med_int(!))
      A la lecture :  si med_int=int  les champs MED_INT64 sont désormais relus en HDFINT64bits sans conversion (utiliser le type med_int64)
                      si med_int=long les champs MED_INT64 sont toujours  relus en HDFINT64bits sans conversion  (utiliser le type med_int64 ou med_int(!))
    
     MED_INT :
      A l'écriture :  si med_int=int  les champs MED_INT sont désormais acceptés et stockés en interne en HDFINT32bits    (utiliser med_int ou med_int32(!))
                      si med_int=long les champs MED_INT sont désormais acceptés et stockés en interne en HDFINT64bits    (utiliser med_int ou med_int64(!)) 
      A la lecture :  si med_int=int  les champs MED_INT sont désormais acceptés et relus en HDFINT32bits avec conversion/maxint si necessaire (utiliser med_int ou med_int32(!))
                      si med_int=long les champs MED_INT sont désormais acceptés et relus en HDFINT64bits avec conversion        si necessaire (utiliser le type med_int32)
   

     Plateforme 32 bits : TODO si la plateforme est 32 bits, il faut .... ?
  */

  if ( _MEDattributeIntWr(_datagroup1,MED_NOM_TYP,&_fieldtype) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_TYP);
    ISCRUTE(_fieldtype);goto ERROR;
  }
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_NOM,
			    MED_SNAME_SIZE*ncomponent,componentname) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_NOM);
    SSCRUTE(componentname);goto ERROR;
  }
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_UNI,
			    MED_SNAME_SIZE*ncomponent,componentunit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_UNI);
    SSCRUTE(componentunit);goto ERROR;
  }

  /*MODEL : MED_NOM_UNI vient du niveau en dessous
    Cree ou ouvre l'attribut  MED_NOM_UNI pour écriture
  */
  if ( _MEDattributeStringWr(_datagroup1,MED_NOM_UNT,MED_SNAME_SIZE,dtunit) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_UNT);
    SSCRUTE(dtunit);goto ERROR;
  }

  /*MODEL : MED_NOM_MAI est écrit dans l'API de création de champ
   * Si c'est la première référence à un maillage, initialise l'attribut MED_MAI à ce maillage
   */
  NOFINALBLANK(meshname,ERROR);

  if (_MEDattributeStringWr(_datagroup1,MED_NOM_MAI,MED_NAME_SIZE,meshname) < 0) {
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,MED_ERR_FIELD_MSG);
    SSCRUTE(fieldname);SSCRUTE(_datagroupname1);SSCRUTE(MED_NOM_MAI);
    SSCRUTE(meshname);
    MED_ERR_(_ret,MED_ERR_WRITE,MED_ERR_ATTRIBUTE,meshname);
    goto ERROR;
  }


  /*
   * On ferme tout
   */

  _ret=0;
 ERROR:

  if (_datagroup1>0)     if (_MEDdatagroupFermer(_datagroup1) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_datagroupname1);
    ISCRUTE_id(_datagroup1);
  }

  if (_root>0)            if (_MEDdatagroupFermer(_root) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,MED_FIELD_GRP);
    ISCRUTE_id(_root);
  }

  va_end(params);
  *fret = _ret;
  return;
}
