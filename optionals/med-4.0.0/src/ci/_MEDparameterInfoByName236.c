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


#include "med_config.h"
#include <med.h>
#include "med_outils.h"
#include <string.h>

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"


void _MEDparameterInfoByName236(int dummy, ...) {


  med_err  _ret=-1,_err=0;
  med_idt  _paramidt=0,_cpstid=0;
  char     _path[MED_NUMERICAL_DATA_GRP_SIZE+MED_TAILLE_NOM+1+1]=MED_NUMERICAL_DATA_GRP;
  char     _cpstname[2*MED_MAX_PARA+1]="";
  med_size _nstep=0;
  med_int  _intparamtype=0;

  MED_VARGS_DECL(const, med_idt              , , fid         );
  MED_VARGS_DECL(const, char   *       , const , paramname   );
  MED_VARGS_DECL(, med_parameter_type *, const , paramtype   );
  MED_VARGS_DECL(, char *              , const , description );
  MED_VARGS_DECL(, char *              , const , dtunit      );
  MED_VARGS_DECL(, med_int *           , const , nstep       );
  MED_VARGS_DECL(, med_err *                  ,, fret        );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt              , , fid         );
  MED_VARGS_DEF(const, char   *       , const , paramname   );
  MED_VARGS_DEF(, med_parameter_type *, const , paramtype   );
  MED_VARGS_DEF(, char *              , const , description );
  MED_VARGS_DEF(, char *              , const , dtunit      );
  MED_VARGS_DEF(, med_int *           , const , nstep       );
  MED_VARGS_DEF(, med_err *                  ,, fret        );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On construit le chemin d'acces
   */
  strcat(_path,paramname);
  strcat(_path,"/");

  if ((_err=_MEDnObjects(fid, _path, &_nstep)) < 0 )
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    }
  *nstep=(med_int) _nstep;

  if ((_paramidt = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    goto ERROR;
  }

  /*
   * L'attribut "TYP"
   */
  if ( _MEDattrEntierLire(_paramidt, MED_NOM_TYP, &_intparamtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_TYP);ISCRUTE(_intparamtype);
    goto ERROR;
  }
  *paramtype = (med_parameter_type) _intparamtype;

  /*
   * L'attribut DES
   */
  if ( _MEDattrStringLire(_paramidt,MED_NOM_DES,MED_TAILLE_DESC,description) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(MED_NOM_DES);SSCRUTE(description);
    goto ERROR;
  }

  if ( _MEDobjectGetName( _paramidt,".",0,_cpstname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);SSCRUTE(_cpstname);
    goto ERROR;
  }


  /* Lecture de l'attribut MED_NOM_UNI */
  if ( _MEDattributeStringRdByName(_paramidt,_cpstname,MED_NOM_UNI,MED_TAILLE_PNOM,dtunit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,_path);
    SSCRUTE(_cpstname);SSCRUTE(MED_NOM_UNI);SSCRUTE(dtunit);goto ERROR;
  }

 _ret = 0;

 ERROR:


 if (_paramidt>0)         if (_MEDdatagroupFermer(_paramidt) < 0) {
   MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
   ISCRUTE_id(_paramidt);
 }

  va_end(params);
  *fret = _ret;
  return;
}
