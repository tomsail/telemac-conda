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


void _MEDparameterInfoByName30(int dummy, ...) {


  med_err _ret = -1, _err = -1;
  med_int _paramtype = 0;
  med_idt _paramidt = 0;
  char    _parampath[MED_NUMERICAL_DATA_GRP_SIZE+MED_NAME_SIZE+1]=MED_NUMERICAL_DATA_GRP;
  med_size _nstep=0;
  
  MED_VARGS_DECL(const, med_idt              , , fid         );
  MED_VARGS_DECL(const, char   *       , const , paramname   );
  MED_VARGS_DECL(, med_parameter_type *, const , paramtype   );
  MED_VARGS_DECL(, char *              , const , description );
  MED_VARGS_DECL(, char *              , const , dtunit      );
  MED_VARGS_DECL(, med_int *           , const , nstep       );
  MED_VARGS_DECL(, med_err *                  ,, fret         );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt              , , fid         );
  MED_VARGS_DEF(const, char   *       , const , paramname   );
  MED_VARGS_DEF(, med_parameter_type *, const , paramtype   );
  MED_VARGS_DEF(, char *              , const , description );
  MED_VARGS_DEF(, char *              , const , dtunit      );
  MED_VARGS_DEF(, med_int *           , const , nstep       );
  MED_VARGS_DEF(, med_err *                  ,, fret         );


  _MEDmodeErreurVerrouiller();

  /* On accede aux donnees */
  strcat(_parampath, paramname);
  if ((_paramidt = _MEDdatagroupOuvrir(fid,_parampath)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_parampath);
    goto ERROR;
  }

  /* on lit les infos : type, description, dtunit, nstep */
  if ( _MEDattrEntierLire(_paramidt,MED_NOM_TYP,&_paramtype) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(MED_NOM_TYP);
    goto ERROR;
  }
  *paramtype = (med_parameter_type) (_paramtype);

  if (_MEDattrStringLire(_paramidt,MED_NOM_DES,MED_COMMENT_SIZE, description) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(MED_NOM_DES);SSCRUTE(description);goto ERROR;
  }

  if (_MEDattrStringLire(_paramidt,MED_NOM_UNT,MED_SNAME_SIZE, dtunit) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(paramname);SSCRUTE(MED_NOM_UNT);
    SSCRUTE(dtunit);goto ERROR;
  }

  if ((_err=_MEDnObjects(_paramidt,".",&_nstep)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,paramname);
      goto ERROR;
    }
  *nstep = (med_int) _nstep;

  _ret = 0;
 ERROR:

  if (_paramidt > 0)            if (_MEDdatagroupFermer(_paramidt) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_parampath);
    ISCRUTE_id(_paramidt);
  }

  va_end(params);
  *fret = _ret;
  return;
}
