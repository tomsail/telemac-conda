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
#include "med_config.h"
#include "med_outils.h"

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

#include <string.h>
#include <stdlib.h>

void
_MEDmeshnAxisByName236(int dummy, ...)
{

  med_err            _ret     = -1;
  med_int            _nmaa    =0;
  med_int            _meshdim =0, _spacedim=0;
  med_maillage       _type;
  char               _meshname[MED_TAILLE_NOM+1] ="";
  int                _it       =0;
  char               _desc    [MED_TAILLE_DESC+1]="";


  MED_VARGS_DECL(const, med_idt      , , fid       );
  MED_VARGS_DECL(const, char * , const , meshname  );
  MED_VARGS_DECL(const, med_bool     , , isasupportmesh );
  MED_VARGS_DECL(, med_int *          ,, fret      );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid       );
  MED_VARGS_DEF(const, char * , const , meshname  );
  MED_VARGS_DEF(const, med_bool     , , isasupportmesh );
  MED_VARGS_DEF(, med_int *          ,, fret      );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * En 2.3, on ne traite que les maillages de calcul
   * Dans le cas d'un maillage support => erreur
   */
  if (isasupportmesh) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,MED_ERR_MESH_MSG);
    ISCRUTE(isasupportmesh);
    goto ERROR;
  }

  if ( strlen(meshname) > MED_TAILLE_NOM ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_RANGE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);ISCRUTE_int(MED_TAILLE_NOM);
    goto ERROR;
  }


  if ( (_nmaa =MEDnMaa(fid) ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE("MEDnMaa");
    goto ERROR;
  }

  for (_it=0; _it< _nmaa; ++_it ) {


    if ( MEDmaaInfo(fid, _it+1, _meshname, &_meshdim,  &_type, _desc) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_MESH_MSG);
      SSCRUTE(_meshname);SSCRUTE("MEDmaaInfo");
      ISCRUTE_int(_it);ISCRUTE(_meshdim);ISCRUTE_int(_type);SSCRUTE(_desc);
      goto ERROR;
    }

    if ( !strcmp(_meshname,meshname) ) {

      if ( (_spacedim = MEDdimEspaceLire(fid, _meshname)) < 0  )_spacedim=_meshdim;

      break;
    }

  }
  _ret = _spacedim;

 ERROR:

  va_end(params);
  *fret = _ret;
  return;
}
