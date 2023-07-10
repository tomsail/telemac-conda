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

#include <2.3.6/med23v30.h>
#include <2.3.6/med23v30_proto.h>
#include "2.3.6/med23v30_misc.h"

void _MEDsubdomainCorrespondenceSizeInfo236(int dummy, ...) {


  med_err  _ret=-1;
  med_int  _nentitycor=0;


  MED_VARGS_DECL(const, med_idt              , , fid            );
  MED_VARGS_DECL(const, char * , const         , meshname       );
  MED_VARGS_DECL(const, char * , const         , jointname      );
  MED_VARGS_DECL(const, med_int              , , numdt          );
  MED_VARGS_DECL(const, med_int              , , numit          );
  MED_VARGS_DECL(const, int                  , , corit          );
  MED_VARGS_DECL(, med_entity_type   *, const  , localentitype  );
  MED_VARGS_DECL(, med_geometry_type *, const  , localgeotype   );
  MED_VARGS_DECL(, med_entity_type   *, const  , remoteentitype );
  MED_VARGS_DECL(, med_geometry_type *, const  , remotegeotype  );
  MED_VARGS_DECL(, med_int *, const            , nentitycor     );
  MED_VARGS_DECL(, med_err *                  ,, fret           );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt              , , fid            );
  MED_VARGS_DEF(const, char * , const         , meshname       );
  MED_VARGS_DEF(const, char * , const         , jointname      );
  MED_VARGS_DEF(const, med_int              , , numdt          );
  MED_VARGS_DEF(const, med_int              , , numit          );
  MED_VARGS_DEF(const, int                  , , corit          );
  MED_VARGS_DEF(, med_entity_type   *, const  , localentitype  );
  MED_VARGS_DEF(, med_geometry_type *, const  , localgeotype   );
  MED_VARGS_DEF(, med_entity_type   *, const  , remoteentitype );
  MED_VARGS_DEF(, med_geometry_type *, const  , remotegeotype  );
  MED_VARGS_DEF(, med_int *, const            , nentitycor     );
  MED_VARGS_DEF(, med_err *                  ,, fret           );

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

  if ( (numdt != MED_NO_DT) || (numit != MED_NO_IT) ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,"");
    ISCRUTE(numdt);ISCRUTE(numit);goto ERROR;
  }


  if ( MEDjointTypeCorres( fid, (char *) meshname, (char *) jointname,corit,
			   (med_entite_maillage * ) localentitype , (med_geometrie_element*) localgeotype,
			   (med_entite_maillage * ) remoteentitype, (med_geometrie_element*) remotegeotype) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDjointTypeCorres");
    SSCRUTE(meshname);SSCRUTE(jointname);
    goto ERROR;
  }

  if ( (_nentitycor=MEDjointnCorres(fid, (char *) meshname, (char *) jointname,
				    (med_entite_maillage ) *localentitype , (med_geometrie_element) *localgeotype,
				    (med_entite_maillage ) *remoteentitype, (med_geometrie_element) *remotegeotype) ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDjointnCorres");
    SSCRUTE(meshname);SSCRUTE(jointname);
    goto ERROR;
  }
  *nentitycor = _nentitycor;
/*   ISCRUTE(*nentitycor); */

  _ret = 0;
 ERROR:

  va_end(params);
  *fret = _ret;
  return;
}
