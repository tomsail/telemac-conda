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

#include "med_versioned.h"

void _MEDmeshComputationStepInfo236(int dummy, ...) {


  med_err   _ret            = -1;
  int       _num;
  med_bool  _changement     = MED_FALSE;
  med_bool  _transformation = MED_FALSE;
  med_int   _n=0,_numdt=MED_NO_DT,_numit=MED_NO_IT;
  int       _dummy = 0;
  char      _profilename[MED_TAILLE_NOM+1]="";
  med_int   _profilesize=0;
  
  MED_VARGS_DECL(const, med_idt      , , fid         );
  MED_VARGS_DECL(const, char * , const , meshname    );
  MED_VARGS_DECL(const, int          , , csit        );
  MED_VARGS_DECL(, med_int *, const    , numdt       );
  MED_VARGS_DECL(, med_int *, const    , numit       );
  MED_VARGS_DECL(, med_float *, const  , dt          );
  MED_VARGS_DECL(, med_bool           ,, musthave1cs );
  MED_VARGS_DECL(, med_err *          ,, fret        );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid         );
  MED_VARGS_DEF(const, char * , const , meshname    );
  MED_VARGS_DEF(const, int          , , csit        );
  MED_VARGS_DEF(, med_int *, const    , numdt       );
  MED_VARGS_DEF(, med_int *, const    , numit       );
  MED_VARGS_DEF(, med_float *, const  , dt          );
  MED_VARGS_DEF(, med_bool           ,, musthave1cs );
  MED_VARGS_DEF(, med_err *          ,, fret        );

  _num            = csit - 1;

  if ( csit != 1 ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,"");
    ISCRUTE_int(csit);goto ERROR;
  }

  _MEDmeshnEntity236(_dummy,
		     fid,
		     meshname,
		     _numdt,
		     _numit,
		     MED_ALL_ENTITY_TYPE,
		     MED_ALL_GEOTYPE,
		     MED_UNDEF_DATATYPE,
		     MED_NO_CMODE,
		     MED_UNDEF_PFLMODE,
		     _profilename,
		     &_profilesize,
		     &_changement,
		     &_transformation, &_n );

  if (_n < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshnEntity236");
    SSCRUTE(meshname);ISCRUTE(_numdt);ISCRUTE(_numit);;
    ISCRUTE(_n);goto ERROR;
  }

  *numdt=MED_NO_DT;
  *numit=MED_NO_IT;
  *dt='\0';

  if (_n < 1 ) {
    if ( musthave1cs ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDmeshnEntity236");
      SSCRUTE(meshname);ISCRUTE(_numdt);ISCRUTE(_numit);
      ISCRUTE(_n);goto ERROR;
    } else  {
      /*Pas d'erreur affiché */
      _ret = MED_ERR_CALL+MED_ERR_API;goto ERROR;
    }
  }
  _ret = 0;
 ERROR:

  va_end(params);
  *fret = _ret;
  return;
}
