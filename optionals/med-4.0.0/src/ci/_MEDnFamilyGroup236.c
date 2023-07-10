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

void _MEDnFamilyGroup236(int dummy, ... )
{


  med_int       _ret=-1;
  med_int       _n=0;
  
  MED_VARGS_DECL(const, med_idt      , , fid      );
  MED_VARGS_DECL(const, char * , const , meshname );
  MED_VARGS_DECL(const, int          , , famit    );
  MED_VARGS_DECL(, med_int *          ,, fret     );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid      );
  MED_VARGS_DEF(const, char * , const , meshname );
  MED_VARGS_DEF(const, int          , , famit    );
  MED_VARGS_DEF(, med_int *          ,, fret     );

  if ( ( _n = MEDnGroupe(fid,(char *)meshname,famit) ) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDnGroupe");
    SSCRUTE(meshname);ISCRUTE_int(famit);
    goto ERROR;
  }

  _ret =  _n;
 ERROR:
  va_end(params);
  *fret = _ret;

  return;
}
