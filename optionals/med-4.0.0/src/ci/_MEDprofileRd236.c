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

void _MEDprofileRd236(int dummy, ...) {


  med_idt        _pfid=0;
  med_err        _ret=-1;
  char           _path[MED_PROFILE_GRP_SIZE+MED_NAME_SIZE+1]=MED_PROFILE_GRP;
  med_filter     _filter        = MED_FILTER_INIT;
  med_int        _nentity=0;
  
  MED_VARGS_DECL(const, med_idt         , , fid          );
  MED_VARGS_DECL(const, char    * , const , profilename  );
  MED_VARGS_DECL(, med_int *, const , profilearray       );
  MED_VARGS_DECL(, med_err *       ,, fret               );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt         , , fid          );
  MED_VARGS_DEF(const, char    * , const , profilename  );
  MED_VARGS_DEF(, med_int *, const , profilearray       );
  MED_VARGS_DEF(, med_err *       ,, fret               );


  if ( MEDprofilLire( fid,profilearray, (char *) profilename) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDprofilLire");
    SSCRUTE(profilename);
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  va_end(params);
  *fret = _ret;

  return;
}
