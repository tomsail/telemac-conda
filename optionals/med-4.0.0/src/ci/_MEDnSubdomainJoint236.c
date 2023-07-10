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

void _MEDnSubdomainJoint236(int dummy, ...) {


  char             _path[MED_JOINT_GRP_SIZE+MED_NAME_SIZE+1]=MED_JOINT_GRP;
  med_int          _ret=-1,_err=-1;
  med_size         _tmpn=0;


  MED_VARGS_DECL(const, med_idt      , , fid       );
  MED_VARGS_DECL(const, char * , const , meshname  );
  MED_VARGS_DECL(, med_int *          ,, fret      );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid       );
  MED_VARGS_DEF(const, char * , const , meshname  );
  MED_VARGS_DEF(, med_int *          ,, fret      );

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();


  strcat(_path,meshname);

  /*
   *  nombre d'équivalence
   */
  if ( (_tmpn= MEDnJoint(fid, (char *) meshname)) <0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDnJoint");
    goto ERROR;
  }

  _ret = (med_int) _tmpn;
 ERROR:
  va_end(params);
  *fret = _ret;
  return;
}


