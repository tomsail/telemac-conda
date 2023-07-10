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

void
_MEDequivalenceInfo236(int dummy, ...)
{


  med_err  _ret=-1,_err=-1;
  char      _path[MED_MESH_GRP_SIZE+MED_EQUIVALENCE_GRP_SIZE+2*MED_TAILLE_NOM+1]=MED_MESH_GRP;
  med_int  _ncor=0;
  med_size _nocstpncorrespondence=0;

  MED_VARGS_DECL(const, med_idt      , , fid                    );
  MED_VARGS_DECL(const, char * , const , meshname               );
  MED_VARGS_DECL(const, int          , , equivit                );
  MED_VARGS_DECL(, char *, const       , equivname              );
  MED_VARGS_DECL(, char *, const       , equivdescription       );
  MED_VARGS_DECL(, med_int *, const    , nstep                  );
  MED_VARGS_DECL(, med_int *, const    , nocstpncorrespondence  );
  MED_VARGS_DECL(, med_err *                     ,, fret        );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid                    );
  MED_VARGS_DEF(const, char * , const , meshname               );
  MED_VARGS_DEF(const, int          , , equivit                );
  MED_VARGS_DEF(, char *, const       , equivname              );
  MED_VARGS_DEF(, char *, const       , equivdescription       );
  MED_VARGS_DEF(, med_int *, const    , nstep                  );
  MED_VARGS_DEF(, med_int *, const    , nocstpncorrespondence  );
  MED_VARGS_DEF(, med_err *                     ,, fret        );
  if (  MEDequivInfo(fid, (char *) meshname, equivit, equivname, equivdescription) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDequivInfo");
    SSCRUTE(meshname); goto ERROR;
  }

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  strcat(_path,meshname);
  strcat(_path,MED_EQUIVALENCE_GRP);
  strcat(_path,equivname);

  if ((_err=_MEDnObjects(fid,_path,&_nocstpncorrespondence)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_CORRESPONDENCE,_path);
      goto ERROR;
    }

  *nocstpncorrespondence = (med_int) _nocstpncorrespondence;
  if (_nocstpncorrespondence > 0 )
    *nstep                 = 1;
  else
    *nstep                 = 0;


 _ret = 0;

 ERROR:

  va_end(params);
  *fret = _ret;

  return;
}


