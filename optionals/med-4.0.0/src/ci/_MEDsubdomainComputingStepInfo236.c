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

void _MEDsubdomainComputingStepInfo236(int dummy, ...) {


  med_idt  _jntid=0;
  med_err  _ret=-1,_err=-1;
  char     _path[MED_MESH_GRP_SIZE+MED_TAILLE_NOM+MED_JOINT_GRP_SIZE+MED_TAILLE_NOM+1]=MED_MESH_GRP;
  int      _num;
  med_size _nocstpncorrespondence=0;


  MED_VARGS_DECL(const, med_idt      , , fid             );
  MED_VARGS_DECL(const, char * , const , meshname        );
  MED_VARGS_DECL(const, char * , const , jointname       );
  MED_VARGS_DECL(const, int          , , csit            );
  MED_VARGS_DECL(, med_int *, const    , numdt           );
  MED_VARGS_DECL(, med_int *, const    , numit           );
  MED_VARGS_DECL(, med_int *, const    , ncorrespondence );
  MED_VARGS_DECL(, med_err *          ,, fret            );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid             );
  MED_VARGS_DEF(const, char * , const , meshname        );
  MED_VARGS_DEF(const, char * , const , jointname       );
  MED_VARGS_DEF(const, int          , , csit            );
  MED_VARGS_DEF(, med_int *, const    , numdt           );
  MED_VARGS_DEF(, med_int *, const    , numit           );
  MED_VARGS_DEF(, med_int *, const    , ncorrespondence );
  MED_VARGS_DEF(, med_err *          ,, fret            );

  _num=csit-1;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

  if ( csit !=1 ) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_PARAMETER,"");ISCRUTE_int(csit);
    goto ERROR;
  }

  /*
   * On recupere le nom du joint
   */
  strcat(_path,meshname);
  strcat(_path,MED_JOINT_GRP);
  strcat(_path,jointname);

  if ((_jntid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_SUBDOMAINJOINT_MSG);
    SSCRUTE(_path);*ncorrespondence=0;
    goto ERROR;
  }

  /*
   * Nombre de correspondances dans le joint
   */

  if ((_err=_MEDnObjects(_jntid, _path, &_nocstpncorrespondence)) < 0 )
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_DATAGROUP,_path);
      goto ERROR;
    } else
      _nocstpncorrespondence =  0;

  *ncorrespondence = (med_int) _nocstpncorrespondence;


  *numdt=MED_NOPDT;
  *numit=MED_NONOR;

 _ret = 0;

 ERROR:

  if (_jntid>0)            if (_MEDdatagroupFermer(_jntid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_jntid);
  }

  va_end(params);
  *fret = _ret;
  return;
}
