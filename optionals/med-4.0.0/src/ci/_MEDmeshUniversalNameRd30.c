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


#include <med_config.h>
#include <med.h>
#include <med_outils.h>

#include <string.h>
#include <stdlib.h>

#include <stdio.h>

void
_MEDmeshUniversalNameRd30(int dummy, ...)
{


  med_err _ret=-1;
  med_idt _meshid;
  char    _path [MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;


  MED_VARGS_DECL(const, med_idt      , , fid       );
  MED_VARGS_DECL(const, char * , const , meshname  );
  MED_VARGS_DECL(, char *, const , univname        );
  MED_VARGS_DECL(, med_err *    ,, fret            );

  va_list params;
  va_start(params,dummy);

  MED_VARGS_DEF(const, med_idt      , , fid       );
  MED_VARGS_DEF(const, char * , const , meshname  );
  MED_VARGS_DEF(, char *, const , univname        );
  MED_VARGS_DEF(, med_err *    ,, fret            );

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();


  /*
   * Si le maillage n'existe pas => erreur
   */
  strcat(_path,meshname);
  if ((_meshid = _MEDdatagroupOuvrir(fid,_path)) < 0) {
    MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_meshid);goto ERROR;
  }

  if ( _MEDattrStringLire(_meshid,MED_NOM_UNV,MED_LNAME_SIZE,univname) < 0) {
    _ret = MED_ERR_READ + MED_ERR_ATTRIBUTE;
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_path);
    ISCRUTE_id(_meshid);
  }
  va_end(params);
  *fret = _ret;
  return;
}
