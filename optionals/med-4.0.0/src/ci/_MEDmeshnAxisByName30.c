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

void
_MEDmeshnAxisByName30(int dummy, ...)
{

  med_int  _ret=0;
  med_idt _meshid=0;
  char    _meshpath[MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]="";
  char    _meshname[MED_NAME_SIZE+1]="";
  med_int _spacedim =-1;


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
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On regarde si le groupe existe => erreur si non
   */
  if (! isasupportmesh) {
    strcpy(_meshpath,MED_MESH_GRP);
    strcat(_meshpath,meshname);
    if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_meshpath); goto ERROR;
    }
  } else {
    strcpy(_meshpath,MED_MESH_SUPPORT_GRP);
    strcat(_meshpath,meshname);
    if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
      SSCRUTE(meshname);SSCRUTE(_meshpath); goto ERROR;
    }
  }

  /*
   * On va lire l'attribut dimension de l'espace
   */
  if (_MEDattrEntierLire(_meshid,MED_NOM_ESP,&_spacedim) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_ATTRIBUTE,MED_ERR_MESH_MSG);
    SSCRUTE(meshname);SSCRUTE(MED_NOM_ESP);goto ERROR;
  }

  _ret = _spacedim;
 ERROR:

  if (_meshid>0)            if (_MEDdatagroupFermer(_meshid) < 0) {
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_DATAGROUP,_meshpath);
    ISCRUTE_id(_meshid);
  }

  va_end(params);
  *fret = _ret;
  return;
}
