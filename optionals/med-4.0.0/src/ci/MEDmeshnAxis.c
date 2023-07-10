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

/**\ingroup MEDmesh
  \brief \MEDmeshnAxisBrief
  \param fid \fid
  \param meshit \meshit
  \retval med_int \naxis
  \details \MEDmeshnAxisDetails
 */

med_int
MEDmeshnAxis(const med_idt fid,  const int meshit)
{
  med_int  _ret=0;
  char     _meshpath[MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  char     _meshname[MED_NAME_SIZE+1]="";
  int      _num      = meshit-1;

  /*
   * On inhibe le gestionnaire d'erreur
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On recupere le nom du maillage
   */
  if ( _MEDobjectGetName(fid, _meshpath ,_num, _meshname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_meshpath);ISCRUTE_int(_num);
    goto ERROR;
  }

  if ( (_ret=MEDmeshnAxisByName( fid, _meshname )) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_MESH_MSG);
    SSCRUTE(_meshname);SSCRUTE(_meshpath);SSCRUTE("MEDmeshnAxisByName");
    goto ERROR;
  }

 ERROR:

  return _ret;
}
