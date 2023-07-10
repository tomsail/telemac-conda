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


med_idt _MEDmeshDatagroupOpen(const med_idt               fid,
			      const char*  const          meshname,
			      char *       const          usedpath,
			      med_bool *   const          isasupportmesh
			     )
{

  med_idt  _ret=-1;
  med_idt  _meshid=0;
  char     __meshpath        [MED_MESH_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_GRP;
  char     __supmeshpath     [MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_SUPPORT_GRP;
  char*   _meshpath         =__meshpath;

  *isasupportmesh = MED_FALSE;

  strcat(_meshpath,meshname);
  if ((_meshid = _MEDdatagroupOuvrir(fid,_meshpath)) < 0) {
    strcat(__supmeshpath,meshname);
    if ((_meshid = _MEDdatagroupOuvrir(fid,__supmeshpath)) < 0) {
      MED_ERR_(_ret,MED_ERR_OPEN,MED_ERR_DATAGROUP,MED_ERR_MESH_MSG);
      SSCRUTE(meshname); goto ERROR;
    } else {
      _meshpath=__supmeshpath;
      *isasupportmesh = MED_TRUE;
    }
  }

  if (usedpath) strcat(usedpath,_meshpath);

  _ret=_meshid;
 ERROR:
  return _ret;

}
