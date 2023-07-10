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


#include "med_config.h"
#include <med.h>
#include "med_outils.h"
#include <string.h>

/**\ingroup MEDsupportMesh
  \brief \MEDsupportMeshInfoBrief
  \param fid \fid
  \param meshit \meshit
  \param supportmeshname \supportmeshname
  \param spacedim \spacedim
  \param meshdim \meshdim
  \param description \description
  \param axistype \axistype
  \param axisname \axisname
  \param axisunit \axisunit
  \retval med_err \error
  \details \MEDsupportMeshInfoDetails
 */

med_err MEDsupportMeshInfo(const med_idt            fid,
			   const int                meshit,
			   char   *           const supportmeshname,
			   med_int *          const spacedim,
			   med_int *          const meshdim,
			   char *             const description,
			   med_axis_type *    const axistype,
			   char *             const axisname,
			   char *             const axisunit)
{
  med_err  _ret=-1;
  char     _meshpath[MED_MESH_SUPPORT_GRP_SIZE+MED_NAME_SIZE+1]=MED_MESH_SUPPORT_GRP;
  int      _num = meshit -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On recupere le nom du champ
   */
  if ( _MEDobjectGetName(fid, _meshpath ,_num, supportmeshname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_meshpath);ISCRUTE_int(meshit);
    goto ERROR;
  }
  strcat(_meshpath,supportmeshname);

  if ( MEDsupportMeshInfoByName(fid,  supportmeshname, spacedim, meshdim ,
				 description, axistype, axisname, axisunit)  < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_MESH_MSG);
    SSCRUTE(supportmeshname);SSCRUTE(_meshpath);SSCRUTE("MEDsupportMeshInfoByName");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;
}
