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
  \brief \MEDsupportMeshInfoByNameBrief
  \param fid \fid
  \param supportmeshname \supportmeshname
  \param spacedim \spacedim
  \param meshdim \meshdim
  \param description \description
  \param axistype \axistype
  \param axisname \axisname
  \param axisunit \axisunit
  \retval med_err \error
  \details \MEDsupportMeshInfoByNameDetails
 */

med_err
MEDsupportMeshInfoByName(const med_idt         fid,
			 const char *    const supportmeshname,
			 med_int *       const spacedim,
			 med_int *       const meshdim,
			 char *          const description,
			 med_axis_type * const axistype,
			 char *          const axisname,
			 char *          const axisunit)
{
  med_err          _ret=-1;
  char             _dtunit[MED_SNAME_SIZE+1]="";
  med_sorting_type _sortingtype=MED_SORT_UNDEF;
  med_mesh_type    _meshtype = MED_UNDEF_MESH_TYPE;
  med_int          _nstep=0;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();


  if ( _MEDmeshInfoByName(fid, MED_MESH_SUPPORT_GRP, supportmeshname, spacedim, meshdim, &_meshtype,
			  description, _dtunit, &_sortingtype, &_nstep,
			  axistype,  axisname, axisunit)  < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_MESH_MSG);
    SSCRUTE(supportmeshname);SSCRUTE("MEDmeshInfoByName");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;
}
