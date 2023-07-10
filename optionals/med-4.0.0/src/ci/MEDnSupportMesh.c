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
#include <string.h>
#include <stdlib.h>
#include <med_outils.h>

/**\ingroup MEDsupportMesh
  \brief \MEDnSupportMeshBrief
  \param fid      \fid
  \retval med_int \nmesh
  \return \error
  \details \MEDnSupportMeshDetails
 */

med_int
MEDnSupportMesh(const med_idt fid)
{
  med_size _n=0;
  med_int  _ret=-1,_err=-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();


  /*
   *  nombre de champs
   */
  if ((_err=_MEDnObjects(fid,MED_MESH_SUPPORT_GRP,&_n)) <0)
    if ( _err == (MED_ERR_COUNT + MED_ERR_DATAGROUP) ) {
      MED_ERR_(_ret,MED_ERR_COUNT,MED_ERR_MESH,MED_MESH_GRP);
      goto ERROR;
    }


  _ret = (med_int) _n;
 ERROR:
  return _ret;
}

