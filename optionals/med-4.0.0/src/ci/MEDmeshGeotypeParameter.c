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

/**\ingroup MEDmesh
  \brief \MEDmeshGeotypeParameterBrief
  \param fid \fid
  \param geotype \geotype
  \param geodim \geodim
  \param nnode \nnode
  \return \error
  \details \MEDmeshGeotypeParameterDetails
 */

med_err
MEDmeshGeotypeParameter(const med_idt                 fid,
			const med_geometry_type       geotype,
			med_int *               const geodim,
			med_int *               const nnode) {

  med_err  _ret=  - 1;
  med_int  _nndes = 0;
  med_int  _ncells= 0;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  if( (geotype > MED_STRUCT_GEO_INTERNAL) && (geotype < MED_STRUCT_GEO_SUP_INTERNAL) ) {
    if ( _MEDgetDynGeometricParameter(fid,MED_STRUCT_ELEMENT,geotype,geodim,nnode,&_ncells) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetDynGeometricParameter");
      ISCRUTE_int(geotype);
      goto ERROR;
    }
  } else {
    if ( _MEDgetGeometricParameter(MED_CELL,geotype,geodim,nnode,&_nndes) < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetGeometricParameter");
      ISCRUTE_int(geotype);
      goto ERROR;
    }
  }
  _ret = 0;

 ERROR:

  return _ret;
}

