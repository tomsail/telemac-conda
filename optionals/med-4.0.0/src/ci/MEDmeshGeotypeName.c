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
  \brief \MEDmeshGeotypeNameBrief
  \param fid \fid
  \param geotype \geotype
  \param geotypename \geotypename
  \return \error
  \details \MEDmeshGeotypeNameDetails
  \see  MEDstructElementName
  \see  MEDstructElementGeotype
 */

med_err
MEDmeshGeotypeName(const med_idt                 fid,
		   const med_geometry_type       geotype,
		   char *                  const geotypename) {

  med_err  _ret=-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  if( (geotype > MED_STRUCT_GEO_INTERNAL) && (geotype < MED_STRUCT_GEO_SUP_INTERNAL) ) {
    if ( MEDstructElementName(fid,geotype,geotypename) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDstructElementname");
      ISCRUTE_int(geotype);
      goto ERROR;
    }
  } else {
    if ( _MEDgetExternalGeometryTypeName(geotypename,geotype) < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDgetInternalGeometryTypeName");
      ISCRUTE_int(geotype);
      goto ERROR;
    }
  }
  _ret = 0;

 ERROR:

  return _ret;
}

