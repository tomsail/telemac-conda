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

/**\ingroup MEDmesh
  \brief \MEDmeshnEntityBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param geotype \geotype
  \param datatype \datatype
  \param cmode \cmode
  \param changement \changement
  \param transformation \transformation
  \retval med_int \nent
  \details \MEDmeshnEntityDetails
  \remarks
  \htmlinclude MEDmeshnEntity.html
 */

med_int
MEDmeshnEntity(const med_idt fid,
	       const char * const meshname,
	       const med_int numdt,
	       const med_int numit,
	       const med_entity_type entitype,
	       const med_geometry_type geotype,
	       const med_data_type datatype,
	       const med_connectivity_mode cmode,
	       med_bool * const changement,
	       med_bool * const transformation )
{

  char                   _profilename[MED_NAME_SIZE+1]="";
  med_int                _profilesize=0;
  med_int                _ret=-1;


  _ret = _MEDmeshnEntity(fid,  meshname, numdt, numit,entitype,  geotype,
		       datatype, cmode, MED_GLOBAL_STMODE,_profilename, &_profilesize,
		       changement, transformation );

  if ( strlen(_profilename) || (_profilesize > 0) ) {
    MED_ERR_(_ret,MED_ERR_EXIST,MED_ERR_PROFILE,_profilename);
    goto ERROR;
  }

 ERROR:
  return _ret;
}
