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
  \brief \MEDmeshNodeCoordinateWithProfileRdBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param storagemode \storagemode
  \param profilename \profilename
  \param switchmode \switchmode
  \param dimselect \dimselect
  \param coordinate \coordinate
  \retval med_err \error
  \details \MEDmeshNodeCoordinateWithProfileRdDetails
 */

med_err MEDmeshNodeCoordinateWithProfileRd(const med_idt               fid,
					   const char*  const          meshname,
					   const med_int               numdt,
					   const med_int               numit,
					   const med_storage_mode      storagemode,
					   const char * const          profilename,
					   const med_switch_mode       switchmode,
					   const med_int               dimselect,
					   med_float* const  coordinate)
{

 return _MEDmeshAdvancedRd(fid,
			   meshname,
			   MED_COORDINATE,
			   MED_NO_NAME,
			   MED_INTERNAL_UNDEF,
			   numdt,
			   numit,
			   MED_NODE,
			   MED_NONE,
			   MED_NO_CMODE,
			   storagemode,
			   profilename,
			   switchmode,
			   dimselect,
			   NULL,
			   (unsigned char * const) coordinate);

}
