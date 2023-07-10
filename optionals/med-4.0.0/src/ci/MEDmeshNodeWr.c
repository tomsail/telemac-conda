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
  \brief \MEDmeshNodeWrBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param dt \dt
  \param switchmode \switchmode
  \param nentity \nentity
  \param coordinate \coordinate
  \param withnodename \withnodename
  \param nodename \nodename
  \param withnodenumber \withnodenumber
  \param nodenumber \nodenumber
  \param withfamnumber \withfamnumber
  \param famnumber \famnumber
  \retval med_err \error
  \details \MEDmeshNodeWrDetails
 */

med_err MEDmeshNodeWr(const med_idt                  fid,
		      const char            * const  meshname,
		      const med_int                  numdt,
		      const med_int                  numit,
		      const med_float                dt,
		      const med_switch_mode          switchmode,
		      const med_int                  nentity,
		      const med_float       * const  coordinate,
		      const med_bool                 withnodename,
		      const char            * const  nodename,
		      const med_bool                 withnodenumber,
		      const med_int         * const  nodenumber,
		      const med_bool                 withfamnumber,
		      const med_int         * const  famnumber) {

  med_err           _ret       = -1;
  med_entity_type   _entitype  = MED_NODE;
  med_geometry_type _geotype   = MED_NONE;

  if ( (_ret = MEDmeshNodeCoordinateWr(fid,
				       meshname,
				       numdt,
				       numit,
				       dt,
				       switchmode,
				       nentity,
				       coordinate) ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshNodeCoordinateWr");
    goto ERROR;
  }

  if ( withnodename )
    if ( (_ret =  MEDmeshEntityNameWr(fid,
				      meshname,
				      numdt,
				      numit,
				      _entitype,
				      _geotype,
				      nentity,
				      nodename) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityNameWr");
      goto ERROR;
    }

  if ( withnodenumber )
    if ( (_ret =  MEDmeshEntityNumberWr(fid,
					meshname,
					numdt,
					numit,
					_entitype,
					_geotype,
					nentity,
					nodenumber) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityNumberWr");
      goto ERROR;
    }

  if (withfamnumber)
    if ( (_ret =  MEDmeshEntityFamilyNumberWr(fid,
					      meshname,
					      numdt,
					      numit,
					      _entitype,
					      _geotype,
					      nentity,
					      famnumber) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityFamilyNumberWr");
      goto ERROR;
    }

  _ret = 0;
 ERROR:
  return _ret;
}
