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
  \brief \MEDmeshNodeRdBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param switchmode \switchmode
  \param coordinate \coordinate
  \param withnodename \withnodename
  \param nodename \nodename
  \param withnodenumber \withnodenumber
  \param nodenumber \nodenumber
  \param withfamnumber \withfamnumber
  \param famnumber \famnumber
  \retval med_err \error
  \details \MEDmeshNodeRdDetails
 */

med_err MEDmeshNodeRd(const med_idt                  fid,
		      const char            * const  meshname,
		      const med_int                  numdt,
		      const med_int                  numit,
		      const med_switch_mode          switchmode,
		      med_float             * const  coordinate,
		      med_bool              * const  withnodename,
		      char                  * const  nodename,
		      med_bool              * const  withnodenumber,
		      med_int               * const  nodenumber,
		      med_bool              * const  withfamnumber,
		      med_int               * const  famnumber) {

  med_err           _ret       = -1;
  med_entity_type   _entitype  = MED_NODE;
  med_geometry_type _geotype   = MED_NONE;
  med_bool          _chgt=MED_FALSE, _trsf=MED_FALSE;

  *withnodename    = MED_FALSE;
  *withnodenumber  = MED_FALSE;
  *withfamnumber   = MED_FALSE;

  if ( (_ret = MEDmeshNodeCoordinateRd(fid,
				       meshname,
				       numdt,
				       numit,
				       switchmode,
				       coordinate) ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshNodeCoordinateRd");
    goto ERROR;
  }


  if ( MEDmeshnEntity(fid, meshname, numdt, numit,
		      _entitype,_geotype,MED_NAME, MED_NO_CMODE,
		      &_chgt, &_trsf) > 0) {
    *withnodename=MED_TRUE;
  }


  if ( *withnodename )
    if ( (_ret =  MEDmeshEntityNameRd(fid,
				      meshname,
				      numdt,
				      numit,
				      _entitype,
				      _geotype,
				      nodename) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityNameRd");
      goto ERROR;
    }

  if ( MEDmeshnEntity(fid, meshname, numdt, numit,
		      _entitype,_geotype,MED_NUMBER, MED_NO_CMODE,
		      &_chgt, &_trsf) > 0) {
    *withnodenumber=MED_TRUE;
  }


  if ( *withnodenumber )
    if ( (_ret =  MEDmeshEntityNumberRd(fid,
					meshname,
					numdt,
					numit,
					_entitype,
					_geotype,
					nodenumber) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityNumberRd");
      goto ERROR;
    }

  if ( MEDmeshnEntity(fid, meshname, numdt, numit,
		      _entitype,_geotype,MED_FAMILY_NUMBER, MED_NO_CMODE,
		      &_chgt, &_trsf) > 0) {
    *withfamnumber=MED_TRUE;
  }


  if (*withfamnumber)
    if ( (_ret =  MEDmeshEntityFamilyNumberRd(fid,
					      meshname,
					      numdt,
					      numit,
					      _entitype,
					      _geotype,
					      famnumber) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityFamilyNumberRd");
      goto ERROR;
    }

  _ret = 0;
 ERROR:
  return _ret;
}
