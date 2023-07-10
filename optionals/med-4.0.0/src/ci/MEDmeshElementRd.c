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
  \brief \MEDmeshElementRdBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param entitype \entitype
  \param geotype \geotype
  \param cmode \cmode
  \param switchmode \switchmode
  \param connectivity \connectivity
  \param withelementname \withelementname
  \param elementname \elementname
  \param withelementnumber \withelementnumber
  \param elementnumber \elementnumber
  \param withfamnumber \withfamnumber
  \param famnumber \famnumber
  \retval med_err \error
  \details \MEDmeshElementRdDetails
 */


med_err MEDmeshElementRd(const med_idt                  fid,
		      const char            * const  meshname,
		      const med_int                  numdt,
		      const med_int                  numit,
		      const med_entity_type          entitype,
		      const med_geometry_type        geotype,
		      const med_connectivity_mode    cmode,
		      const med_switch_mode          switchmode,
		      med_int               * const  connectivity,
		      med_bool              * const  withelementname,
		      char                  * const  elementname,
		      med_bool              * const  withelementnumber,
		      med_int               * const  elementnumber,
		      med_bool              * const  withfamnumber,
		      med_int               * const  famnumber) {

  med_err           _ret       = -1;
  med_bool          _chgt=MED_FALSE, _trsf=MED_FALSE;

  *withelementname   =MED_FALSE;
  *withelementnumber =MED_FALSE;
  *withfamnumber     =MED_FALSE;

  if ( (_ret = MEDmeshElementConnectivityRd(fid,
					    meshname,
					    numdt,
					    numit,
					    entitype,
					    geotype,
					    cmode,
					    switchmode,
					    connectivity) ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshElementConnectivityRd");
    goto ERROR;
  }


  if ( MEDmeshnEntity(fid, meshname, numdt, numit,
		      entitype,geotype,MED_NAME, cmode,
		      &_chgt, &_trsf) > 0) {
    *withelementname=MED_TRUE;
  }

  if ( *withelementname )
    if ( (_ret =  MEDmeshEntityNameRd(fid,
				      meshname,
				      numdt,
				      numit,
				      entitype,
				      geotype,
				      elementname) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityNameRd");
      goto ERROR;
    }

  if ( MEDmeshnEntity(fid, meshname, numdt, numit,
		      entitype,geotype,MED_NUMBER, cmode,
		      &_chgt, &_trsf) > 0) {
    *withelementnumber=MED_TRUE;
  }


  if ( *withelementnumber )
    if ( (_ret =  MEDmeshEntityNumberRd(fid,
					meshname,
					numdt,
					numit,
					entitype,
					geotype,
					elementnumber) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityNumberRd");
      goto ERROR;
    }

  if ( MEDmeshnEntity(fid, meshname, numdt, numit,
		      entitype,geotype,MED_FAMILY_NUMBER, cmode,
		      &_chgt, &_trsf) > 0) {
    *withfamnumber=MED_TRUE;
  }

  if (*withfamnumber)
    if ( (_ret =  MEDmeshEntityFamilyNumberRd(fid,
					      meshname,
					      numdt,
					      numit,
					      entitype,
					      geotype,
					      famnumber) ) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityFamilyNumberRd");
      goto ERROR;
    }

  _ret = 0;
 ERROR:
  return _ret;
}
