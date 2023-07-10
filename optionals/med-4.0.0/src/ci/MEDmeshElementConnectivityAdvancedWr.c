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
  \brief \MEDmeshElementConnectivityAdvancedWrBrief
  \param fid \fid
  \param meshname \meshname
  \param numdt \numdt
  \param numit \numit
  \param dt \dt
  \param entitype \entitype
  \param geotype \geotype
  \param cmode \cmode
  \param filter \filter
  \param connectivity \connectivity
  \retval med_err \error
  \details \MEDmeshElementConnectivityAdvancedWrDetails
 */

med_err MEDmeshElementConnectivityAdvancedWr(const med_idt               fid,
					     const char*  const          meshname,
					     const med_int               numdt,
					     const med_int               numit,
					     const med_float             dt,
					     const med_entity_type       entitype,
					     const med_geometry_type     geotype,
					     const med_connectivity_mode cmode,
					     const med_filter * const    filter,
					     const med_int* const  connectivity)
{
  med_err _ret=-1;
  char            _geotypename[MED_NAME_SIZE+1] = "";

if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  if ( entitype == MED_STRUCT_ELEMENT ) {

    /*Dans le cas particulier des particules,
      la connectivité est vide et l'utilisation de profils est interdite.*/
    if ( MEDstructElementName(fid, geotype,_geotypename) < 0 ) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDstructElementName");
      ISCRUTE_int(geotype);goto ERROR;
    }

    if ( !strcmp(_geotypename,"MED_PARTICLE") )
      if ( strlen((*filter).profilename ) ){
	MED_ERR_(_ret,MED_ERR_USE,MED_ERR_PROFILE,(*filter).profilename);
	goto ERROR;
      }
  }

  _ret = _MEDmeshAdvancedWr(fid,
			    meshname,
			    MED_CONNECTIVITY,
			    MED_NO_NAME,
			    MED_INTERNAL_UNDEF,
			    numdt,
			    numit,
			    dt,
			    entitype,
			    geotype,
			    cmode,
			    MED_UNDEF_STMODE,
			    MED_NO_PROFILE,
			    MED_UNDEF_INTERLACE,
			    MED_ALL_CONSTITUENT,
			    filter,
			    MED_UNDEF_SIZE,
			    connectivity);

 ERROR:
 return _ret;
}
