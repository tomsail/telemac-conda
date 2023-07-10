/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2016  EDF R&D, CEA/DEN
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

med_err _MEDgetFieldEntityGeoTypeName(med_idt                 fid,
				      char * const            entitygeotypename,
				      const med_entity_type   entitytype,
				      const med_geometry_type geotype )
{
  med_err  _ret=-1;
  char     _geotypename       [MED_TAILLE_NOM_ENTITE+1]="";

  /*
   *  Creation/Ouverture du datagroup de niveau 2 <entitytype>[.<geotype>]
   */

  if (_MEDgetEntityTypeName(entitygeotypename,entitytype) < 0) {
    MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_ENTITY,MED_ERR_VALUE_MSG);
    ISCRUTE_int(entitytype);goto ERROR;
  }

  if ( entitytype != MED_NODE ) {

    if ( entitytype == MED_STRUCT_ELEMENT ) {
      if ( MEDstructElementName(fid, geotype,_geotypename) < 0 ) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDstructElementName");
	ISCRUTE_int(geotype);goto ERROR;
      }
    } else {
      if ( _MEDgetInternalGeometryTypeName(fid,_geotypename,geotype) < 0) {
	MED_ERR_(_ret,MED_ERR_INVALID,MED_ERR_GEOMETRIC,MED_ERR_VALUE_MSG);
	ISCRUTE_int(geotype);goto ERROR;
      }
    }
    strcat(entitygeotypename,".");
    strcat(entitygeotypename,_geotypename);
  }
  
  _ret=0;
  
 ERROR:
  return _ret;
}
