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

med_err _MEDgetSupportMeshNbOfEntities(med_idt                   fid,
				       const char *  const       smeshname,
				       med_entity_type * const   smeshentitype,
				       med_geometry_type * const smeshgeotype,
				       char * const              smeshgeotypename,
				       med_int * const           smeshnentity) {

  med_err           _ret  = -1;
  med_entity_type   _smeshentitype   = MED_CELL;
  med_geometry_type _geotype         = MED_UNDEF_GEOTYPE;
  char              _geotypename[MED_TAILLE_NOM_ENTITE+1]="";
  med_data_type     _conorcoo             = MED_CONNECTIVITY;
  med_bool          _chgt=MED_FALSE,_trsf = MED_FALSE;
  med_int           _ngeotype=0, _smeshnentity=0;

  if ( (_ngeotype = MEDmeshnEntity(fid,smeshname,MED_NO_DT,MED_NO_IT,
				   _smeshentitype,MED_ALL_GEOTYPE,_conorcoo,MED_NODAL,
				   &_chgt,&_trsf) )  <= 0) {
    _smeshentitype = MED_NODE;
    _conorcoo      = MED_COORDINATE;
    if ( (_ngeotype = MEDmeshnEntity(fid,smeshname,MED_NO_DT,MED_NO_IT,
				     _smeshentitype,MED_ALL_GEOTYPE,_conorcoo,MED_NODAL,
				     &_chgt,&_trsf) )  < 0) {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
      SSCRUTE(smeshname);ISCRUTE_int(_smeshentitype);ISCRUTE(_ngeotype);goto ERROR;
    }
  }

  if ( (_ngeotype != 1 ) ) {
    MED_ERR_(_ret,MED_ERR_RANGE, MED_ERR_SUPPORT_MESH, smeshname);
    ISCRUTE_int(_smeshentitype);ISCRUTE(_ngeotype); goto ERROR;
  }

  if ( MEDmeshEntityInfo( fid,smeshname,MED_NO_DT,MED_NO_IT,
			  _smeshentitype, 1, _geotypename, &_geotype ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshEntityInfo");
    SSCRUTE(_geotypename);ISCRUTE_int(_geotype);goto ERROR;
  }

  if ( (_smeshnentity = MEDmeshnEntity(fid,smeshname,MED_NO_DT,MED_NO_IT,
				       _smeshentitype,_geotype,_conorcoo,MED_NODAL,
					   &_chgt,&_trsf) )  < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDmeshnEntity");
    ISCRUTE(_smeshnentity);goto ERROR;
  }

  if (smeshentitype) *smeshentitype= _smeshentitype;
  if (smeshnentity)  *smeshnentity = _smeshnentity;
  if (smeshgeotype)  *smeshgeotype = _geotype;
  if (smeshgeotypename) {strncpy(smeshgeotypename,_geotypename,MED_NAME_SIZE+1);
    smeshgeotypename[MED_NAME_SIZE]='\0';
  }

  _ret = 0;
 ERROR:
  return _ret;
}
