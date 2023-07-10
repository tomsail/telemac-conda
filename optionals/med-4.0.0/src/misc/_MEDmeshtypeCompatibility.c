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

med_err _MEDmeshtypeCompatibility(const med_data_type meddatatype,
				  const med_mesh_type meshtype )
{
  med_err _ret=-1;

  switch(meddatatype)
    {
    case MED_COORDINATE_AXIS1 :
    case MED_COORDINATE_AXIS2 :
    case MED_COORDINATE_AXIS3 :
      if ( meshtype == MED_UNSTRUCTURED_MESH ) {
	MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_MEDDATATYPE,MED_ERR_VALUE_MSG);
	ISCRUTE_int(meddatatype);ISCRUTE_int(meshtype);goto ERROR;
      }
      break;

    case MED_CONNECTIVITY :
    case MED_INDEX_NODE:
    case MED_INDEX_FACE:
    case MED_VARIABLE_ATTRIBUTE:
    case MED_COORDINATE_TRSF :
      if ( meshtype == MED_STRUCTURED_MESH ) {
	MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_MEDDATATYPE,MED_ERR_VALUE_MSG);
	ISCRUTE_int(meddatatype);ISCRUTE_int(meshtype);goto ERROR;
      }
      break;


/*     case MED_COORDINATE : */
/*     case MED_NAME : */
/*     case MED_NUMBER : */
/*     case MED_FAMILY : */
    default :
      _ret = 0;
    }

  _ret = 0;
 ERROR:
  return _ret;
}

