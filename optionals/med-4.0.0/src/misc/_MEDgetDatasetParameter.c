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

/* TODO : Réfléchir à l'utiliser dans le module FIELD */
med_err _MEDgetDatasetParameter(const med_data_type         meddatatype,
				const med_int               spacedim,
				const med_entity_type       entitytype,
				const med_geometry_type     geotype,
				const med_connectivity_mode cmode,
				med_int * const             nvalueperentity,
				med_int * const             nconstituentpervalue)
{

  med_err            _ret=-1;
  med_int            _entdim=0,_nnoe=0,_ndes=0;

  *nvalueperentity=1;

  if ( _MEDgetGeometricParameter( entitytype, geotype, &_entdim, &_nnoe, &_ndes) <0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"_MEDparametresGeometrie");
    ISCRUTE_int(entitytype);ISCRUTE_int(geotype);ISCRUTE(_entdim);
    ISCRUTE(_nnoe);ISCRUTE(_ndes);goto ERROR;
  }

  switch(meddatatype)
    {
    case MED_COORDINATE :
      *nconstituentpervalue=spacedim;
      break;
    case MED_CONNECTIVITY :
      switch(cmode)
	{
	case MED_NODAL :
	  *nconstituentpervalue=_nnoe;
	  break;

	case MED_DESCENDING :
	  *nconstituentpervalue=_ndes;
	  break;

	default :
	  MED_ERR_(_ret,MED_ERR_RANGE,MED_ERR_CONNECTIVITYMODE,MED_ERR_VALUE_MSG);
	  ISCRUTE_int(cmode);goto ERROR;
	}
      break;
    default:
      *nconstituentpervalue=1;
    }

  _ret = 0;
 ERROR:
  return _ret;
}
