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

med_err _MEDdatagroupExist(const med_idt        gid,
			   const char * const   datagroupname,
			   med_bool *   const   datagroupexist,
			   med_bool *   const   isasoftlink )
/*Pour l'instant, dans le modèle interne les liens sont uniquement des liens vers
  des datasets*/

{
  med_err        _ret=-1;
  H5L_info_t     _linkinfo;
  H5O_info_t     _oinfo;

  if ( H5Lget_info( gid, datagroupname,  &_linkinfo, H5P_DEFAULT ) >= 0 ) {

    switch ( _linkinfo.type ) {

    case H5L_TYPE_SOFT:
      *isasoftlink=MED_TRUE;
      _oinfo.type=H5G_LINK;
      break;

    case H5L_TYPE_HARD:
      *isasoftlink  = MED_FALSE;
      if ( H5Oget_info_by_name( gid, datagroupname, &_oinfo, H5P_DEFAULT ) <0) {
	MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"H5Oget_info_by_name");
	SSCRUTE( datagroupname);
      }
      break;

    case H5L_TYPE_EXTERNAL:
    case H5L_TYPE_ERROR:
    default:
      MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_HDFTYPE, datagroupname);
      ISCRUTE( _linkinfo.type);
      goto ERROR;
      break;
    }

    /*TODO : A vérifier sur un lien de datagroup, à mon avis ne fonctionne pas */
    switch ( _oinfo.type ) {
    case H5G_GROUP:
    case H5G_LINK:
      *datagroupexist = MED_TRUE;
      break;
    default:
      *datagroupexist = MED_FALSE;
    }

  } else {
      *datagroupexist = MED_FALSE;
      *isasoftlink    = MED_FALSE;
  }

  _ret = 0;
 ERROR:

  return _ret;
}
