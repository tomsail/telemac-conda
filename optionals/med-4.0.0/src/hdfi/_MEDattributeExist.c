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

med_err _MEDattributeExist(const med_idt        gid,
			   const char * const   datagroupname,
			   const char * const   attributename,
			   med_bool *   const   attributeexist )
{
  med_err        _ret=-1;


  _ret =  H5Aexists_by_name( gid, datagroupname, attributename, H5P_DEFAULT );

  if (_ret > 0)
    *attributeexist = MED_TRUE;
  else
    if (_ret == 0)
      *attributeexist = MED_FALSE;
    else {
      MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"H5Aexists_by_name");
      SSCRUTE(datagroupname);SSCRUTE(attributename);goto ERROR;
    }


  _ret = 0;
 ERROR:

  return _ret;
}
