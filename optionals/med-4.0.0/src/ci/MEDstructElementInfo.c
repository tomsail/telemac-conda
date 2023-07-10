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

#include <stdlib.h>
#include <string.h>

/**\ingroup MEDstructElement
  \brief \MEDstructElementInfoBrief
  \param fid                   \fid
  \param mit                    \mit
  \param modelname             \modelname
  \param mgeotype              \mgeotype
  \param modeldim              \modeldim
  \param supportmeshname       \supportmeshname
  \param sentitytype           \sentitytype
  \param snnode             \snnode
  \param sncell             \sncell
  \param sgeotype              \sgeotype
  \param nconstantattribute \nconstantattribute
  \param anyprofile            \anyprofile
  \param nvariableattribute \nvariableattribute

  \return \error

  \details \MEDstructElementInfoDetails
  \see MEDnStructElement
  \sa  MEDstructElementInfo
 */

med_err
MEDstructElementInfo(const med_idt             fid,
		     const int                 mit,
		     char *              const modelname,
		     med_geometry_type * const mgeotype,
		     med_int*            const modeldim,
		     char*               const supportmeshname,
		     med_entity_type*    const sentitytype,
		     med_int*            const snnode,
		     med_int*            const sncell,
		     med_geometry_type*  const sgeotype,
		     med_int*            const nconstantattribute,
		     med_bool*           const anyprofile,
		     med_int*            const nvariableattribute
			   )
{

  med_err  _ret=-1;
  char     _path[MED_ELSTRUCT_GRP_SIZE+MED_NAME_SIZE+1]=MED_ELSTRUCT_GRP;
  int      _num = mit -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On recupere le nom de l'élément de structure
   */
  if ( _MEDobjectGetName(fid, _path ,_num, modelname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);ISCRUTE_int(mit);
    goto ERROR;
  }
  strcat(_path,modelname);

  if (
      MEDstructElementInfoByName(fid,
				 modelname,
				 mgeotype,
				 modeldim,
				 supportmeshname,
				 sentitytype,
				 snnode,
				 sncell,
				 sgeotype,
				 nconstantattribute,
				 anyprofile,
				 nvariableattribute
				 ) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_STRUCT_MSG);
    SSCRUTE(modelname);SSCRUTE(_path);SSCRUTE("MEDstructElementInfoByName");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;
}
