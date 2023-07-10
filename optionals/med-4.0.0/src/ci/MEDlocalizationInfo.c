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

/**\ingroup MEDlocalization
  \brief \MEDlocalizationInfoBrief
  \param fid \fid
  \param localizationit \localizationit
  \param localizationname \localizationname
  \param geotype \geotype
  \param spacedimension \spacedimension
  \param nipoint \nipoint
  \param geointerpname        \geointerpname
  \param sectionmeshname \sectionmeshname
  \param nsectionmeshcell \nsectionmeshcell
  \param sectiongeotype \sectiongeotype
  \retval med_err  \error
  \details \MEDlocalizationInfoDetails
  \par Définition
  \MEDlocalizationDef
  \remarks
  \MEDlocalizationRem
*/

#include <med.h>
#include <med_config.h>
#include <med_outils.h>

#include <string.h>
#include <stdlib.h>

med_err
MEDlocalizationInfo(const med_idt             fid,
		    const int                 localizationit,
		    char              * const localizationname,
		    med_geometry_type * const geotype,
		    med_int           * const spacedimension,
		    med_int           * const nipoint,
		    char *              const geointerpname,
		    char *              const sectionmeshname,
		    med_int           * const nsectionmeshcell,
		    med_geometry_type * const sectiongeotype)
{
  med_err _ret=-1;
  char    _path[MED_LOCALIZATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_LOCALIZATION_GRP;
  int     _num=localizationit-1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * ouverture du groupe /GAUSS/"nom"
   */
  if ( _MEDobjectGetName(fid, _path ,_num, localizationname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_path);
    ISCRUTE_int(localizationit);
    goto ERROR;
  }

  if ( MEDlocalizationInfoByName(fid, localizationname, geotype, spacedimension,
				 nipoint,geointerpname, 
				 sectionmeshname, nsectionmeshcell,sectiongeotype ) < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDlocalizationInfoByName");
    SSCRUTE(localizationname);
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  return _ret;
}
