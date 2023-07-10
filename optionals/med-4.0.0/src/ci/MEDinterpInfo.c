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
#include <hdf5.h>
#include <string.h>

/**\ingroup MEDinterp
  \brief \MEDinterpInfoBrief
  \param fid           \fid
  \param interpit      \interpit
  \param interpname    \interpname
  \param geotype       \geotype
  \param cellnode      \cellnode
  \param nbasisfunc \nbasisfunc
  \param nvariable  \nvariable
  \param maxdegree     \maxdegree
  \param nmaxcoef      \nmaxcoef

  \return \error
  \details \MEDinterpInfoDetails
  \see     MEDinterpInfoByName

 */
med_err
MEDinterpInfo(const med_idt                 fid,
	      const int                      interpit,
	            char*              const interpname,
	            med_geometry_type* const geotype,
	            med_bool*          const cellnode,
	            med_int*           const nbasisfunc,
	            med_int*           const nvariable,
	            med_int*           const maxdegree,
	            med_int*           const nmaxcoef
	      )
{
  med_err  _ret=-1;
  char     _interppath[MED_INTERPOLATION_GRP_SIZE+MED_NAME_SIZE+1]=MED_INTERPOLATION_GRP;
  int      _num = interpit -1;

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On recupere le nom de l'interpolation
   */
  if ( _MEDobjectGetName(fid, _interppath ,_num, interpname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_interppath);ISCRUTE_int(interpit);
    goto ERROR;
  }
  strcat(_interppath,interpname);

  if (  MEDinterpInfoByName(fid,
			    interpname,
			    geotype,
			    cellnode,
			    nbasisfunc,
			    nvariable,
			    maxdegree,
			    nmaxcoef
			    )    < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_INTERP_MSG);
    SSCRUTE(interpname);SSCRUTE(_interppath);SSCRUTE("MEDinterpInfoByName");
    goto ERROR;
  }

  _ret = 0;

 ERROR:

  return _ret;
}
