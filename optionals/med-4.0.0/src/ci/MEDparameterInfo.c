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


#include "med_config.h"
#include <med.h>
#include "med_outils.h"
#include <string.h>

/**\ingroup MEDparameter
  \brief \MEDparameterInfoBrief
  \param fid \fid
  \param paramit \paramit
  \param paramname \paramname
  \param paramtype \paramtype
  \param description \description
  \param dtunit \dtunit
  \param nstep \nstep
  \retval med_err  \error
  \details \MEDparameterInfoDetails
 */

med_err MEDparameterInfo(const med_idt            fid,
			 const int                paramit,
			 char   *             const paramname,
			 med_parameter_type * const paramtype,
			 char *               const description,
			 char *               const dtunit,
			 med_int *            const nstep)
{
  med_err  _ret = -1;
  char     _parampath[MED_NUMERICAL_DATA_GRP_SIZE+MED_NAME_SIZE+1] = MED_NUMERICAL_DATA_GRP;
  int      _num = paramit -1;

  _MEDmodeErreurVerrouiller();

  /* on recupere le nom du parametre */
  if ( _MEDobjectGetName(fid, _parampath ,_num, paramname) < 0 ) {
    MED_ERR_(_ret,MED_ERR_ACCESS,MED_ERR_DATAGROUP,_parampath);ISCRUTE_int(paramit);
    ISCRUTE_id(fid);
    goto ERROR;
  }
  strcat(_parampath,paramname);
  
  /* on lit le reste des infos */
  if ( MEDparameterInfoByName(fid, paramname, paramtype, description,
			     dtunit, nstep)  < 0) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,MED_ERR_PARAM_MSG);
    SSCRUTE(paramname);SSCRUTE(_parampath);SSCRUTE("MEDparameterInfoByName");
    goto ERROR;
  }

  _ret = 0;
 ERROR:

  return _ret;
}
