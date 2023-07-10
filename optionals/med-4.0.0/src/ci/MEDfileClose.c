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


/**\ingroup MEDfile
  \brief \MEDfileCloseBrief
  \param fid \fid
  \retval med_err \error
  \details \MEDfileCloseDetails
 */
med_err
MEDfileClose(med_idt fid)
{
  med_err _ret = -1;

#ifdef _DEBUG_
  _MEDobjetsOuverts(fid);
#endif

  _MEDmodeErreurVerrouiller();

  if (_MEDfichierFermer(fid) < 0) {
    H5Eprint1(stderr);
    MED_ERR_(_ret,MED_ERR_CLOSE,MED_ERR_FILE,"");
    goto ERROR;
  }

  _ret=0;
 ERROR:

  return _ret;
}
