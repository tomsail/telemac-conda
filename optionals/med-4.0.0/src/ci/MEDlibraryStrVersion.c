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


#include <stdio.h>

#include <med.h>
#include <med_config.h>
#include <med_outils.h>

/**\ingroup MEDlibrary
  \brief \MEDlibraryStrVersionBrief
  \param medversion \medversion
  \retval med_err \error
  \details \MEDlibraryStrVersionDetails
 */

med_err 
MEDlibraryStrVersion(char* const medversion)
{
  med_err _ret = -1;

  if (medversion) 
    if (sprintf(medversion,"MED-%d.%d.%d",MED_MAJOR_NUM,MED_MINOR_NUM,MED_RELEASE_NUM) < 0) {
      goto ERROR;
    }

  _ret=0;
 ERROR:

  return _ret;
}
