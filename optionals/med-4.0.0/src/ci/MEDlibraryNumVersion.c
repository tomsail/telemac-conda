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

/**\ingroup MEDlibrary
  \brief \MEDlibraryNumVersionBrief
  \param major \major
  \param minor \minor
  \param release \release
  \retval med_err \error
  \details \MEDlibraryNumVersionDetails
 */

med_err
MEDlibraryNumVersion(med_int* const major, 
		     med_int* const minor, 
		     med_int* const release) 
{
  *major = MED_MAJOR_NUM;
  *minor = MED_MINOR_NUM;
  *release = MED_RELEASE_NUM;

  return 0;
}
