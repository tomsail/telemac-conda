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
  \brief \MEDlibraryHdfStrVersionBrief
  \param hdfversion \hdfversion
  \retval med_err \error
  \details \MEDlibraryHdfStrVersionDetails
 */


med_err
MEDlibraryHdfStrVersion(char* const  hdfversion) {
  med_err _ret = -1;
  unsigned int _major, _minor,_release;

  if (H5get_libversion( &_major, &_minor,  &_release ) < 0) {
    MED_ERR_(_ret,MED_ERR_READ,MED_ERR_LIBRARY," HDF");
    goto ERROR;
  }

  if (hdfversion)
    if (sprintf(hdfversion,"HDF5-%u.%u.%u",_major,_minor,_release) < 0)
      goto ERROR;

  _ret=0;
 ERROR:

  return _ret;
}
