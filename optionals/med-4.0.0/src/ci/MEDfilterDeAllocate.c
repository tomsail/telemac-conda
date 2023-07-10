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

/**\ingroup MEDfilter
   \brief \MEDfilterDeAllocateBrief
   \param nfilter \nfilter
   \retval filter \filter
   \return \error
   \details
   \details \MEDfilterDeAllocateDetails
   \par Remarques
   \MEDfilterDeAllocateNote
*/

#include <med.h>
#include <med_config.h>
#include <med_outils.h>

med_err MEDfilterDeAllocate(const int nfilter, med_filter * filter) {

  med_err _ret =0;
  int          _i=0;

  for (_i=0;_i<nfilter;++_i)
    _ret=_ret | MEDfilterClose(&filter[_i]);

  free(filter);

 ERROR:
  return _ret;

}
