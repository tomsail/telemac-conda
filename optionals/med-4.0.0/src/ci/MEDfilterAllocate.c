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

/**\ingroup MEDfilter
   \brief \MEDfilterAllocateBrief
   \param nfilter \nfilter
   \return \filter \filterMEDfilterCm

   \details
   \MEDfilterAllocateDetails

   \par Remarques
   \MEDfilterAllocateNote
*/

med_filter* MEDfilterAllocate(const int nfilter) {

  med_err      _ret=0;
  med_filter * _filter=NULL;
  int          _i=0;
  med_idt      _memspace[MED_MAX_FILTER_SPACES]=MED_MAX_FILTER_SPACES_INIT;
  med_idt      _diskspace[MED_MAX_FILTER_SPACES]=MED_MAX_FILTER_SPACES_INIT;

  _filter=(med_filter*)calloc(nfilter,sizeof(med_filter));

  for (_i=0;_i<nfilter;++_i) {
    if ( _MEDsetFilter(MED_MAX_FILTER_SPACES,_memspace, _diskspace,
		       0, 0, 0, 0, MED_UNDEF_INTERLACE,
		       MED_NO_FILTER_SIZE, MED_NO_PROFILE_SIZE,
		       MED_UNDEF_STMODE, MED_NO_PROFILE, &_filter[_i] ) <0) {
      MED_ERR_(_ret,MED_ERR_INIT,MED_ERR_FILTER,"");
    }
    _filter[_i].nspaces=0;
  }


  return _filter;

}
