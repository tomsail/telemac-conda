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


med_err
_MEDgetComputationStepName(const med_sorting_type sortingtype, const med_int numdt,
			   const med_int numit, char * const datagroupname)
{

  med_err    _ret=-1;
  long _numdt = numdt;
  long _numit = numit;

  switch (sortingtype) {
  case MED_SORT_DTIT:
    sprintf(datagroupname,"%0*li%0*li",MED_MAX_PARA, _numdt,MED_MAX_PARA, _numit);
    break;
  case MED_SORT_ITDT:
    sprintf(datagroupname,"%0*li%0*li",MED_MAX_PARA, _numit,MED_MAX_PARA, _numdt);
    break;
  default:
    MED_ERR_(_ret,MED_ERR_UNRECOGNIZED,MED_ERR_PARAMETER,"");
    ISCRUTE_int(sortingtype);
    goto ERROR;
  }
  _ret=0;
 ERROR:
  return _ret;

}
