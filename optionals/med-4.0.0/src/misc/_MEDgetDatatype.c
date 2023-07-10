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
#include <string.h>

med_err _MEDgetDatatype(med_internal_type * const datatype, const med_data_type meddatatype, med_connectivity_mode cmode )
{
  med_err _ret=-1;
  switch(meddatatype)
    {
    case MED_COORDINATE :
    case MED_COORDINATE_AXIS1 :
    case MED_COORDINATE_AXIS2 :
    case MED_COORDINATE_AXIS3 :
    case MED_COORDINATE_TRSF :
      *datatype=MED_INTERNAL_FLOAT64;
      break;

    case MED_NAME :
      *datatype=MED_INTERNAL_SNAME;
      break;

    default :
      *datatype  = MED_INTERNAL_INT;
    }

  _ret = 0;
 ERROR:
  return _ret;
}

