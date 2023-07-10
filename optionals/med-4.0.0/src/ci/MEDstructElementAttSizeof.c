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
#include <stdlib.h>

/**\ingroup MEDstructElement
  \brief \MEDstructElementAttSizeofBrief
  \param atttype          \atttype
  \retval int \sizeofatttype
  \return \error

  \details \MEDstructElementAttSizeofDetails
 */

int
MEDstructElementAttSizeof( const med_attribute_type atttype ) {

  switch (atttype) {

  case  MED_ATT_FLOAT64 :
    return sizeof(med_float);
    break;

  case MED_ATT_INT :
    return sizeof(med_int);
    break;

  case MED_ATT_NAME :
    return MED_NAME_SIZE*sizeof(char);
    break;

  default:
    return -1;
    break;
  }

  return -1;
}
