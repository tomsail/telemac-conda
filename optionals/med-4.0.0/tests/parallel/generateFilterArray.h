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

#ifndef _GENERATE_FILTER_ARRAY_
#define _GENERATE_FILTER_ARRAY_


#include <med.h>
#define MESGERR 1

med_err generateFilterArray( const med_size nentities, const med_size nvaluesperentity, const med_size nconstituentpervalue,
			     const med_size profilearraysize, const med_int * const profilearray,
			     med_int  * const nentitiesfiltered, med_int **filterarray );

#endif
