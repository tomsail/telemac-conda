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

#ifndef _GENERATE_DATAS_H_
#define _GENERATE_DATAS_H_

#include <med.h>
#define MESGERR 1

/* #include <string.h> */
/* #include <unistd.h> */

typedef void (*GenerateDataType)(const int myrank, const int lastrank, const int sizeoftype,
			const med_storage_mode profilemode, const med_size profilesize, const med_int * const profilearray,
			const med_size start, const med_size stride, const med_size count, const med_size blocksize, const med_size lastblocksize,
			const int nentities, const int nvaluesperentity, const int nconstituentpervalue,
			med_float ** valuesarray );

/*Les données générées, le sont uniquement aux endroits utilisés */
void generateFullIDatas(const int myrank, const int lastrank, const int sizeoftype,
			const med_storage_mode profilemode, const med_size profilesize, const med_int * const profilearray,
			const med_size start, const med_size stride, const med_size count, const med_size blocksize, const med_size lastblocksize,
			const int nentities, const int nvaluesperentity, const int nconstituentpervalue,
			med_float ** valuesarray );

void generateNoIDatas(const int myrank, const int lastrank,  const int sizeoftype,
		      const med_storage_mode storagemode, const med_size profilearraysize, const med_int * const profilearray,
		      const med_size start, const med_size stride, const med_size count, const med_size blocksize, const med_size lastblocksize,
		      const int nentities, const int nvaluesperentity, const int nconstituentpervalue,
		      med_float ** valuesarray );

#endif
