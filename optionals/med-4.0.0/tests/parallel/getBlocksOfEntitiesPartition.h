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

#ifndef _GET_BLOCKS_OF_ENTITIES_PARTITION_H
#define _GET_BLOCKS_OF_ENTITIES_PARTITION_H

#include <med.h>
#define MESGERR 1
/* #include "med_utils.h" */


#include "getBlocksOfEntitiesPartition.h"

typedef void (*GetBlocksOfEntitiesType)(const int myrank, const int nproc, const int nentities,
				   med_size * const start, med_size * const stride, med_size * const count, med_size * blocksize,
				   int * const lastusedrank, med_size * const lastblocksize );

void getContinuousBlocksOfEntities(const int myrank, const int nproc, const int nentities,
				   med_size * const start, med_size * const stride, med_size * const count, med_size * blocksize,
				   int * const lastusedrank, med_size * const lastblocksize );
void getCyclicBlocksOfEntities(const int myrank, const int nproc, const int nentities,
				   med_size * const start, med_size * const stride, med_size * const count, med_size * blocksize,
				   int * const lastusedrank, med_size * const lastblocksize );

#endif




