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
#define MESGERR 1
#include "med_utils.h"


#include "getBlocksOfEntitiesPartition.h"


void getContinuousBlocksOfEntities(const int myrank, const int nproc, const int nentities,
				    med_size * const start, med_size * const stride, med_size * const count, med_size * blocksize,
				    int * const lastusedrank, med_size * const lastblocksize ) {

    int      _nusedproc      = nproc;
    int      _lastusedrank   = 0;
    med_size _blocksize      = nentities/nproc;
    /* _nblocks_pproc vaut 1 ou 0 si l'on utilise pas tous les processus */
    int      _nblocks_pproc  = 0;


    /*Tant que la taille des blocks est nulle on diminue le
      nombre de processus utilisé jusqu'au minimum d'un processus
    */
    for (; (_blocksize < 1) && ( _nusedproc > 1 ) ; ) {
      SSCRUTE("NOT USING ALL PROCESS");
      --_nusedproc;
      _blocksize = nentities/_nusedproc;
    }
    _lastusedrank   = _nusedproc-1;

    if ( myrank < _nusedproc)
      _nblocks_pproc = 1;
    else
      _blocksize = 0;

/*     if ( _blocksize == 0 ) { */
/*       if (myrank == 0 ) { _nblocks_pproc=1;_blocksize=nentities;} */
/*       _lastusedrank = 0; */
/*       _nusedproc = 1; */
/*       _blocksize    = nentities; */ /*TODO : essayer de l'enlever maintenant : Ajouté pour symétrie des opération MPI_File, *count == 0*/
/*     } else { */
/*       _nblocks_pproc = 1; */
/*     } */

    *start         = myrank*_nblocks_pproc*_blocksize;
    *stride        = _blocksize;
    *count         = _nblocks_pproc;
    *lastblocksize = 0;

    if ( myrank == _lastusedrank ) {
      *blocksize = nentities+_blocksize*(1-_nusedproc);
    } else {
      *blocksize =_blocksize;
    }
    ++(*start);
    *lastusedrank=_lastusedrank;
    printf("My rank %d , start %l , stride %l , blocksize %l , count %l , lastblocksize %l\n",
            myrank,*start,*stride,*blocksize,*count,*lastblocksize);
    return;
}

void getCyclicBlocksOfEntities(const int myrank, const int nproc, const int nentities,
			       med_size * const start, med_size * const stride,  med_size * const io_count, med_size * blocksize,
			       int * const lastusedrank, med_size * const lastblocksize ) {

    int      _nusedproc      = nproc;
    int      _lastusedrank   = nproc-1;
    int      _nblocks_pproc  = *io_count;
    int      _nblocks        = _nblocks_pproc*nproc;
    med_size _blocksize      = 0;

    if (_nblocks) _blocksize=nentities/_nblocks;

    /*Tant que la taille des block est nulle on diminue le
     nombre de blocks affecté par processus jusqu'au minimum
     d'un block par processus
    */
    for (; (_blocksize < 1) && ( _nblocks_pproc > 1 ) ; ) {
      --_nblocks_pproc;
      _nblocks   = _nblocks_pproc*nproc;
      _blocksize = nentities/_nblocks;
    }

/*     ISCRUTE(_nblocks_pproc); */
/*     ISCRUTE(_blocksize); */

    /*Si la taille des blocks est toujours nulle,
      c'est qu'il y a trop de processus pour le nombre d'entités :
      -> On effectue alors une répartition par block contigüs qui prend
      en compte la possible non affectation de certains processus.
    */
    if ( _blocksize == 0 ) {
      MESSAGE("Downcasting getCyclicBlocksOfEntities to getContinuousBlocksOfEntities");
      getContinuousBlocksOfEntities(myrank, nproc, nentities,
				    start, stride, io_count, blocksize, lastusedrank, lastblocksize );
      return;
    }

    /* A partir d'ici on est en mesure de calculer une répartition cyclique par block*/
    *blocksize     = _blocksize;
    *stride        = _blocksize*nproc;
    *start         = _blocksize*myrank;
    *io_count      = _nblocks_pproc;

    if (myrank == _lastusedrank) {
      *lastblocksize = nentities + _blocksize*(1-nproc*_nblocks_pproc);
      /*Dans le cas _nblocks_pproc==1 on a en fait une répartition contigüe des blocks
       lastblocksize vaut alors 0 car il n'est pas utilisé.*/
      if ( _nblocks_pproc == 1 ) {*blocksize=*lastblocksize;*lastblocksize=0;}
    } else
      *lastblocksize=0;

    ++(*start);
    *lastusedrank=_lastusedrank;
/*     printf("My rank %d, start %d, stride %d, blocksize %d, io_count %d, lastblocksize %d\n",myrank,*start,*stride,*blocksize,*io_count,*lastblocksize); */
    return;
}






