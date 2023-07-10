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

static inline med_int _identity  (int i)  { return i; }
static const med_int * profilearray_global;
static inline med_int _withprofilearray(int i) {
  return (profilearray_global[i]-1);
}

/*Les données générées, le sont uniquement aux endroits utilisés */
void generateFullIDatas(const int myrank, const int lastrank, const int sizeoftype,
			const med_storage_mode profilemode, const med_size profilesize, const med_int * const profilearray,
			const med_size start, const med_size stride, const med_size count, const med_size blocksize, const med_size lastblocksize,
			const int nentities, const int nvaluesperentity, const int nconstituentpervalue,
			med_float ** valuesarray ) {

  med_size _start=start-1,_blockstart = 0,_blocksize=blocksize,_allblocksize=0,_index=0;
  med_int  (*_profilearrayfunc)(int)=0;
  int _blocknum=0,_i=0,_j=0,_k=0;

  profilearray_global = profilearray;
  
  if (profilesize) {
    if ( profilearray == NULL ) { MESSAGE("Error, profilesize > 0 && profilearray == 0"); }
    MESSAGE("Using a profile...");
    _profilearrayfunc = _withprofilearray;
  } else {
    _profilearrayfunc = _identity;
  }

  switch(profilemode) {

  case MED_GLOBAL_STMODE :

    /*       ISCRUTE(lastblocksize); */
    /*En mode global on n'a normalement pas besoin de prendre en compte les profils. Il ne peut pas y en avoir.
      Celà n'a pas de sens sauf si la sélection demandée est un seul block !
      Tous les processus possèdent le tableau global. */
    *valuesarray = (med_float *) calloc(nentities*nvaluesperentity*nconstituentpervalue,sizeoftype);
    for (_blocknum=0; _blocknum< count; ++_blocknum) {
      _blockstart=_blocknum*stride;
      /* 	ISCRUTE(_blockstart); */
      if ( (count > 1) && (_blocknum == (count-1) ) && (myrank == lastrank) ) _blocksize=lastblocksize;
      /* 	ISCRUTE(_blocksize); */
      for (_i=0; _i<_blocksize; ++_i)
	for (_j=0; _j < nvaluesperentity; ++_j)
	  for (_k=0; _k < nconstituentpervalue; ++_k) {
	    _index = _profilearrayfunc(_start+_blockstart+_i)*nvaluesperentity*nconstituentpervalue
	      +_j*nconstituentpervalue+_k;
	    (*valuesarray)[_index]= (myrank+1)*1000+_blocknum*100+_i+0.1*_j+0.01*_k;
	    /*      ISCRUTE(_index); */
	    /*      RSCRUTE((*valuesarray)[_index]); */

	  }
    }
    break;

  case MED_COMPACT_STMODE :

    /*Idem avec ou sans profil*/
    if ( (myrank == lastrank) ) _allblocksize=blocksize*count+lastblocksize; else _allblocksize = blocksize*count;
    *valuesarray = (med_float *) calloc(_allblocksize*nvaluesperentity*nconstituentpervalue,sizeoftype);

    _index = 0;
    for (_blocknum=0; _blocknum< count; ++_blocknum) {
      if ( (count > 1) && (_blocknum == (count-1) ) && (myrank == lastrank) ) _blocksize=lastblocksize;
      for (_i=0; _i<_blocksize; ++_i)
	for (_j=0; _j < nvaluesperentity; ++_j)
	  for (_k=0; _k < nconstituentpervalue; ++_k) {
	    (*valuesarray)[_index]= (myrank+1)*1000+_blocknum*100+_i+0.1*_j+0.01*_k;
	    /*      ISCRUTE(_index); */
	    /*      RSCRUTE((*valuesarray)[_index]); */
	    _index++;
	  }
    }

    break;
  default:
    break;
  }
}

void generateNoIDatas(const int myrank, const int lastrank, const int sizeoftype,
		      const med_storage_mode storagemode, const med_size profilearraysize, const med_int * const profilearray,
		      const med_size start, const med_size stride, const med_size count, const med_size blocksize, const med_size lastblocksize,
		      const int nentities, const int nvaluesperentity, const int nconstituentpervalue,
		      med_float ** valuesarray ) {

  med_size _start=start-1,_blockstart = 0,_blocksize=blocksize,_allblocksize=0,_index=0,_dim=0;
  med_int  (*_profilearrayfunc)(int)=0;
  int _blocknum=0,_i=0,_j=0,_k=0;
  profilearray_global = profilearray;

  if (profilearraysize) {
    MESSAGE("Using a profile...");
    if ( profilearray == NULL ) {MESSAGE("Error, profilearraysize > 0 && profilearray == 0"); }
    _profilearrayfunc = _withprofilearray;
  } else {
    _profilearrayfunc = _identity;
  }

  switch(storagemode) {

  case MED_GLOBAL_STMODE :

    /*En mode global on n'a normalement pas besoin de prendre en compte les profils. Il ne peut pas y en avoir.
      Celà n'a pas de sens sauf si la sélection demandée est un seul block !
      Tous les processus possèdent le tableau global. */
    *valuesarray = (med_float *) calloc(nentities*nvaluesperentity*nconstituentpervalue,sizeoftype);

    for (_dim=0; _dim< nconstituentpervalue; ++_dim) {
      _blocksize = blocksize;
      for (_blocknum=0; _blocknum< count; ++_blocknum) {
	_blockstart=_blocknum*stride;
	if ( (count > 1) && (_blocknum == (count-1) ) && (myrank == lastrank) ) _blocksize=lastblocksize;
	for (_i=0; _i<_blocksize; ++_i)
	  for (_j=0; _j < nvaluesperentity; ++_j) {
	    _index = ( _dim*nentities
		       +_profilearrayfunc(_start+_blockstart+_i) )
	      *nvaluesperentity + _j;
	    (*valuesarray)[_index]= (myrank+1)*1000+_blocknum*100+_i+0.1*_j+0.01*_dim;
	    /*      ISCRUTE(_index); */
	    /*      RSCRUTE((*valuesarray)[_index]); */
	  }
      }
    }
    break;

  case MED_COMPACT_STMODE :
    if ( (myrank == lastrank) ) _allblocksize=blocksize*count+lastblocksize; else _allblocksize = blocksize*count;
    *valuesarray = (med_float *) calloc(_allblocksize*nvaluesperentity*nconstituentpervalue,sizeoftype);

    _index = 0;
    for (_dim=0; _dim< nconstituentpervalue; ++_dim) {
      _blocksize = blocksize;
      for (_blocknum=0; _blocknum< count; ++_blocknum) {
	if ( (count > 1) && (_blocknum == (count-1) ) && (myrank == lastrank) ) _blocksize=lastblocksize;
	for (_i=0; _i<_blocksize; ++_i)
	  for (_j=0; _j < nvaluesperentity; ++_j) {
	    (*valuesarray)[_index]= (myrank+1)*1000+_blocknum*100+_i+0.1*_j+0.01*_dim;
	    /*      ISCRUTE(_index); */
	    /*      RSCRUTE((*valuesarray)[_index]); */
	    _index++;
	  }
      }
    }

    break;
  default:
    break;
  }
}
