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
#include <string.h>

#include <unistd.h>
#include <time.h>
#include <assert.h>
#include <stdlib.h>

static int cmp(const med_int *p1, const med_int *p2) { return *p1-*p2; }

med_err generateFilterArray( const med_size nentities, const med_size nvaluesperentity, const med_size nconstituentpervalue,
			     const med_size profilearraysize, const med_int * const profilearray,
			     med_int  * const nentitiesfiltered, med_int **filterarray ) {


  med_err    _ret=-1;
  med_int    _nentitiesfiltered=0,_maxfilternentities=0;
  med_int    *_indexarray=NULL;
  int        _i=0;
  struct tm  *_tm ;
  time_t _tt=time(0);

  _tm = localtime(&_tt);


  /*Taille du tableau des numéros d'entités à filtrer */
  if ( profilearraysize ) {
    _maxfilternentities = profilearraysize;
  } else {
    _maxfilternentities = nentities;
  }

  /* Allocation du tableau de filtre */

  srandom((*_tm).tm_sec * (*_tm).tm_min );
  _nentitiesfiltered         = 1 + (int) ((float)(_maxfilternentities) * (random() / (RAND_MAX + 1.0)));
  /*       _nentitiesfiltered         = 2; */

  (*filterarray)         = malloc(_nentitiesfiltered*sizeof(med_int));

/*   if ( profilearraysize) { */
/*     _indexarray     = malloc(_nentitiesfiltered*sizeof(med_int)); */
/*   } else { */
/*     _indexarray=(*filterarray); */
/*   } */

  _indexarray=(*filterarray);

  for (_i=0; _i < _nentitiesfiltered; ++_i ) {
    _indexarray[_i] =  1 + (int) ((double)(_maxfilternentities) * (random() / (RAND_MAX + 1.0)));
  }

  /*N'enlève pas les doublons, mais celà fonctionne*/
  qsort(_indexarray, _nentitiesfiltered, sizeof(med_int), (int(*)(const void *, const void *) ) cmp);

/*   for (_i=0; _i < _nentitiesfiltered; ++_i ) { */
/*     ISCRUTE(_indexarray[_i]); */
/*   } */

  /* Cette indirection ne doit jamais être faite car le tableau filtre contient des indices de profils.*/
/*   if ( profilearraysize) */
/*     for (_i=0; _i < _nentitiesfiltered; ++_i ) { */
/*       (*filterarray)[_i] = profilearray[_indexarray[_i]]; */
/*       ISCRUTE((*filterarray)[_i]); */
/*     } */

  *nentitiesfiltered=_nentitiesfiltered;

  _ret=0;

 ERROR:
/*   if ( profilearraysize) free(_indexarray); */

  return _ret;

}
