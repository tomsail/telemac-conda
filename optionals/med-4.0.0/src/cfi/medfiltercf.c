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
#include "med_config.h"
#include "med_outils.h"
#include <string.h>
#include <stdlib.h>

/*
From Fortran call of following C functions : 
- MEDfilterEntityCr
- MEDfilterAllocate
- MEDfilterDeAllocate
- MEDfilterBlockOfEntityCr   
*/

#define nmfrfall F77_FUNC(mfrfall,NMFRFALL)
#define nmfrfdea F77_FUNC(mfrfdea,NMFRFDEA)
#define nmfrfcre F77_FUNC(mfrfcre,MFRFCRE)
#define nmfrfblc F77_FUNC(mfrfblc,MFRFBLC)


#ifdef PPRO_NT
med_int
MFRFALL(med_int *nflt,unsigned char **flt)
#else
med_int
nmfrfall(med_int* nflt,unsigned char** flt)
#endif
{
  med_filter *_filter;
  med_err _ret=0;
  int _i;

  _filter = MEDfilterAllocate((med_int) *nflt);
  if (! _filter) _ret = -1;

  for (_i=0;_i<*nflt;_i++) {
    *(flt + _i*8) = 
      (unsigned char*) &_filter[_i];
  }
  
  return (_ret);
}




#ifdef PPRO_NT
med_int
MFRFDEA(med_int *nflt,unsigned char **flt)
#else
med_int
nmfrfdea(med_int* nflt,unsigned char** flt)
#endif
{
  med_filter *_filter;
  med_err     _ret=0;
  int         _i;

  for (_i=0;_i<*nflt;_i++) 
    MEDfilterClose( (med_filter *) *(flt + _i*8));

  free((med_filter *)flt[0]);
  
  return (_ret);
}




#ifdef PPRO_NT
med_int
MFRFCRE(med_idt *fid, med_int* nent, med_int *nvent, med_int *ncent,
		  med_int *cs, med_int *swm, med_int *stm, char *pname, unsigned int bidon, med_int *pnamelen, 
		  med_int *fltas, med_int *flta, unsigned char **flt)
#else
med_int
nmfrfcre(med_idt *fid, med_int* nent, med_int *nvent, med_int *ncent,
	 med_int *cs, med_int *swm,med_int *stm,char *pname, med_int *pnamelen, 
	 med_int *fltas,med_int *flta, unsigned char **flt)
#endif
{
  char *_fn=NULL;
  med_int _ret=0;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_storage_mode  _smode = (med_storage_mode) *stm;
  med_filter *_filter = (med_filter*) *flt; 
  med_int *_flta=NULL;

  _fn = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn)
    return(-1);

  if (*fltas != MED_UNDEF_SIZE)
    _flta = flta;
  
  _ret = (med_int) MEDfilterEntityCr((med_idt) *fid,
				     (med_int) *nent,
				     (med_int) *nvent,
				     (med_int) *ncent,
				     (med_int) *cs,
				     _swm,
				     _smode,
				     _fn,
				     (med_int) *fltas,
				     _flta,
				     _filter);


  if (_fn) 
    _MEDcstringFree(_fn);

  return(_ret); 
}




#ifdef PPRO_NT
med_int
MFRFBLC(med_idt *fid, med_int* nent, med_int *nvent, med_int *ncent,
		  med_int *cs, med_int *swm, med_int *stm, char *pname, unsigned int bidon, med_int *pnamelen,  
		  med_int *start, med_int *stride, med_int *count, med_int *bsize, med_int *lbsize,
		  unsigned char **flt)
#else
med_int
nmfrfblc(med_idt *fid, med_int* nent, med_int *nvent, med_int *ncent,
	 med_int *cs, med_int *swm,med_int *stm,char *pname, med_int *pnamelen, 
	 med_int *start, med_int *stride, med_int *count, med_int *bsize, med_int *lbsize,
	 unsigned char **flt)
#endif
{
  char *_fn=NULL;
  med_int _ret=0;
  med_switch_mode _swm = (med_switch_mode) *swm;
  med_storage_mode  _smode = (med_storage_mode) *stm;
  med_filter *_filter= (med_filter*) *flt; 
  med_size _start = (med_size) *start;
  med_size _stride = (med_size) *stride;
  med_size _count = (med_size) *count;
  med_size _bsize = (med_size) *bsize;
  med_size _lbsize = (med_size) *lbsize;

  _fn = _MED2cstring((char *) pname, (int) *pnamelen);
  if (!_fn)
    return(-1);
  
  _ret = (med_int) MEDfilterBlockOfEntityCr((med_idt) *fid,
					    (med_int) *nent,
					    (med_int) *nvent,
					    (med_int) *ncent,
					    (med_int) *cs,
					    _swm,
					    _smode,
					    _fn,
					    _start,
					    _stride,
					    _count,
					    _bsize,
					    _lbsize,
					    _filter);


  if (_fn) 
    _MEDcstringFree(_fn);

  return(_ret); 
}







