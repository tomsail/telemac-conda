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

#include <2.3.6/med23v30_misc.h>

/*
 *  Chaine C MED2.3 -> Chaine C MED3.0
 */

med_err _MED23v30stringConvert(char *chaine30, med_int substrsize30,
			       char *chaine23, med_int substrsize23,
			       med_int nsubstr )
{
  int       i,j;
  med_int  _substrsize23 = substrsize23;
  med_int  _nsubstr      = 0;
  med_int  _realnsubstr  = 0;
  int      _lenchaine23  = 0;

  if ( substrsize30 < substrsize23 ) return -1;

  _lenchaine23 = strlen(chaine23);
  _realnsubstr = _lenchaine23/substrsize23;

  if ( ( _realnsubstr < nsubstr) && _lenchaine23 ) {
/*     MESSAGE("Be careful, size of string chaine23 should a multiple of substrsize23"); */
/*     SSCRUTE(chaine23); */
/*     ISCRUTE(_lenchaine23); */
/*     ISCRUTE(substrsize23); */
/*     ISCRUTE(nsubstr); */
/*     ISCRUTE(_realnsubstr); */
    _nsubstr=_realnsubstr+1;
  }
  if (_realnsubstr == nsubstr ) _nsubstr=nsubstr;
/*     SSCRUTE(chaine23); */
/*     ISCRUTE(_lenchaine23); */
/*     ISCRUTE(substrsize23); */
/*     ISCRUTE(nsubstr); */
/*     ISCRUTE(_nsubstr); */
/*     ISCRUTE(_realnsubstr); */

  for (i=0;i<_nsubstr;i++) {
    strncpy(chaine30+i*substrsize30,
	    chaine23+i*substrsize23,
	    substrsize23);

    if ( (i == (_nsubstr-1)       ) &&
	 (_realnsubstr != nsubstr ) ) _substrsize23=_lenchaine23-_realnsubstr*substrsize23;

    for (j=_substrsize23;j<substrsize30;j++) {
      *(chaine30+i*substrsize30 + j)=' ';
    }
  }
  *(chaine30+_nsubstr*substrsize30) = '\0';
/*   SSCRUTE(chaine30); */
/*   ISCRUTE(strlen(chaine30)); */

  return 0;
}
