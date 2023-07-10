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
- MEDlibraryClose
- MEDlibraryHdfNumVersion
- MEDlibraryHdfStrVersion 
- MEDlibraryNumVersion
- MEDlibraryStrVersion
*/

#define nmlbfclo F77_FUNC(mlbfclo,MLBFCLO)
#define nmlbfnuv F77_FUNC(mlbfnuv,MLBFNUV)
#define nmlbfstv F77_FUNC(mlbfstv,MLBFSTV)
#define nmlbfhnv F77_FUNC(mlbfhnv,MLBFHNV)
#define nmlbfhsv F77_FUNC(mlbfhsv,MLBFHSV)

#ifdef PPRO_NT
med_int
MLBFCLO(void)
#else
med_int
nmlbfclo(void)
#endif
{
  med_int _ret;
  
  _ret = (med_int) MEDlibraryClose();

  return(_ret);
}


#ifdef PPRO_NT
med_int
MLBFNUV(med_int* const major, 
		  med_int* const minor, 
		  med_int* const release)
#else
med_int
nmlbfnuv(med_int* const major, 
	 med_int* const minor, 
	 med_int* const release)
#endif
{
  med_int _ret;
  
  _ret = (med_int) MEDlibraryNumVersion(major,minor,release);

  return(_ret);
}

#ifdef PPRO_NT
med_int
MLBFSTV(char* const medversion, unsigned int bidon, med_int *len)
#else
med_int
nmlbfstv(char* const medversion, med_int *len)
#endif
{
  med_int _ret;
  char    _fs1[20]="";

  _ret = (med_int) MEDlibraryStrVersion(_fs1);

  _MEDc2fString(_fs1,medversion,*len);

  return(_ret);
}


#ifdef PPRO_NT
med_int
MLBFHNV(med_int* const major, 
		  med_int* const minor, 
		  med_int* const release)
#else
med_int
nmlbfhnv(med_int* const major, 
	 med_int* const minor, 
	 med_int* const release)
#endif
{
  med_int _ret;
  
  _ret = (med_int) MEDlibraryHdfNumVersion(major,minor,release);

  return(_ret);
}



#ifdef PPRO_NT
med_int
MLBFHSV(char* const hdfversion, unsigned int bidon, med_int *len)
#else
med_int
nmlbfhsv(char* const hdfversion, med_int *len)
#endif
{
  med_int _ret;
  char    _fs1[20]="";

  _ret = (med_int) MEDlibraryHdfStrVersion(_fs1);

  _MEDc2fString(_fs1,hdfversion,*len);

  return(_ret);
}
