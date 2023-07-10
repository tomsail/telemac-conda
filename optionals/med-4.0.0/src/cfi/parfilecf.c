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

#define MED_HAVE_MPI

#include <med.h>
#include "med_config.h"
#include "med_outils.h"
#include <string.h>
#include <stdlib.h>

#include <mpi.h>

extern void *MedVersionedApi3F;
extern void f77Api3IsSet(void * obj);

/*
  From Fortran call of following C functions :
- MEDparfileOpen
*/



#define nmpffope F77_FUNC(mpffope,MPFFOPE)

#ifdef PPRO_NT
med_int
MPFFOPE(const char const *name,
		  const unsigned int bidon,
		  const med_int const *access,
		  const med_int const *len,
		  const med_int const *com,
		  const med_int const *info)
#else
med_int
nmpffope (const char const *name,
	  const med_int const *access,
	  const med_int const *len,
	  const med_int const *com,
	  const med_int const *info)
#endif
{
  char *          _fn;
  med_int         _ret;
  med_access_mode _access;
  MPI_Comm        _comm;
  MPI_Info        _info;

  /* Cette méthode a pour unique but de forcer la construction
   de l'objet MedVersionedApiF et donc la mise à jour de MedVersionedApi
  avec les APis fortran. Certains compilateurs suppriment le symbole MedVersionedApiF
  croyant qu'il nest pas utilisé et la mise à jour de MedVersionedApi n'est
  alors pas effectuée.*/
  f77Api3IsSet(MedVersionedApi3F);

  _comm = MPI_Comm_f2c((MPI_Fint) *com);
  _info = MPI_Info_f2c((MPI_Fint) *info);

  _fn = _MED2cstring((char *) name, (int) *len);
  if (!_fn)
	return(-1);
  _access = (med_access_mode) *access;
/*   ISCRUTE(*com); */
/*   ISCRUTE(_comm); */
/*   ISCRUTE(*info); */
/*   ISCRUTE(_info); */
/*   SSCRUTE(_fn); */
/*   ISCRUTE_int(_access); */

  _ret = (med_int) MEDparFileOpen(_fn, _access, _comm, _info);
/*   ISCRUTE(_ret); */

  _MEDcstringFree(_fn);

  return(_ret);
}


