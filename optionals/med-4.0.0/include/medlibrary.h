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

#ifndef MED_MEDLIBRARY_H
#define MED_MEDLIBRARY_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Interface de l'API MED */
/* Library */

MEDC_EXPORT med_err
MEDlibraryNumVersion(med_int* const major,
		     med_int* const minor,
		     med_int* const release);

MEDC_EXPORT med_err
MEDlibraryStrVersion(char* const medversion);

MEDC_EXPORT med_err
MEDlibraryHdfNumVersion(med_int* const major,
			med_int* const minor,
			med_int* const release);
MEDC_EXPORT med_err
MEDlibraryHdfStrVersion(char* const  version);
MEDC_EXPORT med_err
MEDlibraryClose(void);

#ifdef __cplusplus
}
#endif

#endif /* MED_MEDLIBRARY_H */
