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

#ifndef MED_MEDPROFILE_H
#define MED_MEDPROFILE_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif


/* Profile  */
MEDC_EXPORT med_int
MEDnProfile(const med_idt fid );

MEDC_EXPORT med_err
MEDprofileInfo(const med_idt          fid,
	       const int              profileit,
	       char    *     const    profilename,
	       med_int *     const    profilesize );
MEDC_EXPORT med_err
MEDprofileWr(const med_idt        fid,
	     const char* const    profilename,
	     const med_int        profilesize,
	     const med_int* const profilearray);

MEDC_EXPORT med_int
MEDprofileSizeByName(const med_idt fid, const char * const profilename);

MEDC_EXPORT med_err
MEDprofileRd(const med_idt      fid,
	     const char * const profilename,
	     med_int * const    profilearray );


#ifdef __cplusplus
}
#endif

#endif /* MED_MEDPROFILE_H */

