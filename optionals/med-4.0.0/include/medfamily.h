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

#ifndef MED_MEDFAMILY_H
#define MED_MEDFAMILY_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif


/* Family  */

MEDC_EXPORT med_err
MEDfamilyCr(const med_idt      fid,
	    const char * const meshname,
	    const char * const familyname,
	    const med_int      familynumber,
	    const med_int      ngroup,
	    const char * const groupname);

MEDC_EXPORT med_int
MEDnFamily(const med_idt      fid,
	   const char * const meshname);

MEDC_EXPORT med_int
MEDnFamilyGroup(const med_idt      fid,
		const char * const meshname,
		const int          famit);

MEDC_EXPORT med_err
MEDfamilyInfo(const med_idt      fid,
	      const char * const meshname,
	      const int          famit,
	      char * const       familyname,
	      med_int * const    familynumber,
	      char * const       groupname);

MEDC_EXPORT med_int
MEDnFamily23Attribute(const med_idt      fid,
		      const char * const meshname,
		      const int          famit);

MEDC_EXPORT med_err
MEDfamily23Info( const med_idt       fid,
		 const char *  const meshname,
		 const int           famit,
		 char *        const familyname,
		 med_int *     const attributenumber,
		 med_int *     const attributevalue,
		 char *        const attributedes,
		 med_int *     const familynumber,
		 char *        const groupname);
#ifdef __cplusplus
}
#endif

#endif /* MED_MEDFAMILY_H */

