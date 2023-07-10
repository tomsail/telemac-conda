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

#ifndef MED_MEDLINK_H
#define MED_MEDLINK_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Link */
MEDC_EXPORT med_err
MEDlinkWr(const med_idt        fid,
	  const char   * const meshname,
	  const char   * const link);

MEDC_EXPORT med_err
MEDlinkRd(const med_idt     fid,
	  const char* const meshname,
	  char* const       link);

MEDC_EXPORT med_int
MEDnLink(const med_idt fid );
MEDC_EXPORT med_int
MEDlinkInfoByName(const med_idt             fid,
		  const char        * const meshname );

MEDC_EXPORT med_err
MEDlinkInfo(const med_idt             fid,
	    const int                 linkit,
	    char              * const meshname,
	    med_int           * const linksize );


#ifdef __cplusplus
}
#endif

#endif /* MED_MEDLINK_H */

