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

#ifndef MED_MEDINTERP_H
#define MED_MEDINTERP_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif
MEDC_EXPORT med_err
MEDinterpCr(const med_idt                 fid,
	    const char*             const interpname,
	    const med_geometry_type       geotype,
	    const med_bool                cellnodes,
	    const med_int                 nvariable,
	    const med_int                 maxdegree,
	    const med_int                 nmaxcoef
	    );

MEDC_EXPORT med_err
MEDinterpBaseFunctionWr( const med_idt          fid,
			 const char*      const interpname,
			 const med_int          basisfuncit,
			 const med_int          ncoef,
			 const med_int*   const power,
			 const med_float* const coefficient);

MEDC_EXPORT med_int
MEDnInterp(const med_idt fid);

MEDC_EXPORT med_err
MEDinterpInfo(const med_idt                 fid,
	      const int                      interpit,
	            char*              const interpname,
	            med_geometry_type* const geotype,
	            med_bool*          const cellnode,
	            med_int*           const nbasisfunc,
	            med_int*           const nvariable,
	            med_int*           const maxdegree,
	            med_int*           const nmaxcoef
	      );

MEDC_EXPORT med_err
MEDinterpInfoByName(const med_idt                   fid,
		    const char*               const interpname,
		           med_geometry_type* const geotype,
		           med_bool*          const cellnode,
		           med_int*           const nbasisfunc,
		           med_int*           const nvariable,
		           med_int*           const maxdegree,
		           med_int*           const nmaxcoef
		    );

MEDC_EXPORT med_err
MEDinterpBaseFunctionRd( const med_idt          fid,
			 const char*      const interpname,
			 const int              basisfuncit,
			       med_int*   const ncoef,
			       med_int*   const power,
			 med_float* const coefficient);

MEDC_EXPORT med_int
MEDinterpBaseFunctionCoefSize( const med_idt          fid,
			       const char*      const interpname,
			       const med_int          basisfuncit);

/* Fonctions d'interpolation */

#ifdef __cplusplus
}
#endif

#endif /* MED_MEDINTERP_H */

