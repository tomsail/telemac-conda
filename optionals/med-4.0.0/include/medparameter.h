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

#ifndef MED_MEDPARAMETER_H
#define MED_MEDPARAMETER_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

MEDC_EXPORT med_err
MEDparameterCr(const med_idt fid,
	       const char * const paramname,
	       const med_parameter_type paramtype,
	       const char* const description,
	       const char * const dtunit
	       );

MEDC_EXPORT med_int
MEDnParameter(const med_idt fid);

MEDC_EXPORT med_err
MEDparameterInfo(const med_idt            fid,
		 const int                paramit,
		 char   *             const paramname,
		 med_parameter_type * const paramtype,
		 char *               const description,
		 char *               const dtunit,
		 med_int *            const nstep);

MEDC_EXPORT med_err
MEDparameterInfoByName(const med_idt        fid,
		       const char   *       const paramname,
		       med_parameter_type * const paramtype,
		       char *               const description,
		       char *               const dtunit,
		       med_int *            const nstep);

MEDC_EXPORT med_err
MEDparameterValueWr(const med_idt              fid,
		    const char*  const         paramname,
		    const med_int              numdt,
		    const med_int              numit,
		    const med_float            dt,
		    const unsigned char* const value);

MEDC_EXPORT med_err
MEDparameterValueRd(const med_idt              fid,
		    const char*  const         paramname,
		    const med_int              numdt,
		    const med_int              numit,
		    unsigned char* const value);


MEDC_EXPORT med_err
MEDparameterComputationStepInfo(const med_idt      fid,
				const char * const paramname,
				const int          csit,
				med_int * const    numdt,
				med_int * const    numit,
				med_float * const  dt );

#ifdef __cplusplus
}
#endif

#endif /* MED_MEDPARAMETER_H */

