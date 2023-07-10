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

#ifndef MED_MEDSUBDOMAINJOINT_H
#define MED_MEDSUBDOMAINJOINT_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Subdomainjoint */

MEDC_EXPORT med_err
MEDsubdomainJointCr(const med_idt      fid,
		    const char * const localmeshname,
		    const char * const jointname,
		    const char * const description,
		    const med_int      domainnumber,
		    const char * const remotemeshname);

MEDC_EXPORT med_err
MEDsubdomainCorrespondenceWr(const med_idt            fid,
			     const char * const       meshname,
			     const char * const       jointname,
			     const med_int            numdt,
			     const med_int            numit,
			     const med_entity_type    localentitytype,
			     const med_geometry_type  localgeotype,
			     const med_entity_type    remoteentitytype,
			     const med_geometry_type  remotegeotype,
			     const med_int            nentity,
			     const med_int * const    correspondence);

MEDC_EXPORT med_int
MEDnSubdomainJoint(const med_idt      fid,
		   const char * const meshname);

MEDC_EXPORT med_err
MEDsubdomainJointInfo(const med_idt      fid,
		      const char * const meshname,
		      const int          jointit,
		      char * const       jointname,
		      char * const       description,
		      med_int * const    domainnumber,
		      char * const       remotemeshname,
		      med_int * const    nstep,
		      med_int * const    nocstpncorrespondence);

MEDC_EXPORT med_err
MEDsubdomainComputingStepInfo(const med_idt      fid,
			      const char * const meshname,
			      const char * const jointname,
			      const int          csit,
			      med_int * const    numdt,
			      med_int * const    numit,
			      med_int * const    ncorrespondence );

MEDC_EXPORT med_err
MEDsubdomainCorrespondenceSizeInfo(const med_idt              fid,
				   const char * const         meshname,
				   const char * const         jointname,
				   const med_int              numdt,
				   const med_int              numit,
				   const int                  corit,
				   med_entity_type   * const  localentitytype,
				   med_geometry_type * const  localgeotype,
				   med_entity_type   * const  remoteentitytype,
				   med_geometry_type * const  remotegeotype,
				   med_int * const            nentity );

MEDC_EXPORT med_err
MEDsubdomainCorrespondenceSize(const med_idt              fid,
			       const char * const         meshname,
			       const char * const         jointname,
			       const med_int              numdt,
			       const med_int              numit,
			       const med_entity_type      localentitytype,
			       const med_geometry_type    localgeotype,
			       const med_entity_type      remoteentitytype,
			       const med_geometry_type    remotegeotype,
			       med_int * const            nentity );
MEDC_EXPORT med_err
MEDsubdomainCorrespondenceRd(const med_idt            fid,
			     const char * const       meshname,
			     const char * const       jointname,
			     const med_int            numdt,
			     const med_int            numit,
			     const med_entity_type    localentitytype,
			     const med_geometry_type  localgeotype,
			     const med_entity_type    remoteentitytype,
			     const med_geometry_type  remotegeotype,
			     med_int * const          correspondence);

#ifdef __cplusplus
}
#endif

#endif /* MED_MEDSUBDOMAINJOINT_H */

