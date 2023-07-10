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

#ifndef MED_MEDEQUIVALENCE_H
#define MED_MEDEQUIVALENCE_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif


/* Equivalence  */

MEDC_EXPORT med_err
MEDequivalenceCr(const med_idt      fid,
		 const char * const meshname,
		 const char * const equivname,
		 const char * const description);

MEDC_EXPORT med_err
MEDequivalenceCorrespondenceWr(const med_idt            fid,
			       const char * const       meshname,
			       const char * const       equivname,
			       const med_int            numdt,
			       const med_int            numit,
			       const med_entity_type    entitype,
			       const med_geometry_type  geotype,
			       const med_int            nentity,
			       const med_int * const    correspondence);

MEDC_EXPORT med_err
MEDequivalenceCorrespondenceRd(const med_idt            fid,
			       const char * const       meshname,
			       const char * const       equivname,
			       const med_int            numdt,
			       const med_int            numit,
			       const med_entity_type    entitype,
			       const med_geometry_type  geotype,
			       med_int * const          correspondence);

MEDC_EXPORT med_int
MEDnEquivalence(const med_idt      fid,
		const char * const meshname);

MEDC_EXPORT med_err
MEDequivalenceInfo(const med_idt      fid,
		   const char * const meshname,
		   const int          equivit,
		   char * const       equivname,
		   char * const       equivdescription,
		   med_int * const    nstep,
		   med_int * const    nocstpncorrespondence);

MEDC_EXPORT med_err
MEDequivalenceComputingStepInfo(const med_idt      fid,
				const char * const meshname,
				const char * const equivname,
				const int          csit,
				med_int * const    numdt,
				med_int * const    numit,
				med_int * const    ncorrespondence );


MEDC_EXPORT med_err
MEDequivalenceCorrespondenceSizeInfo(const med_idt              fid,
				     const char * const         meshname,
				     const char * const         equivname,
				     const med_int              numdt,
				     const med_int              numit,
				     const int                  corit,
				     med_entity_type * const    entitype,
				     med_geometry_type* const   geotype,
				     med_int * const            nentity );

/*Uses case equivalencename from Mesh element iteration*/
/* MEDC_EXPORT med_err */
/* MEDequivalenceInfoByName(const med_idt      fid, */
/* 			 const char * const meshname, */
/* 			 const char * const equivname, */
/* 			 char * const       equivdescription, */
/* 			 med_int * const    nstep, */
/* 			 med_int * const    nocstpncorrespondence); */

MEDC_EXPORT med_err
MEDequivalenceCorrespondenceSize(const med_idt             fid,
				 const char * const        meshname,
				 const char * const        equivname,
				 const med_int             numdt,
				 const med_int             numit,
				 const med_entity_type     entitype,
				 const med_geometry_type   geotype,
				 med_int * const           nentity );



/* MEDC_EXPORT med_err */
/* MEDmeshEquivalenceInfo(const med_idt            fid, */
/* 		       const char * const       meshname, */
/* 		       const med_int            numdt, */
/* 		       const med_int            numit, */
/* 		       const med_entity_type    entitype, */
/* 		       const med_geometry_type  geotype, */
/* 		       char * const             equivname ); */

#ifdef __cplusplus
}
#endif

#endif /* MED_MEDEQUIVALENCE_H */

