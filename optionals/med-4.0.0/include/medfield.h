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

#ifndef MED_MEDFIELD_H
#define MED_MEDFIELD_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Interface de l'API MED */
MEDC_EXPORT med_err
MEDfieldCr( const med_idt        fid,
	    const char * const   fieldname,
	    const med_field_type fieldtype,
	    const med_int        ncomponent,
	    const char * const   componentname,
	    const char * const   componentunit,
	    const char * const   dtunit,
	    const char * const   meshname);

MEDC_EXPORT med_err
MEDfieldValueAdvancedWr(const med_idt               fid,
			const char * const          fieldname,
			const med_int               numdt,
			const med_int               numit,
			const med_float             dt,
			const med_entity_type       entitype,
			const med_geometry_type     geotype,
			const char * const          localizationname,
			const med_filter * const    filter,
			const unsigned char * const value);

MEDC_EXPORT med_err
MEDfieldValueWithProfileWr(const med_idt               fid,
			   const char *  const         fieldname,
			   const med_int               numdt,
			   const med_int               numit,
			   const med_float             dt,
			   const med_entity_type       entitype,
			   const med_geometry_type     geotype,
			   const med_storage_mode      storagemode,
			   const char * const          profilename,
			   const char * const          localizationname,
			   const med_switch_mode       switchmode,
			   const med_int               componentselect,
			   const med_int               nentity,
			   const unsigned char * const value);

MEDC_EXPORT med_err
MEDfieldValueWr(const med_idt               fid,
		const char *  const         fieldname,
		const med_int               numdt,
		const med_int               numit,
		const med_float             dt,
		const med_entity_type       entitype,
		const med_geometry_type     geotype,
		const med_switch_mode       switchmode,
		const med_int               componentselect,
		const med_int               nentity,
		const unsigned char * const value);

MEDC_EXPORT med_err
MEDfieldValueAdvancedRd(const med_idt              fid,
			const char *  const        fieldname,
			const med_int              numdt,
			const med_int              numit,
			const med_entity_type      entitype,
			const med_geometry_type    geotype,
			const med_filter * const   filter,
			unsigned char * const      value);
MEDC_EXPORT med_err
MEDfieldValueWithProfileRd(const med_idt              fid,
			   const char *  const        fieldname,
			   const med_int              numdt,
			   const med_int              numit,
			   const med_entity_type      entitype,
			   const med_geometry_type    geotype,
			   const med_storage_mode     storagemode,
			   const char * const         profilename,
			   const med_switch_mode      switchmode,
			   const med_int              componentselect,
			   unsigned char * const      value);

MEDC_EXPORT med_err
MEDfield23ValueWithProfileRd(const med_idt              fid,
			     const char *  const        fieldname,
			     const med_int              numdt,
			     const med_int              numit,
			     const med_entity_type      entitype,
			     const med_geometry_type    geotype,
			     const char *  const        meshname,
			     const med_storage_mode     storagemode,
			     const char * const         profilename,
			     const med_switch_mode      switchmode,
			     const med_int              componentselect,
			     unsigned char * const      value);
MEDC_EXPORT med_err
MEDfieldValueRd(const med_idt              fid,
		const char *  const        fieldname,
		const med_int              numdt,
		const med_int              numit,
		const med_entity_type      entitype,
		const med_geometry_type    geotype,
		const med_switch_mode      switchmode,
		const med_int              componentselect,
		unsigned char * const      value);

MEDC_EXPORT med_int
MEDfieldnComponentByName(const med_idt      fid,
			 const char * const fieldname);

MEDC_EXPORT med_int
MEDfieldnComponent(const med_idt fid,
		   const int     ind);

MEDC_EXPORT med_int
MEDnField(const med_idt fid);

MEDC_EXPORT med_err
MEDfieldInfo(const med_idt          fid,
	     const int              ind,
	     char * const           fieldname,
	     char * const           meshname,
	     med_bool * const       localmesh,
	     med_field_type * const fieldtype,
	     char * const           componentname,
	     char * const           componentunit,
	     char * const           dtunit,
	     med_int * const        ncstp);

MEDC_EXPORT med_err
MEDfieldComputingStepInfo(const med_idt      fid,
			  const char * const fieldname,
			  const int          csit,
			  med_int * const    numdt,
			  med_int * const    numit,
			  med_float * const  dt);

MEDC_EXPORT med_err
MEDfieldComputingStepMeshInfo(const med_idt      fid,
			      const char * const fieldname,
			      const int          csit,
			      med_int * const    numdt,
			      med_int * const    numit,
			      med_float * const  dt,
			      med_int * const    meshnumdt,
			      med_int * const    meshnumit);

MEDC_EXPORT med_err
MEDfield23ComputingStepMeshInfo(const med_idt         fid,
				const char *    const fieldname,
				const int             csit,
				med_int *       const numdt,
				med_int *       const numit,
				med_float *     const dt,
				med_int *       const nmesh,
				char    *       const meshname,
				med_bool *      const localmesh,
				med_int *       const meshnumdt,
				med_int *       const meshnumit);

MEDC_EXPORT med_err
MEDfieldComputingStepMeshWr(const med_idt      fid,
			    const char * const fieldname,
			    const med_int      numdt,
			    const med_int      numit,
			    const med_int      meshnumdt,
			    const med_int      meshnumit);

MEDC_EXPORT med_err
MEDfieldInfoByName(const med_idt          fid,
		   const char * const     fieldname,
		   char * const           meshname,
		   med_bool * const       localmesh,
		   med_field_type * const fieldtype,
		   char * const           componentname,
		   char * const           componentunit,
		   char * const           dtunit,
		   med_int * const        ncstp);

MEDC_EXPORT med_int
MEDfieldnValueWithProfileByName(const med_idt      fid,
				const char * const fieldname,
				const med_int numdt,
				const med_int numit,
				const med_entity_type entitype,
				const med_geometry_type geotype,
				const char * const profilename,
				const med_storage_mode storagemode,
				med_int * const profilesize,
				char * const localizationname,
				med_int * const nintegrationpoint);

MEDC_EXPORT med_int
MEDfieldnValueWithProfile(const med_idt fid,
			  const char * const fieldname,
			  const med_int numdt,
			  const med_int numit,
			  const med_entity_type entitype,
			  const med_geometry_type geotype,
			  const int profileit,
			  const med_storage_mode storagemode,
			  char * const profilename ,
			  med_int * const profilesize,
			  char * const localizationname,
			  med_int * const nintegrationpoint);

MEDC_EXPORT med_int
MEDfieldnValue(const med_idt fid,
	       const char * const fieldname,
	       const med_int numdt,const med_int numit,
	       const med_entity_type entitype,
	       const med_geometry_type geotype);

MEDC_EXPORT med_int
MEDfield23nValueWithProfile(const med_idt fid,
			    const char * const fieldname,
			    const med_int numdt,
			    const med_int numit,
			    const med_entity_type entitype,
			    const med_geometry_type geotype,
			    const char * const meshname,
			    const int profileit,
			    const med_storage_mode storagemode,
			    char * const profilename,
			    med_int * const profilesize,
			    char * const localizationname,
			    med_int * const nintegrationpoint);

MEDC_EXPORT med_int
MEDfieldnProfile(const med_idt fid,
		 const char * const fieldname,
		 const med_int numdt,
		 const med_int numit,
		 const med_entity_type enttype,
		 const med_geometry_type geotype,
		 char * const defaultprofilename,
		 char * const defaultlocalizationname);

MEDC_EXPORT med_int
MEDfield23nProfile(const med_idt fid,
		   const char * const fieldname,
		   const med_int numdt,
		   const med_int numit,
		   const med_entity_type   entitype,
		   const med_geometry_type geotype,
		   const int    meshit,
		   char * const meshname,
		   char * const defaultprofilename,
		   char * const defaultlocalizationname);


MEDC_EXPORT med_err
MEDfieldInterpWr(const med_idt fid,
		 const char * const fieldname,
		 const char * const interpname);

MEDC_EXPORT med_int
MEDfieldnInterp(const med_idt fid,
		const char * const fieldname);


MEDC_EXPORT med_err
MEDfieldInterpInfo(const med_idt       fid,
		   const char *   const fieldname,
		   const int           interpit,
		         char *   const interpname
		   );


#ifdef __cplusplus
}
#endif

#endif /* MED_MEDFIELD_H */

