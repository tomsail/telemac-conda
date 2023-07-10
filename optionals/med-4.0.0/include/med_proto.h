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

#ifndef MED_PROTO_H
#define MED_PROTO_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Interface de l'API MED */

/* Library */
#include "medlibrary.h"

/* File */
#include "medfile.h"

/* Mesh */
#include "medmesh.h"

/*routine interne*/
MEDC_EXPORT med_err
_MEDmeshCr(const med_idt fid, const char * const root,
	   const char * const meshname, const med_int spacedim,
	   const med_int meshdim, const med_mesh_type meshtype,
	   const char * const description, const char * const dtunit,
	   const med_sorting_type sortingtype,
	   const med_axis_type axistype, const char * const axisname,
	   const char * const axisunit);

MEDC_EXPORT med_err
_MEDmeshInfoByName(const med_idt fid, const char * const root,const char * const meshname,
		   med_int * const spacedim,med_int * const meshdim,  med_mesh_type * const meshtype,
		   char * const description,  char * const dtunit,
		   med_sorting_type *const sortingtype,
		   med_int * const nstep,  med_axis_type * const axistype,  char * const axisname,
		   char * const axisunit);

MEDC_EXPORT med_idt
_MEDmeshAssociatedGroupCr(const med_idt               id,
                          const char*  const          rootname,
                          const char*  const          meshname,
                          const med_int               numdt,
                          const med_int               numit,
                          const med_float             dt,
 			  const med_bool              justopen,
                          const char*  const          datagroupname );

MEDC_EXPORT med_err
_MEDmeshAdvancedWr(const med_idt               fid,
		   const char*  const          meshname,
		   const med_data_type         meddatatype,
		   const char*  const          datasetname,
		   const med_internal_type     datatype,
		   const med_int               numdt,
		   const med_int               numit,
		   const med_float             dt,
		   const med_entity_type       entitype,
		   const med_geometry_type     geotype,
		   const med_connectivity_mode cmode,
		   const med_storage_mode      storagemode,
		   const char * const          profilename,
		   const med_switch_mode       switchmode,
		   const med_int               dimselect,
		   const med_filter * const    filter,
		   const med_int               nentity,
		   const void * const          value);

MEDC_EXPORT med_err
_MEDmeshAdvancedRd(const med_idt               fid,
		   const char*  const          meshname,
		   const med_data_type         meddatatype,
		   const char*  const          datasetname,
		   const med_internal_type     datatype,
		   const med_int               numdt,
		   const med_int               numit,
		   const med_entity_type       entitype,
		   const med_geometry_type     geotype,
		   const med_connectivity_mode cmode,
		   const med_storage_mode      storagemode,
		   const char * const          profilename,
		   const med_switch_mode       switchmode,
		   const med_int               dimselect,
		   const med_filter * const    filter,
		   unsigned char* const  value);

MEDC_EXPORT med_int
_MEDmeshnEntity(const med_idt fid,
		const char * const meshname,
		const med_int numdt,
		const med_int numit,
		const med_entity_type entitype,
		const med_geometry_type geotype,
		const med_data_type datatype,
		const med_connectivity_mode cmode,
		const med_storage_mode storagemode,
		char     * const profilename,
		med_int  * const profilesize,
		med_bool * const changement,
		med_bool * const transformation );

 MEDC_EXPORT med_idt
_MEDmeshDatagroupOpen(const med_idt               fid,
		      const char*  const          meshname,
		      char *       const          usedpath,
		      med_bool *   const          isasupportmesh
		     );

/*fin routines  internes */

/*FIELD*/

#include "medfield.h"

/*routines internes*/

MEDC_EXPORT med_err
_MEDfieldComputingStepCheck236(med_idt fid,
			       const char * const fieldname,
			       med_size * const ncpst,
			       med_bool checkmultiplemesh, med_bool * const multiplemesh,
			       med_bool checkmeshname, med_bool * const samedefaultmeshname);

MEDC_EXPORT med_int
_MEDfieldnValue(const med_idt fid, const char * const fieldname,const med_int numdt,const med_int numit,
		const med_entity_type entitype, const med_geometry_type geotype,
		char * const profilename, const  int profileit,
		const med_storage_mode storagemode,med_int * const profilesize,
		char * const localizationname, med_int * const nintegrationpoint);

MEDC_EXPORT med_int
_MEDfield23nValue(const med_idt fid, const char * const fieldname,const med_int numdt,const med_int numit,
		const med_entity_type entitype, const med_geometry_type geotype, const char * const meshname,
		char * const profilename, const  int profileit,
		const med_storage_mode storagemode,med_int * const profilesize,
		char * const localizationname, med_int * const nintegrationpoint);

/*fin routines  internes */

/* Family  */
#include "medfamily.h"

/* Equivalence    */
#include "medequivalence.h"

/* Joints */
#include "medsubdomain.h"

/* Profile */

#include "medprofile.h"

/* Filter */
#include "medfilter.h"

/* Localization */
#include "medlocalization.h"

/* Link */
#include "medlink.h"

/* Struct Elements */
#include "medstructelement.h"

/* Variables scalaires */

#include "medparameter.h"

/* Fonctions d'interpolation */

#include "medinterp.h"



#ifdef __cplusplus
}
#endif

#endif /* MED_PROTO_H */




