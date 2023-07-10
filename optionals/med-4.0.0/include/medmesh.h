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

#ifndef MED_MEDMESH_H
#define MED_MEDMESH_H

#include "medC_win_dll.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Interface de l'API MED */

MEDC_EXPORT med_err
MEDmeshCr(const med_idt fid,
	  const char * const meshname, const med_int spacedim,
	  const med_int meshdim, const med_mesh_type meshtype,
	  const char * const description, const char * const dtunit,
	  const med_sorting_type sortingtype,
	  const med_axis_type axistype, const char * const axisname,
	  const char * const axisunit);


MEDC_EXPORT med_err
MEDmeshInfoByName(const med_idt fid,const char * const meshname,
		  med_int * const spacedim, med_int * const meshdim,  med_mesh_type * const meshtype,
		  char * const description,  char * const dtunit,
		  med_sorting_type * const sortingtype,
		  med_int * const nstep,  med_axis_type * const axistype,  char * const axisname,
		  char * const axisunit);

MEDC_EXPORT med_err
MEDmeshInfo(const med_idt            fid,
	    const int                meshit,
	    char   *           const meshname,
	    med_int *          const spacedim,
	    med_int *          const meshdim,
	    med_mesh_type *    const meshtype,
	    char *             const description,
	    char *             const dtunit,
	    med_sorting_type * const sortingtype,
	    med_int *          const nstep,
	    med_axis_type *    const axistype,
	    char *             const axisname,
	    char *             const axisunit);

MEDC_EXPORT med_int
MEDnMesh(const med_idt fid);

MEDC_EXPORT med_int
MEDmeshnAxis(const med_idt fid, const int meshit);

MEDC_EXPORT med_int
MEDmeshnAxisByName(const med_idt fid, const char * const meshname);

MEDC_EXPORT med_err
MEDmeshGridTypeWr(const med_idt fid,const char * const meshname, const med_grid_type gridtype);

MEDC_EXPORT med_err
MEDmeshGridTypeRd(const med_idt fid,const char * const meshname, med_grid_type * const gridtype);

MEDC_EXPORT med_err
MEDmeshGridIndexCoordinateWr(const med_idt               fid,
			     const char*  const          meshname,
			     const med_int               numdt,
			     const med_int               numit,
			     const med_float             dt,
			     const med_int               axis,
			     const med_int               indexsize,
			     const med_float * const     gridindex);
MEDC_EXPORT med_err
MEDmeshGridIndexCoordinateRd(const med_idt               fid,
			     const char*  const          meshname,
			     const med_int               numdt,
			     const med_int               numit,
			     const med_int               axis,
			     med_float * const     gridindex);

MEDC_EXPORT med_err
MEDmeshGridStructWr(const med_idt               fid,
		    const char*  const          meshname,
		    const med_int               numdt,
		    const med_int               numit,
		    const med_float             dt,
		    const med_int * const       gridstruct);

MEDC_EXPORT med_err
MEDmeshGridStructRd(const med_idt               fid,
		    const char*  const          meshname,
		    const med_int               numdt,
		    const med_int               numit,
		    med_int * const       gridstruct);

MEDC_EXPORT med_err
MEDmeshUniversalNameWr(const med_idt fid, const char * const meshname);

MEDC_EXPORT med_err
MEDmeshUniversalNameRd(const med_idt fid, const char * const meshname,char * const univname);

MEDC_EXPORT med_err
MEDmeshComputationStepCr(const med_idt fid,const char * const meshname,
			 const med_int numdt1, const med_int numit1,
			 const med_int numdt2, const med_int numit2,
			 const med_float dt2 );

MEDC_EXPORT med_err
MEDmeshAttributeRd(const med_idt fid,
		   const char * const meshname,
		   med_int    * const isolatednodes,
		   med_int    * const verticesnodes,
		   med_int    * const cellmaxnodes);

MEDC_EXPORT med_err
MEDmeshAttributeWr(const med_idt fid, const char * const meshname,
		   const med_int isolatednodes,const med_int verticesnodes,const med_int cellmaxnodes);

MEDC_EXPORT med_err
MEDmeshComputationStepDtRd(const med_idt fid,const char * const meshname,
			   const med_int numdt, const med_int umit, med_float * const dt );

MEDC_EXPORT med_err
MEDmeshComputationStepInfo(const med_idt fid,
			   const char * const meshname,
			   const int csit,
			   med_int   * const numdt,
			   med_int   * const numit,
			   med_float * const  dt );

MEDC_EXPORT med_err
MEDmeshSortingTypeRd(const med_idt               fid,
		     const char*  const          meshname,
		     med_sorting_type * const    sortingtype );


MEDC_EXPORT med_err
MEDmeshNodeCoordinateAdvancedWr(const med_idt               fid,
				const char*  const          meshname,
				const med_int               numdt,
				const med_int               numit,
				const med_float             dt,
				const med_filter * const    filter,
				const med_float* const  value);
MEDC_EXPORT med_err
MEDmeshNodeCoordinateWithProfileWr(const med_idt               fid,
				   const char*  const          meshname,
				   const med_int               numdt,
				   const med_int               numit,
				   const med_float             dt,
				   const med_storage_mode      storagemode,
				   const char * const          profilename,
				   const med_switch_mode       switchmode,
				   const med_int               dimselect,
				   const med_int               nentity,
				   const med_float* const      coordinates);
MEDC_EXPORT med_err
MEDmeshNodeCoordinateWr(const med_idt               fid,
				   const char*  const          meshname,
				   const med_int               numdt,
				   const med_int               numit,
				   const med_float             dt,
				   const med_switch_mode       switchmode,
				   const med_int               nentity,
				   const med_float* const  coordinates);

MEDC_EXPORT med_err
MEDmeshNodeCoordinateTrsfWr(const med_idt               fid,
			    const char*  const          meshname,
			    const med_int               numdt,
			    const med_int               numit,
			    const med_float             dt,
			    const med_float* const      coordinatetrsf);
MEDC_EXPORT med_err
MEDmeshNodeCoordinateTrsfRd(const med_idt               fid,
				    const char*  const          meshname,
				    const med_int               numdt,
				    const med_int               numit,
				    const med_float* const      coordinatetrsf);
MEDC_EXPORT med_err
MEDmeshElementConnectivityWr(const med_idt               fid,
			     const char*  const          meshname,
			     const med_int               numdt,
			     const med_int               numit,
			     const med_float             dt,
			     const med_entity_type       entitype,
			     const med_geometry_type     geotype,
			     const med_connectivity_mode cmode,
			     const med_switch_mode       switchmode,
			     const med_int               nentity,
			     const med_int* const  connectivity);

MEDC_EXPORT med_err
MEDmeshElementConnectivityAdvancedWr(const med_idt               fid,
				     const char*  const          meshname,
				     const med_int               numdt,
				     const med_int               numit,
				     const med_float             dt,
				     const med_entity_type       entitype,
				     const med_geometry_type     geotype,
				     const med_connectivity_mode cmode,
				     const med_filter * const    filter,
				     const med_int* const  connectivity);
MEDC_EXPORT med_err
MEDmeshElementConnectivityWithProfileWr(const med_idt               fid,
					const char*  const          meshname,
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
					const med_int               nentity,
					const med_int* const  connectivity);

MEDC_EXPORT med_err
MEDmeshNodeCoordinateAdvancedRd(const med_idt               fid,
				const char*  const          meshname,
				const med_int               numdt,
				const med_int               numit,
				const med_filter * const    filter,
				med_float* const  value);
MEDC_EXPORT med_err
MEDmeshNodeCoordinateWithProfileRd(const med_idt               fid,
				   const char*  const          meshname,
				   const med_int               numdt,
				   const med_int               numit,
				   const med_storage_mode      storagemode,
				   const char * const          profilename,
				   const med_switch_mode       switchmode,
				   const med_int               dimselect,
				   med_float* const  coordinates);
MEDC_EXPORT med_err
MEDmeshNodeCoordinateRd(const med_idt               fid,
			const char*  const          meshname,
			const med_int               numdt,
			const med_int               numit,
			const med_switch_mode       switchmode,
			med_float* const  coordinates);

MEDC_EXPORT med_err
MEDmeshElementConnectivityRd(const med_idt               fid,
			     const char*  const          meshname,
			     const med_int               numdt,
			     const med_int               numit,
			     const med_entity_type       entitype,
			     const med_geometry_type     geotype,
			     const med_connectivity_mode cmode,
			     const med_switch_mode       switchmode,
			     med_int* const  connectivity);
MEDC_EXPORT med_err
MEDmeshElementConnectivityAdvancedRd(const med_idt               fid,
				     const char*  const          meshname,
				     const med_int               numdt,
				     const med_int               numit,
				     const med_entity_type       entitype,
				     const med_geometry_type     geotype,
				     const med_connectivity_mode cmode,
				     const med_filter * const    filter,
				     med_int* const  connectivity);
MEDC_EXPORT med_err
MEDmeshElementConnectivityWithProfileRd(const med_idt               fid,
					const char*  const          meshname,
					const med_int               numdt,
					const med_int               numit,
					const med_entity_type       entitype,
					const med_geometry_type     geotype,
					const med_connectivity_mode cmode,
					const med_storage_mode      storagemode,
					const char * const          profilename,
					const med_switch_mode       switchmode,
					const med_int               dimselect,
					const med_int               nentity,
					med_int* const  connectivity);

/*Cr~er une version sans aucun param~tre concernant les s~quences de calcul*/
MEDC_EXPORT med_int
MEDmeshnEntity(const med_idt fid,
	       const char * const meshname,
	       const med_int numdt,
	       const med_int numit,
	       const med_entity_type entitype,
	       const med_geometry_type geotype,
	       const med_data_type datatype,
	       const med_connectivity_mode cmode,
	       med_bool * const changement,
	       med_bool * const transformation );

MEDC_EXPORT med_int
MEDmeshnEntityWithProfile(const med_idt fid,
			  const char * const meshname,
			  const med_int numdt,
			  const med_int numit,
			  const med_entity_type entitype,
			  const med_geometry_type geotype,
			  const med_data_type datatype,
			  const med_connectivity_mode cmode,
			  const med_storage_mode storagemode,
			  char * const profilename,
			  med_int * const profilesize,
			  med_bool * const changement,
			  med_bool * const transformation );

MEDC_EXPORT med_err
MEDmeshEntityInfo(const med_idt                   fid,
		  const char *              const meshname,
		  const med_int                   numdt,
		  const med_int                   numit,
		  const med_entity_type           entitype,
		  const int                       geotypeit,
		  char              *       const geotypename,
		        med_geometry_type * const geotype
		  );

MEDC_EXPORT med_err
MEDmeshEntityNameWr(const med_idt               fid,
		    const char*  const          meshname,
		    const med_int               numdt,
		    const med_int               numit,
		    const med_entity_type       entitype,
		    const med_geometry_type     geotype,
		    const med_int               nentity,
		    const char* const           name);

MEDC_EXPORT med_err
MEDmeshEntityNameRd(const med_idt              fid,
		    const char*  const         meshname,
		    const med_int              numdt,
		    const med_int              numit,
		    const med_entity_type      entitype,
		    const med_geometry_type    geotype,
		    char* const                name);

MEDC_EXPORT med_err
MEDmeshEntityNumberWr(const med_idt               fid,
		      const char*  const          meshname,
		      const med_int               numdt,
		      const med_int               numit,
		      const med_entity_type       entitype,
		      const med_geometry_type     geotype,
		      const med_int               nentity,
		      const med_int * const       number);

MEDC_EXPORT med_err
MEDmeshEntityNumberRd(const med_idt               fid,
		      const char*  const          meshname,
		      const med_int               numdt,
		      const med_int               numit,
		      const med_entity_type       entitype,
		      const med_geometry_type     geotype,
		      med_int * const             number);

MEDC_EXPORT med_err
MEDmeshEntityFamilyNumberWr(const med_idt               fid,
			    const char*  const          meshname,
			    const med_int               numdt,
			    const med_int               numit,
			    const med_entity_type       entitype,
			    const med_geometry_type     geotype,
			    const med_int               nentity,
			    const med_int * const       number);
MEDC_EXPORT med_err
MEDmeshEntityFamilyNumberRd(const med_idt               fid,
			    const char*  const          meshname,
			    const med_int               numdt,
			    const med_int               numit,
			    const med_entity_type       entitype,
			    const med_geometry_type     geotype,
			    med_int * const             number);

MEDC_EXPORT med_err
MEDmeshEntityAttributeAdvancedRd(const med_idt               fid,
				 const char*  const          meshname,
				 const med_data_type         datatype,
				 const med_int               numdt,
				 const med_int               numit,
				 const med_entity_type       entitype,
				 const med_geometry_type     geotype,
				 const med_filter * const    filter,
				 void * const                attval);

MEDC_EXPORT med_err
MEDmeshEntityAttributeAdvancedWr(const med_idt               fid,
				 const char*  const          meshname,
				 const med_data_type         datatype,
				 const med_int               numdt,
				 const med_int               numit,
				 const med_entity_type       entitype,
				 const med_geometry_type     geotype,
				 const med_filter * const    filter,
				 const void * const          attval);

MEDC_EXPORT med_err
MEDmeshPolygonWr(const med_idt               fid,
		 const char*  const          meshname,
		 const med_int               numdt,
		 const med_int               numit,
		 const med_float             dt,
		 const med_entity_type       entitype,
		 const med_connectivity_mode cmode,
		 const med_int               indexsize,
		 const med_int * const       polyindex,
		 const med_int * const       connectivity );

MEDC_EXPORT med_err
MEDmeshPolygon2Wr(const med_idt               fid,
		  const char*  const          meshname,
		  const med_int               numdt,
		  const med_int               numit,
		  const med_float             dt,
		  const med_entity_type       entitype,
		  const med_geometry_type     polytype,
		  const med_connectivity_mode cmode,
		  const med_int               indexsize,
		  const med_int * const       polyindex,
		  const med_int * const       connectivity );

MEDC_EXPORT med_err
MEDmeshPolygonRd(const med_idt               fid,
		 const char*  const          meshname,
		 const med_int               numdt,
		 const med_int               numit,
		 const med_entity_type       entitype,
		 const med_connectivity_mode cmode,
		 med_int * const             polyindex,
		 med_int * const             connectivity );

MEDC_EXPORT med_err
MEDmeshPolygon2Rd(const med_idt               fid,
		  const char*  const          meshname,
		  const med_int               numdt,
		  const med_int               numit,
		  const med_entity_type       entitype,
		  const med_geometry_type     polytype,
		  const med_connectivity_mode cmode,
		  med_int * const             polyindex,
		  med_int * const             connectivity );

MEDC_EXPORT med_err
MEDmeshPolyhedronRd(const med_idt               fid,
		    const char*  const          meshname,
		    const med_int               numdt,
		    const med_int               numit,
		    const med_entity_type       entitype,
		    const med_connectivity_mode cmode,
		    med_int * const             faceindex,
		    med_int * const             nodeindex,
		    med_int * const             connectivity );

MEDC_EXPORT med_err
MEDmeshPolyhedronWr(const med_idt               fid,
		    const char*  const          meshname,
		    const med_int               numdt,
		    const med_int               numit,
		    const med_float             dt,
		    const med_entity_type       entitype,
		    const med_connectivity_mode cmode,
		    const med_int               faceindexsize,
		    const med_int * const       faceindex,
		    const med_int               nodeindexsize,
		    const med_int * const       nodeindex,
		    const med_int * const       connectivity );

MEDC_EXPORT med_err
MEDmeshGeotypeName(const med_idt                 fid,
		   const med_geometry_type       geotype,
		   char *                  const geotypename);
MEDC_EXPORT med_err
MEDmeshGeotypeParameter(const med_idt                 fid,
			const med_geometry_type       geotype,
			med_int *               const geodim,
			med_int *               const nnodes);

MEDC_EXPORT
med_err MEDmeshGlobalNumberWr(const med_idt               fid,
			      const char*  const          meshname,
			      const med_int               numdt,
			      const med_int               numit,
			      const med_entity_type       entitytype,
			      const med_geometry_type     geotype,
			      const med_int               nentity,
			      const med_int * const       number);
MEDC_EXPORT
med_err MEDmeshGlobalNumberRd(const med_idt               fid,
			      const char*  const          meshname,
			      const med_int               numdt,
			      const med_int               numit,
			      const med_entity_type       entitytype,
			      const med_geometry_type     geotype,
			      med_int * const             number);

/* Routines de niveau intermediaire */
MEDC_EXPORT
med_err MEDmeshNodeWr(const med_idt                  fid,
		      const char            * const  meshname,
		      const med_int                  numdt,
		      const med_int                  numit,
		      const med_float                dt,
		      const med_switch_mode          switchmode,
		      const med_int                  nentity,
		      const med_float       * const  coordinate,
		      const med_bool                 withnodename,
		      const char            * const  nodename,
		      const med_bool                 withnodenumber,
		      const med_int         * const  nodenumber,
		      const med_bool                 withfamnumber,
		      const med_int         * const  famnumber);

MEDC_EXPORT
med_err MEDmeshNodeRd(const med_idt                  fid,
		      const char            * const  meshname,
		      const med_int                  numdt,
		      const med_int                  numit,
		      const med_switch_mode          switchmode,
		      med_float             * const  coordinate,
		      med_bool              * const  withnodename,
		      char                  * const  nodename,
		      med_bool              * const  withnodenumber,
		      med_int               * const  nodenumber,
		      med_bool              * const  withfamnumber,
		      med_int               * const  famnumber);

MEDC_EXPORT
med_err MEDmeshElementWr(const med_idt                  fid,
			 const char            * const  meshname,
			 const med_int                  numdt,
			 const med_int                  numit,
			 const med_float                dt,
			 const med_entity_type          entitype,
			 const med_geometry_type        geotype,
			 const med_connectivity_mode    cmode,
			 const med_switch_mode          switchmode,
			 const med_int                  nentity,
			 const med_int         * const  connectivity,
			 const med_bool                 withelementname,
			 const char            * const  elementname,
			 const med_bool                 withelementnumber,
			 const med_int         * const  elementnumber,
			 const med_bool                 withfamnumber,
			 const med_int         * const  famnumber);
MEDC_EXPORT
med_err MEDmeshElementRd(const med_idt                  fid,
			 const char            * const  meshname,
			 const med_int                  numdt,
			 const med_int                  numit,
			 const med_entity_type          entitype,
			 const med_geometry_type        geotype,
			 const med_connectivity_mode    cmode,
			 const med_switch_mode          switchmode,
			 med_int               * const  connectivity,
			 med_bool              * const  withelementname,
			 char                  * const  elementname,
			 med_bool              * const  withelementnumber,
			 med_int               * const  elementnumber,
			 med_bool              * const  withfamnumber,
			 med_int               * const  famnumber);

/* Maillages support aux modèles d'éléments de structure */

MEDC_EXPORT med_err
MEDsupportMeshCr(const med_idt       fid,
		 const char* const   supportmeshname,
		 const med_int       spacedim,
		 const med_int       meshdim,
		 const char* const   description,
		 const med_axis_type axistype,
		 const char* const   axisname,
		 const char* const   axisunit
		 );



MEDC_EXPORT med_err
MEDsupportMeshInfoByName(const med_idt         fid,
			 const char *    const supportmeshname,
			 med_int *       const spacedim,
			 med_int *       const meshdim,
			 char *          const description,
			 med_axis_type * const axistype,
			 char *          const axisname,
			 char *          const axisunit);

MEDC_EXPORT med_err
MEDsupportMeshInfo(const med_idt            fid,
		   const int                meshit,
		   char   *           const supportmeshname,
		   med_int *          const spacedim,
		   med_int *          const meshdim,
		   char *             const description,
		   med_axis_type *    const axistype,
		   char *             const axisname,
		   char *             const axisunit);

MEDC_EXPORT med_int
MEDnSupportMesh(const med_idt fid);

MEDC_EXPORT med_int
MEDsupportMeshnAxis(const med_idt fid, const int meshit);

MEDC_EXPORT med_int
MEDsupportMeshnAxisByName(const med_idt fid, const char * const meshname);


#ifdef __cplusplus
}
#endif

#endif /* MED_MEDMESH_H */

