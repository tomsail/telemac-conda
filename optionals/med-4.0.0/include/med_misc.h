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

#ifndef MED_MISC_H
#define MED_MISC_H

#include <med.h>
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Type fonction MED pour le versionement des APIs */
typedef void (*MedFuncType)(int,...) ;

extern MEDC_EXPORT
MedFuncType    _MEDversionedApi3(const char * const name, const med_int majeur,
				 const med_int mineur, const med_int release);

extern MEDC_EXPORT
med_access_mode _MEDmodeAcces (med_idt oid);
extern MEDC_EXPORT
med_err         _MEDsetModeAcces(med_idt fid, med_access_mode mode);

extern MEDC_EXPORT
med_err           _MEDfileVersionSetCache(const med_idt oid,const med_file_version v);
extern MEDC_EXPORT
med_file_version  _MEDfileVersion(const med_idt oid);

extern MEDC_EXPORT
med_bool _MEDfieldChecked(const med_idt oid,const char * const fieldname);
extern MEDC_EXPORT
med_err _MEDfieldCheckedSetCache(const med_idt      oid,
				 const char * const fieldname,
				 const med_bool         ischecked);

/* CHAINES DE CARACTERES FORTRAN => C */
extern MEDC_EXPORT
med_err _MEDcstring(char *source, char *dest);

extern MEDC_EXPORT
char *_MED1cstring(char *chaine,int longueur_reelle,int longueur_fixee);

extern MEDC_EXPORT
char *_MED2cstring(char *chaine, int longueur);

extern MEDC_EXPORT
med_err _MEDcstringFree(char *chaine);

extern MEDC_EXPORT
med_err _MEDfstring(char *chaine, med_int longueur_fixee);

extern MEDC_EXPORT
med_err _MEDc2fString(const char *   const chainec,
		            char *   const chainef,
		      med_int        longueur_buffer77);


extern MEDC_EXPORT med_err
_MEDgetDatasetName(char * const datasetname,
		   const med_data_type datatype,
		   med_connectivity_mode cmode );

extern MEDC_EXPORT med_err
_MEDgetDatasetParameter(const med_data_type         meddatatype,
			const med_int               spacedim,
			const med_entity_type       entitytype,
			const med_geometry_type     geotype,
			const med_connectivity_mode cmode,
			med_int * const             nvalueperentity,
			med_int * const             nconstituentpervalue);

extern MEDC_EXPORT med_err
_MEDgetDatatype(med_internal_type  * const datatype,
		const med_data_type        meddatatype,
		med_connectivity_mode      cmode );

extern MEDC_EXPORT med_err
_MEDgetEntityTypeName(char * const entitytypename,
		      const med_entity_type entitytype);
extern MEDC_EXPORT med_err
_MEDgetFieldEntityGeoTypeName(med_idt fid,
			      char * const entitygeotypename,
			      const med_entity_type entitytype,
			      const med_geometry_type geotype );
  
extern MEDC_EXPORT med_err
_MEDgetGeometricParameter(const med_entity_type       entitytype,
			  const med_geometry_type     geotype,
			  med_int * const             entdim,
			  med_int * const             nnodes,
			  med_int * const             nndes);

extern MEDC_EXPORT med_err
_MEDgetDynGeometricParameter(const med_idt fid,
			     const med_entity_type       entitytype,
			     const med_geometry_type     geotype,
			     med_int * const             entdim,
			     med_int * const             nnodes,
			     med_int * const             ncells);

extern MEDC_EXPORT med_err
_MEDgetInternalGeometryTypeName(const med_idt fid, char * const geotypename, med_geometry_type geotype);

extern MEDC_EXPORT med_err
_MEDgetExternalGeometryTypeName(char * const geotypename,med_geometry_type geotype);

extern MEDC_EXPORT med_err
_MEDmeshtypeCompatibility(const med_data_type meddatatype,
			  const med_mesh_type meshtype );

extern MEDC_EXPORT med_err
_MEDsetFilter(const med_int nspaces, const med_idt* const memspace,
	      const med_idt *const diskspace, const med_int nentity,
	      const med_int nvaluesperentity, const med_int nconstituentpervalue,
	      const med_int constituentselect, const med_switch_mode switchmode,
	      const med_int filterarraysize, const med_int profilearraysize, const med_storage_mode storagemode,
	      const char* const profilename, med_filter* const filter);

extern MEDC_EXPORT med_err
_MEDcheckVersion30(med_idt fid);

/*From C++*/
extern MEDC_EXPORT med_err
MEDversionLire(med_idt fid, med_int *majeur, med_int *mineur, med_int *release);

extern MEDC_EXPORT
med_geometry_type MEDgetGeometryTypeFromName(const char * const keycharpart);

extern MEDC_EXPORT
med_err _MEDgetSupportMeshNbOfEntities(med_idt                   fid,
				       const char * const        smeshname,
				       med_entity_type * const   smeshentitype,
				       med_geometry_type * const smeshgeotype,
				       char * const              smeshgeotypename,
				       med_int * const           smeshnentity);

/* Gestion des messages d'erreur */
/* extern MEDC_EXPORT */
/* void exit_if(char * fichier,int ligne, int condition,char * message, char * arg); */
#include <med_exit_if.h>

#ifdef __cplusplus
}
#endif

#endif /* MED_MISC_H */

