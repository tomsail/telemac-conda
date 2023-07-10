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

#ifndef MED_HDFI_H
#define MED_HDFI_H
#include <med.h>

#ifdef __cplusplus
extern "C" {
#endif


/* FONCTIONS INTERFACE MED/HDF */


/* Gestion des attributs HDF */
extern MEDC_EXPORT
med_idt _MEDattrOuvrir(med_idt pid,char * nom);

extern MEDC_EXPORT
med_err _MEDattributeExist(const med_idt        gid,
			   const char * const   datagroupname,
			   const char * const   attributename,
			   med_bool *   const   attributeexist );
extern MEDC_EXPORT
med_err _MEDattrFermer(med_idt id);

extern MEDC_EXPORT
med_err _MEDattrNumEcrire(med_idt pere,med_field_type type,char *nom,unsigned char *val);

#define _MEDattributeIntWr(w,x,y)  _MEDattributeNumWr(w,x, MED_INTERNAL_INT, (unsigned char *) y)
#define _MEDattrFloatEcrire(w,x,y)   _MEDattrNumEcrire(w,MED_FLOAT64,x,(unsigned char *) y)

extern MEDC_EXPORT
med_err _MEDattributeNumWr(med_idt pid,
			   const char * const attname,
			   const med_internal_type type,
			   const unsigned char * const  val);

extern MEDC_EXPORT
med_err _MEDattributeNumWrByName(med_idt pid,
				 const char * const path ,
				 const char * const attname,
				 const med_internal_type type,
				 const unsigned char * const  val);

extern MEDC_EXPORT
med_err _MEDattrNumLire(med_idt pere,med_field_type type,const char * const nom,unsigned char *val);

#define _MEDattrEntierLire(x,y,z) _MEDattrNumLire(x,MED_INT ,y,(unsigned char*)z)
#define _MEDattrFloatLire(x,y,z)  _MEDattrNumLire(x,MED_FLOAT64,y,(unsigned char*)z)

extern MEDC_EXPORT
med_err _MEDattributeNumRdByName(med_idt pid,
				 const char * const path ,
				 const char * const attname,
				 const med_internal_type type,
				 unsigned char * const val);

extern MEDC_EXPORT
med_err _MEDattrStringEcrire(med_idt pere,const char * const nom,int longueur,const char * const val);

extern MEDC_EXPORT
med_err _MEDattributeStringWrByName(med_idt pid,
				    const char * const path,
				    const char * const attname,
				    const med_size attsize,
				    const char * const val);
extern MEDC_EXPORT
med_err _MEDattributeStringWr(med_idt pid,
			      const char * const attname,
			      const med_size attsize,
			      const char * const val);
extern MEDC_EXPORT

med_err _MEDattrStringLire(med_idt pere,char *nom,int longueur,char *val);

extern MEDC_EXPORT
med_err _MEDattributeStringRdByName(med_idt pid,
				    const char * const path ,
				    const char * const attname,
				    const med_size attsize,
				    char * const val);


/* Gestion des datagroups HDF */
extern MEDC_EXPORT
med_idt _MEDdatagroupCreer(med_idt pid, const char * const nom);

extern MEDC_EXPORT
med_idt _MEDdatagroupCrOrderCr(const med_idt pid, const char * const name);

extern MEDC_EXPORT
med_idt _MEDdatagroupOuvrir(med_idt pid, const char * const nom);

extern MEDC_EXPORT
med_idt _MEDdatagroupOpen(const med_idt pid, const char * const name);

extern MEDC_EXPORT
med_err _MEDdatagroupFermer(med_idt id);

extern MEDC_EXPORT
med_err _MEDdatagroupLienCreer(med_idt id,const char *nom, const char *nom_lien);

extern MEDC_EXPORT
med_err _MEDdatagroupLienSupprimer(med_idt id,const char *nom_lien);


/* Gestion des datasets HDF */

extern MEDC_EXPORT
med_idt _MEDdatasetOuvrir(med_idt pid,char *nom);

extern MEDC_EXPORT
med_err _MEDdatasetFermer(med_idt id);

extern MEDC_EXPORT
med_err _MEDdatasetWr(const med_idt               id,
			 const char * const          datasetname,
			 const med_internal_type     datatype,
			 const med_filter* const     filter,
			 const void * const value);
extern MEDC_EXPORT
med_err _MEDdatasetRd(const med_idt               id,
			 const char * const          datasetname,
			 const med_internal_type     datatype,
			 const med_filter* const     filter,
			 unsigned char * const value);

/* Gestion des fichiers HDF */
#ifdef MED_HAVE_MPI

extern MEDC_EXPORT
med_idt _MEDparFileOpen(const char * const filename,const med_access_mode accessmode,
			const MPI_Comm comm, const MPI_Info info);

extern MEDC_EXPORT
med_idt _MEDparFileCreate(const char * const filename, const med_access_mode accessmode,
			  const MPI_Comm comm, const MPI_Info info);
#endif

extern MEDC_EXPORT
med_idt _MEDfileOpen(const char * const filename,const med_access_mode accessmode);


extern MEDC_EXPORT
med_int _MEDfileName(med_idt id, char * const filename, const med_int filenamesize);

extern MEDC_EXPORT
med_idt _MEDfileCreate(const char * const filename, const med_access_mode accessmode, const med_int major, const med_int minor, const med_int release);

extern MEDC_EXPORT
med_idt _MEDmemFileOpen(const char * const filename, med_memfile * const memfile, const med_bool filesync, const med_access_mode accessmode);

extern MEDC_EXPORT
med_err _MEDfichierFermer(med_idt fid);

extern MEDC_EXPORT
med_err _MEDfichierMonter(med_idt pid, const char *nom, med_idt fid);

extern MEDC_EXPORT
med_err _MEDfichierDemonter(med_idt pid, const char *nom);

extern MEDC_EXPORT
med_err _MEDfichierNo(med_idt id, unsigned long * fileno);

/* Filtres */
extern MEDC_EXPORT
med_err _MEDfilterEntityFullIGlobalCr(const med_idt          fid,
				const med_int          nentity,
				const med_int          nvaluesperentity,
				const med_int          nconstituentpervalue,
				const med_int          constituentselect,
				const med_storage_mode storagemode,
				const char * const     profilename,
				const med_int          filterarraysize,
				const med_int* const   filterarray,
				med_filter*    const filter);
extern MEDC_EXPORT
med_err _MEDfilterEntityFullICompactCr(const med_idt          fid,
				       const med_int          nentity,
				       const med_int          nvaluesperentity,
				       const med_int          nconstituentpervalue,
				       const med_int          constituentselect,
				       const med_storage_mode storagemode,
				       const char * const     profilename,
				       const med_int          filterarraysize,
				       const med_int* const   filterarray,
				       med_filter*    const   filter);
extern MEDC_EXPORT
med_err _MEDfilterEntityNoIGlobalCr(const med_idt          fid,
				    const med_int          nentity,
				    const med_int          nvaluesperentity,
				    const med_int          nconstituentpervalue,
				     const med_int          constituentselect,
				    const med_storage_mode storagemode,
				    const char * const     profilename,
				    const med_int          filterarraysize,
				    const med_int* const   filterarray,
				    med_filter* const      filter);

extern MEDC_EXPORT
med_err _MEDfilterEntityNoICompactCr(const med_idt          fid,
				     const med_int          nentity,
				     const med_int          nvaluesperentity,
				     const med_int          nconstituentpervalue,
				     const med_int          constituentselect,
				     const med_storage_mode storagemode,
				     const char * const     profilename,
				     const med_int          filterarraysize,
				     const med_int* const   filterarray,
				     med_filter* const      filter);

/* extern MEDC_EXPORT */
/* med_err _MEDselectAllEntities(const med_idt          fid, */
/* 			      const med_int          nentity, */
/* 			      const med_int          nvaluesperentity, */
/* 			      const med_int          nconstituentpervalue, */
/* 			      const med_int          constituentselect, */
/* 			      med_filter*    const   filter); */


extern MEDC_EXPORT
med_err _MEDselectAllEntitiesFullI(const med_idt          fid,
				   const med_int          nentity,
				   const med_int          nvaluesperentity,
				   const med_int          nconstituentpervalue,
				   const med_int          constituentselect,
				   med_filter*    const   filter);

extern MEDC_EXPORT
med_err _MEDselectAllEntitiesNoI(const med_idt          fid,
				 const med_int          nentity,
				 const med_int          nvaluesperentity,
				 const med_int          nconstituentpervalue,
				 const med_int          constituentselect,
				 med_filter*    const   filter);

extern MEDC_EXPORT
med_err _MEDfilterBlockOfEntityFullICompactCr(const med_idt          fid,
					      const med_int          nentity,
					      const med_int          nvaluesperentity,
					      const med_int          nconstituentpervalue,
					      const med_int          constituentselect,
					      const med_storage_mode storagemode,
					      const char * const     profilename,
					      const med_size  start,
					      const med_size  stride,
					      const med_size  count,
					      const med_size  blocksize,
					      const med_size  lastblocksize,
					      med_filter*    const   filter);

extern MEDC_EXPORT
med_err _MEDfilterBlockOfEntityFullIGlobalCr(const med_idt          fid,
					      const med_int          nentity,
					      const med_int          nvaluesperentity,
					      const med_int          nconstituentpervalue,
					      const med_int          constituentselect,
					      const med_storage_mode storagemode,
					      const char * const     profilename,
					      const med_size  start,
					      const med_size  stride,
					      const med_size  count,
					      const med_size  blocksize,
					      const med_size  lastblocksize,
					      med_filter*    const   filter);
extern MEDC_EXPORT
med_err _MEDfilterBlockOfEntityNoIGlobalCr(const med_idt          fid,
					      const med_int          nentity,
					      const med_int          nvaluesperentity,
					      const med_int          nconstituentpervalue,
					      const med_int          constituentselect,
					      const med_storage_mode storagemode,
					      const char * const     profilename,
					      const med_size  start,
					      const med_size  stride,
					      const med_size  count,
					      const med_size  blocksize,
					      const med_size  lastblocksize,
					      med_filter*    const   filter);
extern MEDC_EXPORT
med_err _MEDfilterBlockOfEntityNoICompactCr(const med_idt          fid,
					      const med_int          nentity,
					      const med_int          nvaluesperentity,
					      const med_int          nconstituentpervalue,
					      const med_int          constituentselect,
					      const med_storage_mode storagemode,
					      const char * const     profilename,
					      const med_size  start,
					      const med_size  stride,
					      const med_size  count,
					      const med_size  blocksize,
					      const med_size  lastblocksize,
					      med_filter*    const   filter);


/* Divers */

extern MEDC_EXPORT
med_err _MEDnObjects(const med_idt fid,const char * const path,med_size *n);

extern MEDC_EXPORT
med_err _MEDobjectGetName(const med_idt fid,const char * const path,const med_size ind,char *name);

extern MEDC_EXPORT
med_err
_MEDobjectCrOrderGetName(const med_idt fid,const char * const path,const med_size ind,char *name);

extern MEDC_EXPORT
med_err
__MEDobjectGetName(const med_idt fid, const char * const path, const med_size ind, char *name,
		   const H5_index_t index_type, const H5_iter_order_t order );


typedef struct {
  char * srcpath;
  char * dstpath;
  med_idt gid1;
  med_idt gid2;
} visitordatas;

typedef struct {
  char *   attname;
  char *   attval;
  char *   attvalprec;
  med_int  attsize;
} med_string_itdatas;


extern MEDC_EXPORT
void * _MEDcheckAttributeStringInit( med_string_itdatas * const itdatas, const char * const attname, med_int attsize);
extern MEDC_EXPORT
med_err _MEDcheckAttributeStringFunc(med_idt id,const char *lname, const H5L_info_t *linfo, med_string_itdatas *data);
extern MEDC_EXPORT
void  _MEDcheckAttributeStringFin( med_string_itdatas * const itdatas);

extern MEDC_EXPORT
med_err _MEDchecknSublinkFunc(med_idt id,const char *lname, const H5L_info_t *linfo, med_bool *data);

#define MED_CHECK_ATTRIBUTE_FUNC(_check_med_type_) _MEDcheckAttributeFunc##_check_med_type_
#define MED_CHECK_ATTRIBUTE_INIT(_check_med_type_,_check_med_itdata_,_check_med_attname_,_check_med_attsize_) _MEDcheckAttributeInit##_check_med_type_ (_check_med_itdata_,_check_med_attname_,_check_med_attsize_)


extern MEDC_EXPORT
med_err _MEDcopyName(med_idt id,const char *lname, const H5L_info_t *linfo, void *data);

extern MEDC_EXPORT
med_err _MEDlinkobjs(med_idt id,const char *lname, const H5L_info_t *linfo, visitordatas *data);

extern MEDC_EXPORT
med_err _MEDsoftlinkDel(const med_idt               id,	const char * const          softlinkname,
			med_bool                    linkmustexist);
extern MEDC_EXPORT
med_err _MEDisasoftlink(const med_idt               id,
			const char * const          linkname,
			med_bool                    linkmustexist,
			med_bool *   const          isasoftlink
			);
extern MEDC_EXPORT
med_err _MEDgetDatasetChgt(const med_idt                       gid,
			   const med_data_type                 meddatatype,
			   const med_connectivity_mode         cmode,
			   med_bool *                  const   isasoftlink,
			   med_bool *                  const   chgt );

extern MEDC_EXPORT
med_err _MEDdatasetExistByMedtype(const med_idt                       gid,
			 const med_data_type                 meddatatype,
			 const med_connectivity_mode         cmode,
			 med_bool *                  const   datasetexist,
			 med_bool *                  const   isasoftlink );

extern MEDC_EXPORT
med_err _MEDdatagroupExist(const med_idt        gid,
			   const char * const   datagroupname,
			   med_bool *   const   datagroupexist,
			   med_bool *   const   isasoftlink );
extern MEDC_EXPORT
med_err _MEDdatasetExist(const med_idt       gid,
			 const char *  const datasetname,
			 med_bool   *  const datasetexist,
			 med_bool   *  const isasoftlink );

typedef herr_t (*medvisitorfunc)( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data);
typedef medvisitorfunc mediteratorfunc;

extern MEDC_EXPORT
med_err _MEDvisit(const med_idt fid, const char * const srcpath, const char * const dstpath,
		  medvisitorfunc);
extern MEDC_EXPORT
med_err _MEDiterate(const med_idt fid, herr_t (*func)(), void * itdatas );

extern MEDC_EXPORT
med_err
_MEDgetComputationStepName(const med_sorting_type sortingtype, const med_int numdt,
			   const med_int numit, char * const datagroupname);

extern MEDC_EXPORT
void _MEDmodeErreurVerrouiller(void);

extern MEDC_EXPORT
void _MEDobjetsOuverts(med_idt fid);
#ifdef __cplusplus
}
#endif

#endif /* MED_HDFI_H */
