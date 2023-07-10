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

#ifndef MAJ_236_300_H
#define MAJ_236_300_H


#ifdef __cplusplus
# include "MEDerreur.hxx"
#endif

extern void MAJ_236_300_maillages (med_idt fid);


extern int MAJ_236_300_chaine(char * nomi, char * nomf);


extern  int MAJ_236_300_entites(med_idt fid,
				    char * const _pathi,
				    char * const _pathf,
				    const char * const meshname,
				    const med_entity_type enttype);

extern int MAJ_236_300_mesh_datasets(med_idt fid,
				     char * const _pathi,
				     char * const _pathf,
				     const char * const meshname,
				     const med_entity_type   enttype,
				     const med_geometry_type geotype);

extern int MAJ_236_300_string_datasets(med_idt fid,
				       const char * const absdatagrouppath,
				       const char * const datasetname,
				       int           isubstringsize,
				       int           fsubstringsize,
				       int           nsubstring);

extern med_err _MEDconvertStringDatasets(med_idt id,
					 const char *lname,
					 const H5L_info_t *linfo,
					 visitordatas *data);

extern void MAJ_236_300_champs(med_idt fid);

extern med_err MAJ_236_300_fieldOnEntity(med_idt fid, const char * const nomcha, const char * const meshname,
					 med_field_type typcha, med_int ncomp, med_entity_type entite, med_int ncstp,
					 char * const _pathi, char * const _pathf);

extern med_err
MED30linkWr(const med_idt        fid,
	    const char   * const meshname,
	    const char   * const link);

extern med_err
MAJ_236_300_equivalence(med_idt fid,const char * const maa);

extern med_err
MAJ_236_300_joint(med_idt fid, const char * const maa);


#endif
