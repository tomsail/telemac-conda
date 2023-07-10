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

#ifndef MAJ_231_232_H
#define MAJ_231_232_H


#ifdef __cplusplus
# include "MEDerreur.hxx"
#endif

extern void MAJ_231_232_maillages (med_idt fid);
extern void MAJ_231_232_champs    (med_idt fid);
extern int  MAJ_231_232_chaine    (char * nomi, char * nomf);


extern med_err MED231champNormaliser(med_idt fid, char * nomcha, med_type_champ typcha, med_int ncomp,
				     med_entite_maillage entite);

extern med_err MED231champLireEtUnlink(med_idt fid,char *maa, char *cha, unsigned char *val,med_mode_switch interlace,med_int numco,
				       char * locname, char *profil, med_mode_profil pflmod, 
				       med_entite_maillage type_ent, med_geometrie_element type_geo,
				       med_int numdt, med_int numo);

extern med_err MED231champRefInfoEtRenMaa(med_idt fid,char *champ,
					     med_entite_maillage type_ent, med_geometrie_element type_geo,
					     int indice, med_int numdt, med_int numo,
					     char * maa, med_booleen * local, med_int *ngauss);

extern med_err MED231champInfoEtRen(med_idt fid,int indice,char *champ,
				    med_type_champ *type,char *comp,char *unit, 
				    med_int ncomp);


#endif
