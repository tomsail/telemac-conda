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

#ifndef MAJ_21_22_H
#define MAJ_21_22_H

#ifdef __cplusplus

#include "MEDerreur.hxx"

#endif

extern void MAJ_21_22_chaine               (char *ancienne_chaine,char *nouvelle_chaine,med_int n);
extern void MAJ_21_22_champs               (med_idt fid);
extern void MAJ_21_22_elements_maillage    (med_idt mid, med_int dimension);
extern void MAJ_21_22_familles_maillage    (med_idt mid);
extern void MAJ_21_22_localisation_Gauss   (med_idt fid,char *nom_modele,med_int ngauss);
extern void MAJ_21_22_maillages            (med_idt fid);
extern void MAJ_21_22_noeuds_maillage      (med_idt mid, med_int dimension);
extern void MAJ_21_22_profils              (med_idt fid,med_int nprofil);

/* depuis la 3.0.0*/
med_err _MED23v30stringConvert(char *chaine30, med_int substrsize30,
			       char *chaine23, med_int substrsize23,
			       med_int nsubstr );

#endif
