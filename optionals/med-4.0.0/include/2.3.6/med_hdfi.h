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

/* Gestion des fichiers HDF */
extern MEDC_EXPORT
med_idt _MEDfichierCreer(char *nom, med_mode_acces mode);

extern MEDC_EXPORT
med_idt _MEDfichierOuvrir(char *nom,med_mode_acces mode);

extern MEDC_EXPORT
med_err _MEDfichierFermer(med_idt fid);

extern MEDC_EXPORT
med_err _MEDfichierNo(med_idt id, unsigned long * fileno);

extern MEDC_EXPORT
med_err _MEDfichierMonter(med_idt pid, const char *nom, med_idt fid);

extern MEDC_EXPORT 
med_err _MEDfichierDemonter(med_idt pid, const char *nom);  


/* Gestion des datagroups HDF */
extern MEDC_EXPORT 
med_idt _MEDdatagroupCreer(med_idt pid, char *nom);

extern MEDC_EXPORT 
med_idt _MEDdatagroupOuvrir(med_idt pid, char *nom);

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
med_err _MEDdatasetNumEcrire (med_idt pere,char *nom, med_type_champ type,
			      med_mode_switch interlace, med_size nbdim, med_size fixdim, 
			      med_size psize, med_mode_profil pflmod, med_int modifpfl, med_size * profil,
			      med_int ngauss, med_size *size,  unsigned char *val);


extern MEDC_EXPORT
med_err _MEDdatasetNumLire(med_idt pere,char *nom,med_type_champ type,
			   med_mode_switch interlace, med_size nbdim, med_size fixdim, 
			   med_size psize, med_mode_profil pflmod, med_int pflcmp, med_size * pfltab,
               med_int ngauss, med_int nbelem, unsigned char *val);

extern MEDC_EXPORT
med_err _MEDdatasetStringEcrire(med_idt pere,char *nom,med_size *dimd,
				char *val);

extern MEDC_EXPORT
med_err _MEDdatasetStringLire(med_idt pere,char *nom,char *val);

/* Gestion des attributs HDF */
extern MEDC_EXPORT
med_idt _MEDattrOuvrir(med_idt pid,char * nom);

extern MEDC_EXPORT
med_err _MEDattrFermer(med_idt id);

extern MEDC_EXPORT
med_err _MEDattrNumEcrire(med_idt pere,med_type_champ type,char *nom,unsigned char *val);

#define _MEDattrEntierEcrire(w,x,y)  _MEDattrNumEcrire(w,MED_INT   ,x,(unsigned char *) y)
#define _MEDattrFloatEcrire(w,x,y)   _MEDattrNumEcrire(w,MED_FLOAT64,x,(unsigned char *) y)

extern MEDC_EXPORT
med_err _MEDattrNumLire(med_idt pere,med_type_champ type,char *nom,unsigned char *val);

#define _MEDattrEntierLire(x,y,z) _MEDattrNumLire(x,MED_INT   ,y,(unsigned char*)z)
#define _MEDattrFloatLire(x,y,z)  _MEDattrNumLire(x,MED_FLOAT64,y,(unsigned char*)z)

extern MEDC_EXPORT
med_err _MEDattrStringEcrire(med_idt pere,char *nom,int longueur,char *val);

extern MEDC_EXPORT
med_err _MEDattrStringLire(med_idt pere,char *nom,int longueur,char *val);


/* Divers */
extern MEDC_EXPORT
med_err _MEDindiceInfo(med_idt id, const char *nom, void *donnees);

extern MEDC_EXPORT
med_err _MEDindiceNum(med_idt id,const char *nom, void *donnees);

extern MEDC_EXPORT  
med_err _MEDobjetIdentifier(med_idt fid,char *chemin,int indice,void *nom);

extern MEDC_EXPORT 
med_err _MEDnObjets(med_idt fid,char *chemin,int *n);

extern MEDC_EXPORT 
void _MEDmodeErreurVerrouiller(void);

extern MEDC_EXPORT
void _MEDobjetsOuverts(med_idt fid);

#ifdef __cplusplus
}
#endif

#endif /* MED_HDFI_H */
