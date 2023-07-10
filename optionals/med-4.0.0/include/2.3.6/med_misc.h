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

#ifndef MED23_MISC_H
#define MED23_MISC_H

#include <med.h>
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

/* type fonction MED pour le versionement des APIs */
typedef void (*MedFuncType)(int,...) ;

extern MEDC_EXPORT 
MedFuncType    _MEDversionedApi(char * name, med_int majeur,
				 med_int mineur, med_int release);

extern MEDC_EXPORT 
med_mode_acces _MEDmodeAcces (med_idt oid);

extern MEDC_EXPORT
med_err _MEDsetModeAcces(med_idt fid, med_mode_acces mode);


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

/* Noms associes aux objets MED */
extern MEDC_EXPORT
med_err _MEDnomEntite(char *nom_ent,med_entite_maillage type_ent);

extern MEDC_EXPORT
med_err _MEDnomGeometrie(char *nom_geo,med_geometrie_element type_geo);

extern MEDC_EXPORT
med_err _MEDnomGeometrie30(char *nom_geo,med_geometrie_element type_geo);

extern MEDC_EXPORT
med_err _MEDparametresGeometrie(med_entite_maillage type_ent, 
			       med_geometrie_element type_geo, int *dim, int *nnoe,
			       int *ndes);
extern MEDC_EXPORT
med_err _MEDnomDataset(char *nom_dataset,med_table quoi,
		       med_connectivite type_conn);

/* Geometrie des objets MED */
extern MEDC_EXPORT 
med_err _MEDGeometrieElement(med_geometrie_element typ_geo[],
			     med_entite_maillage typ_ent);


extern MEDC_EXPORT med_err MEDcheckVersion(med_idt fid);

/* Gestion des messages d'erreur */
#include "med_exit_if.h"

#ifdef __cplusplus
}
#endif

#endif /* MED23_MISC_H */

