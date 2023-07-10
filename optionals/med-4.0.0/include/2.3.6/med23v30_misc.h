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

#ifndef MED23v30_MISC_H
#define MED23v30_MISC_H

#include <2.3.6/med23v30.h>
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

extern MEDC_EXPORT
med_err _MEDnomEntite(char *nom_ent,med_entite_maillage type_ent);

extern MEDC_EXPORT
med_err _MEDnomGeometrie(char *nom_geo,med_geometrie_element type_geo);

extern MEDC_EXPORT
med_err _MEDnomGeometrie30(char *nom_geo,med_geometrie_element type_geo);


/* Gestion des messages d'erreur */
extern MEDC_EXPORT
med_err _MED23v30stringConvert(char *chaine30, med_int substrsize30,
			       char *chaine23, med_int substrsize23,
			       med_int nsubstr );
#ifdef __cplusplus
}
#endif

#endif /* MED23V30_MISC_H */

