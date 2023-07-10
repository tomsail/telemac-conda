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

#ifndef MED_EXIT_IF_H
#define MED_EXIT_IF_H

#include <medC_win_dll.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Gestion des messages d'erreur */
extern MEDC_EXPORT
void exit_if(const char * const fichier,int ligne, int condition,const char * const message, const char * const arg) ;

#ifdef __cplusplus
}
#endif

#endif /* MED_EXIT_IF_H */

