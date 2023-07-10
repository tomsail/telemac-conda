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

#ifndef MED_VERSIONED_CFI_H
#define MED_VERSIONED_CFI_H

#include <medC_win_dll.h>
#include <stdarg.h>

/* Interfaces versionnées de l'API MED */

/* Toutes les routines versionnées doivent avoir
   un premier paramètre de type int foo  même s'il
   n'est pas utilisé (cf : MEDOuvrirXYZ(int ,...)*/

/* Il faut ajouter une ligne dans le fichier misc/MEDversionedApi.cxx */
#include <med_config.h>
#define nedffamc231 F77_FUNC(edffamc231,EDFFAMC231)
#define nedffamc232 F77_FUNC(edffamc232,EDFFAMC232)

void nedffamc231(int foo,...);
void nedffamc232(int foo,...);


#endif
