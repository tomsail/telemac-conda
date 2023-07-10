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

#ifndef MED_VERSIONED_PROTO_H
#define MED_VERSIONED_PROTO_H

#include "medC_win_dll.h"
#include <stdarg.h>

/* Interfaces versionnées de l'API MED */

/* Toutes les routines versionnées doivent avoir
   un premier paramètre de type int foo  même s'il
   n'est pas utilisé (cf : MEDOuvrirXYZ(int ,...)*/

/* Il faut ajouter une ligne dans le fichier misc/MEDversionedApi.cxx */

MEDC_EXPORT void MEDchampEcr231(int foo,...);
MEDC_EXPORT void MEDchampEcr232(int foo,...);
MEDC_EXPORT void MEDchampEcr233(int foo,...);

MEDC_EXPORT void MEDjointCr231(int foo,...);
MEDC_EXPORT void MEDjointCr232(int foo,...);

MEDC_EXPORT void MEDfamCr231(int foo,...);
MEDC_EXPORT void MEDfamCr232(int foo,...);

#endif
