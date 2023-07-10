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
#include "MEDversionedApi3.hxx"
#include "med_config.h"
#include "med_utils.h"
#include <iostream>

// extern "C" {
#include "med_versioned.h"
// }

using namespace std;

// #define STR(chaine) # chaine
// #define XSTR(chaine) STR(chaine)

static MED_VERSIONED_API3 & addFortranSymbol(MED_VERSIONED_API3& table) {

  map<keyType,MedFuncType > &
    _table  = dynamic_cast< map<keyType,
    MedFuncType > & > ( table ) ;

//Ds l'interface C/F les noms de functions résultent
//d'une macro F77_FUNC
//    table[ XSTR(nedffamc231) ]          = nedffamc231 ;
//    table[ XSTR(nedffamc232) ]          = nedffamc232 ;
//   _table[ "nedffamc231" ]              = nedffamc231 ;
//   _table[ "nedffamc232" ]              = nedffamc232 ;
//   _table[ "nedffamc233" ]              = nedffamc232 ;
  return table;
}

extern "C"  { MED_VERSIONED_API3 & MedVersionedApi3F=addFortranSymbol(MED_VERSIONED_API3::Instance()); }
