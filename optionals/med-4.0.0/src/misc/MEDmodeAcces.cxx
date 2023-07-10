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
#include "MEDmodeAcces.hxx"

using namespace std;

static MED_MODE_ACCES & MedModeAcces=MED_MODE_ACCES::Instance();

MED_MODE_ACCES& MED_MODE_ACCES::Instance() {
  static MED_MODE_ACCES obj;
  return obj;
}


extern "C" valueType getModeAcces(keyType key) {

  map<keyType, valueType >::const_iterator it;

  it = MedModeAcces.find(key);
  
  if (it != MedModeAcces.end() ) return (*it).second;

  return MED_ACC_UNDEF;
}

MED_MODE_ACCES::~MED_MODE_ACCES() {};

extern "C" med_err setModeAcces(keyType key,valueType v) {

//   map<keyType, valueType >::const_iterator it;

//   it = MedModeAcces.find(key);
  
//   if (it == MedModeAcces.end() ) 
//     MedModeAcces[key]=v;
//   else if ( (*it).second != v )
//     return -1;
  MedModeAcces[key]=v;
  return 0;
}
