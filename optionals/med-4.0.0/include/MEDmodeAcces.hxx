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
#ifndef MED_MODE_ACCES_HXX
#define MED_MODE_ACCES_HXX

// extern "C" {
#include <med.h>
// }

#include <map>

typedef unsigned long   keyType;
typedef med_access_mode  valueType;

class MED_MODE_ACCES : public std::map< keyType, valueType > 
{
public:
  static MED_MODE_ACCES& Instance();

private :
  MED_MODE_ACCES(){};
  MED_MODE_ACCES(const MED_MODE_ACCES &){};
  ~MED_MODE_ACCES();

};

extern "C" valueType getModeAcces(keyType);
extern "C" med_err   setModeAcces(keyType,valueType);

#endif
