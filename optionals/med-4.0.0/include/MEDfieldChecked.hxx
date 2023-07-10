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
#ifndef MED_FIELD_CHECKED_HXX
#define MED_FIELD_CHECKED_HXX

#include <med.h>
#include <map>
#include <string>


struct keyType:std::pair< unsigned long, std::string >{};

typedef unsigned long fileNo;
typedef med_bool      valueType;

class MED_FIELD_CHECKED : public std::map< keyType, valueType >
{
public:
  static MED_FIELD_CHECKED& Instance();

private :
  MED_FIELD_CHECKED(){};
  MED_FIELD_CHECKED(const MED_FIELD_CHECKED &){};
  ~MED_FIELD_CHECKED();

};


#endif
