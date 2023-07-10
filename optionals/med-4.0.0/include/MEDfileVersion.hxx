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
#ifndef MED_FILE_VERSION_HXX
#define MED_FILE_VERSION_HXX

#include <med.h>

#include <map>

typedef unsigned long     keyType;
typedef med_file_version  valueType;
typedef keyType           fileNo;

class MED_FILE_VERSION : public std::map< keyType, valueType >
{
public:
  static MED_FILE_VERSION& Instance();

private :
  MED_FILE_VERSION(){};
  MED_FILE_VERSION(const MED_FILE_VERSION &){};
  ~MED_FILE_VERSION();

};

// extern "C" valueType  _MEDfileVersion(const med_idt);
// extern "C" med_err    _MEDfileVersionSetCache(const med_idt, const valueType& v);

#endif
