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
#ifndef MED_VERSIONED_API_HXX
#define MED_VERSIONED_API_HXX

#include <string>
#include <map>
#include <cassert>
#include <cstdarg>

#include <med_misc.h>

typedef std::string     keyType;

class MEDC_EXPORT MED_VERSIONED_API : public std::map<keyType,
					  MedFuncType > 
{
public :
  static MED_VERSIONED_API& Instance();
  MedFuncType operator[]( const keyType & c ) const;
  void f77ApiIsSet();

private:
  bool _f77ApiIsSet;
  MED_VERSIONED_API();
  MED_VERSIONED_API(const MED_VERSIONED_API &){};
  //MED_VERSIONED_API& operator =(const MED_VERSIONED_API &){};
  ~MED_VERSIONED_API();
};


extern "C" MedFuncType 
getVersionedApi(const char * const keycharpart,
		const char * const keynumpart);

extern "C" void MEDC_EXPORT f77ApiIsSet(void * obj);

#endif
