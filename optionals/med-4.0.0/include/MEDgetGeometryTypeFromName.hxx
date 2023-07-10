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
#ifndef MED_GET_GEOTYPE_FROM_NAME_HXX
#define MED_GET_GEOTYPE_FROM_NAME_HXX

#include <string>
#include <map>

#include <med.h>

class MEDC_EXPORT MED_GET_GEOTYPE_FROM_NAME : public std::map<std::string,med_geometry_type> 
{
public :
  static MED_GET_GEOTYPE_FROM_NAME& Instance();
  med_geometry_type operator[]( const std::string & c ) const;

private:
  MED_GET_GEOTYPE_FROM_NAME();
  MED_GET_GEOTYPE_FROM_NAME(const MED_GET_GEOTYPE_FROM_NAME &){};
  //MED_GET_GEOTYPE_FROM_NAME& operator =(const MED_GET_GEOTYPE_FROM_NAME &){};
  ~MED_GET_GEOTYPE_FROM_NAME();
};

// Définit dans med_misc.h inclus via med_outils.h
//extern "C" med_geometry_type MEDgetGeometryTypeFromName(const char * const keycharpart);


#endif
