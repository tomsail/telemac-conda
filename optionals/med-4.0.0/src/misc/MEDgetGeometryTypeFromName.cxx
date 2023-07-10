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
#include "MEDgetGeometryTypeFromName.hxx"

#include "med_config.h"
#include "med_outils.h"

using namespace std;

static MED_GET_GEOTYPE_FROM_NAME & MEDgetGeotypeFromName=MED_GET_GEOTYPE_FROM_NAME::Instance();

MED_GET_GEOTYPE_FROM_NAME& MED_GET_GEOTYPE_FROM_NAME::Instance() {
  static MED_GET_GEOTYPE_FROM_NAME obj;
  return obj;
}

MED_GET_GEOTYPE_FROM_NAME::~MED_GET_GEOTYPE_FROM_NAME() {}
//
MED_GET_GEOTYPE_FROM_NAME::MED_GET_GEOTYPE_FROM_NAME() : map<string,med_geometry_type>()
{
  map<string,med_geometry_type > &  table  = ( map<string, med_geometry_type > & ) *this ;

  table["PO1"] = MED_POINT1;
  table["SE2"] = MED_SEG2;
  table["SE3"] = MED_SEG3;
  table["SE4"] = MED_SEG4;
  table["TR3"] = MED_TRIA3;
  table["TR6"] = MED_TRIA6;
  table["TR7"] = MED_TRIA7;
  table["QU4"] = MED_QUAD4;
  table["QU8"] = MED_QUAD8;
  table["QU9"] = MED_QUAD9;
  table["TE4"] = MED_TETRA4;
  table["T10"] = MED_TETRA10;
  table["O12"] = MED_OCTA12;
  table["HE8"] = MED_HEXA8;
  table["H20"] = MED_HEXA20;
  table["H27"] = MED_HEXA27;
  table["PE6"] = MED_PENTA6;
  table["P15"] = MED_PENTA15;
  table["P18"] = MED_PENTA18;
  table["PY5"] = MED_PYRA5;
  table["P13"] = MED_PYRA13;
  table["POG"] = MED_POLYGON;
  table["PO2"] = MED_POLYGON2;
  table["POE"] = MED_POLYHEDRON;

}


med_geometry_type MED_GET_GEOTYPE_FROM_NAME::operator[]( const string & c ) const
{
  map<string,med_geometry_type > &table = (map<string, med_geometry_type >&)*this ;

  map<string,med_geometry_type >::iterator it = table.find( c );

  if ( it == table.end() ) return (med_geometry_type) 0;
  return (*it).second;
}


extern "C" {
  med_geometry_type MEDgetGeometryTypeFromName(const char * const keycharpart) {
    return MEDgetGeotypeFromName[string(keycharpart)];
  }
}
